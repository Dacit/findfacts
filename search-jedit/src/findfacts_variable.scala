package isabelle.jedit_findfacts


import isabelle.*
import de.qaware.findfacts.*
import de.qaware.findfacts.common.solr.*
import de.qaware.findfacts.common.dt.EtField
import de.qaware.findfacts.core.dt.ShortBlock
import de.qaware.findfacts.core.{Exact, FacetQuery, FieldFilter, FilterQuery, QueryService}
import de.qaware.findfacts.core.solrimpl.{SolrFieldFilterMapper, SolrFilterMapper, SolrQueryMapper, SolrQueryService}
import de.qaware.findfacts.importer.solrimpl.SolrImporterModule
import de.tum.in.isabelle.search.importer.Local_Wrapper
import isabelle.{Console_Progress, Document, Export, Export_Theory, Isabelle_System, Long_Name, Session}
import isabelle.jedit.PIDE
import org.slf4j.LoggerFactory

object Findfacts_Variable {
  sealed trait Status
  case object Init extends Status
  case object Indexing extends Status
  case class Error(exn: Throwable) extends Status
  case class Ready(num_theories: Int, no_exports: List[String], indexed: Document.Version) extends Status
}

class Findfacts_Variable {
  import Findfacts_Variable.*

  val DRAFT_SHASUM = SHA1.fake_shasum("Draft")
  val NO_EXPORT = SHA1.fake_shasum("No_Export")
  def not_exported(version: SHA1.Shasum): Boolean = version == NO_EXPORT

  private val solr_dir = Path.explode("$ISABELLE_HOME_USER/findfacts")
  Isabelle_System.make_directory(solr_dir)

  private val repo: SolrRepository = LocalSolr(solr_dir.absolute_file)
  private val importer = new SolrImporterModule(repo)

  private implicit val INDEX: String = "local"
  if (!repo.listIndexes.contains(INDEX)) {
    repo.createIndex(INDEX)
  }

  val search_service: QueryService = new SolrQueryService(repo,
    new SolrQueryMapper(new SolrFieldFilterMapper(new SolrFilterMapper())))

  private var _indexed_theories: Map[(String, String), SHA1.Shasum] = {
    val query = FilterQuery(List(FieldFilter(EtField.StartLine, core.Term("1"))), pageSize = Int.MaxValue)
    val res = search_service.getResultShortlist(query).get

    def is_deleted(b: ShortBlock): Boolean = b.version == DRAFT_SHASUM.toString && !Path.explode(b.file).file.exists()

    val (present, deleted) = res.values.partition(is_deleted)
    deleted.toList.map(b => Exact(b.file)) match {
      case Nil =>
      case elem :: Nil => search_service.deleteBlock(List(FieldFilter(EtField.SourceFile, elem)))
      case elem1 :: elem2 :: elems =>
        search_service.deleteBlock(List(FieldFilter(EtField.SourceFile, core.Or(elem1, elem2, elems: _*))))
    }

    present.map(block => (block.session, block.theory) -> SHA1.fake_shasum(block.version)).toMap
  }

  private var _status: Status = Init
  def status: Status = _status
  def indexed: Option[Document.Version] = _status match {
    case Ready(_, _, indexed) => Some(indexed)
    case _ => None
  }


  /* import */

  def do_import(
    session_name: String,
    shasum: SHA1.Shasum,
    node_name: Document.Node.Name,
    theory_context: Export.Theory_Context,
    xml: XML.Body
  ): Unit = {
    val wrapper = new Local_Wrapper(session_name, shasum)
    val theory = wrapper.map_theory(node_name, Export_Theory.read_theory(theory_context), xml)
    importer.importTheory(INDEX, theory)
  }

  def import_draft_theory(
     node_name: Document.Node.Name,
     session_name: String,
     session_context: Export.Session_Context,
     snapshot: Document.Snapshot,
   ): Unit = {
    val base_name = Long_Name.base_name(node_name.theory)
    val do_update =
      _indexed_theories.get(session_name, base_name) match {
        case Some(DRAFT_SHASUM) => snapshot.state.node_initialized(snapshot.version, node_name)
        case Some(_) => false
        case None => true
      }

    if (do_update) {
      search_service.deleteBlock(List(
        FieldFilter(EtField.SessionFacet, Exact(session_name)),
        FieldFilter(EtField.SourceTheoryFacet, Exact(base_name)),
        FieldFilter(EtField.Version, Exact(DRAFT_SHASUM.toString))))

      val xml = snapshot.state.xml_markup(snapshot.version, node_name)
      val theory_context = session_context.theory(node_name.theory)
      do_import(session_name, DRAFT_SHASUM, node_name, theory_context, xml)

      _indexed_theories = _indexed_theories + ((session_name, base_name) -> DRAFT_SHASUM)
    }
  }

  def try_import_session(
    background: Sessions.Background,
    session_name: String,
    info: Sessions.Info,
    session_context: Export.Session_Context,
  ): Unit = {
    val proper_theories =
      if (background.base.session_name == session_name) background.base.proper_session_theories.map(_.theory)
      else for {
        theory_name <- session_context.theory_names(session_name)
        if session_context.sessions_structure.theory_qualifier(theory_name) == session_name
      } yield theory_name

    def is_imported(theory_name: String): Boolean = {
      val res = _indexed_theories.get(session_name, Long_Name.base_name(theory_name))
      res.contains(info.meta_info) || res.contains(NO_EXPORT)
    }

    if (!proper_theories.forall(is_imported)) {
      val imported =
        for {
          theory_name <- proper_theories
          node_name <- PIDE.resources.find_theory_node(theory_name)
        } yield {
          val theory_context = session_context.theory(theory_name)
          val xml = theory_context.yxml(Export.MARKUP)
          if (xml.isEmpty) false
          else {
            do_import(session_name, info.meta_info, node_name, theory_context, xml)
            true
          }
        }

      val success = imported.forall(identity)
      _indexed_theories = _indexed_theories ++ proper_theories.map(theory_name =>
        (session_name, Long_Name.base_name(theory_name)) -> (if (success) info.meta_info else NO_EXPORT))
    }
  }


  /* updates */

  private def handle_update(): Unit = synchronized {
    for {
      snapshot <- PIDE.maybe_snapshot()
      if !snapshot.is_outdated
    } {
      val store = Store(PIDE.options.value)
      val background = PIDE.resources.session_background

      using(Export.open_session_context(store, background, Some(snapshot))) { session_context =>
        val structure = session_context.sessions_structure
        Exn.capture {
          // import draft
          val draft_thys = session_context.theory_names(Sessions.DRAFT)
          for {
            node_name <- snapshot.version.nodes.domain.toList
            theory_name = node_name.theory
            if draft_thys.contains(theory_name)
            session_name = structure.theory_qualifier(theory_name)
          } import_draft_theory(node_name, session_name, session_context, snapshot)

          // import sessions from db
          for {
            session_name <- session_context.session_stack
            if session_name != Thy_Header.PURE
            info <- structure.get(session_name)
          } try_import_session(background, session_name, info, session_context)
        } match {
          case Exn.Exn(exn) => _status = Error(exn)
          case Exn.Res(_) =>
            val no_exports = _indexed_theories.filter(_._2 == NO_EXPORT).keys.map(_._1).toList.distinct
            _status = Ready(_indexed_theories.values.filterNot(not_exported).size, no_exports, snapshot.version)
        }
      }
      GUI_Thread.later(post_update())
    }
  }

  private def post_update(): Unit = {
    GUI_Thread.require {}
    PIDE.session.caret_focus.post(Session.Caret_Focus)
  }

  private val main =
    Session.Consumer[Any](getClass.getName)(_ => Future.thread("findfacts-importer")(handle_update()))

  def install_handlers(): Unit = {
    PIDE.session.global_options += main
    PIDE.session.commands_changed += main
    PIDE.session.finished_theories += main
  }

  def uninstall_handlers(): Unit = {
    PIDE.session.global_options -= main
    PIDE.session.commands_changed -= main
    PIDE.session.finished_theories -= main
  }
}
