package isabelle.jedit_findfacts


import isabelle.*
import de.qaware.findfacts.*
import de.qaware.findfacts.common.solr.*
import de.qaware.findfacts.common.dt.EtField
import de.qaware.findfacts.core.{FacetQuery, QueryService}
import de.qaware.findfacts.core.{FieldFilter, FilterQuery}
import de.qaware.findfacts.core.solrimpl.{SolrFieldFilterMapper, SolrFilterMapper, SolrQueryMapper, SolrQueryService}
import de.qaware.findfacts.importer.solrimpl.SolrImporterModule
import de.tum.in.isabelle.search.importer.Local_Wrapper
import isabelle.{Console_Progress, Document, Export, Export_Theory, Isabelle_System, Long_Name, Session}
import isabelle.jedit.PIDE
import org.slf4j.LoggerFactory


class Findfacts_Variable {
  private val solr_dir = Path.explode("$ISABELLE_HOME_USER/findfacts")
  Isabelle_System.make_directory(solr_dir)

  private val repo: SolrRepository = LocalSolr(solr_dir.absolute_file)

  private implicit val INDEX: String = "local"
  if (!repo.listIndexes.contains(INDEX)) {
    repo.createIndex(INDEX)
  }

  private var _indexed: Option[Document.Version] = None
  def indexed: Option[Document.Version] = _indexed

  val search_service: QueryService = new SolrQueryService(repo,
    new SolrQueryMapper(new SolrFieldFilterMapper(new SolrFilterMapper())))

  private var _indexed_theories: Map[(String, String), SHA1.Shasum] = {
    val query = FilterQuery(List(FieldFilter(EtField.StartLine, core.Term("1"))), pageSize = Int.MaxValue)
    search_service.getResultShortlist(query).get.values.map(v =>
      (v.session, v.theory) -> SHA1.fake_shasum(v.version)).toMap
  }

  private var _index_message: String = "Indexing..."
  def index_message: String = _index_message

  private def handle_update(): Unit = synchronized {
    for {
      snapshot <- PIDE.maybe_snapshot()
      if !snapshot.is_outdated
    } {
      val store = Sessions.store(PIDE.options.value)
      val background = PIDE.resources.session_background
      val base = background.base

      using(Export.open_session_context(store, background, Some(snapshot))) { session_context =>
        // session context contains static + snapshot exports
        val importer = new SolrImporterModule(repo)

        // import static base
        val structure = session_context.sessions_structure

        val nodes_theories = snapshot.version.nodes.domain.toList.map(_.theory)
        val sessions = session_context.session_stack ::: nodes_theories.map(structure.theory_qualifier)

        for {
          session_name <- sessions.distinct.filter(_ != Thy_Header.PURE)
          info <- structure.get(session_name)
        } {
          val proper_theories =
            if (base.session_name == session_name) base.proper_session_theories.map(_.theory)
            else for {
              theory_name <- session_context.theory_names(session_name)
              if structure.theory_qualifier(theory_name) == session_name
            } yield theory_name

          Exn.capture {
            val wrapper = new Local_Wrapper(session_name, info, PIDE.resources)
            for {
              theory_name <- proper_theories
              base_name = Long_Name.base_name(theory_name)
              if !_indexed_theories.get(session_name, base_name).contains(info.meta_info)
            } {
              search_service.deleteBlock(List(
                FieldFilter(EtField.SessionFacet, core.Term(session_name)),
                FieldFilter(EtField.SourceTheoryFacet, core.Term(base_name))))

              val theory = wrapper.map_theory(session_context.theory(theory_name))
              importer.importTheory(INDEX, theory)

              _indexed_theories = _indexed_theories + ((session_name, base_name) -> info.meta_info)
            }
          } match {
            case Exn.Exn(exn) => _index_message = "Error indexing: " + exn.toString
            case Exn.Res(_) =>
              _indexed = Some(snapshot.version)
              _index_message = "Indexed " + _indexed_theories.size + " theories"
          }
          GUI_Thread.later(post_update())
        }

        // TODO import theories in draft session / sessions without heap images
      }
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
