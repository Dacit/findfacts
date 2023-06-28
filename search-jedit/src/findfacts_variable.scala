/* Title:      jedit_findfacts/findfacts_variable.scala
   Author:     Fabian Huch, TU Munich

Findfacts plugin variable with global index state.
 */
package isabelle.jedit_findfacts


import isabelle.*
import isabelle.{Console_Progress, Document, Export, Export_Theory, Isabelle_System, Long_Name, Session}
import isabelle.jedit.PIDE

import scala.util.Try

import de.qaware.findfacts.*
import de.qaware.findfacts.common.solr.*
import de.qaware.findfacts.common.dt.EtField
import de.qaware.findfacts.core.QueryService.ResultList
import de.qaware.findfacts.core.dt.ShortBlock
import de.qaware.findfacts.core.{Exact, FacetQuery, FieldFilter, Filter, FilterQuery, QueryService}
import de.qaware.findfacts.core.solrimpl.{SolrFieldFilterMapper, SolrFilterMapper, SolrQueryMapper, SolrQueryService}
import de.qaware.findfacts.importer.ImporterModule
import de.qaware.findfacts.importer.solrimpl.SolrImporterModule
import de.tum.in.isabelle.search.importer.Local_Wrapper

import org.slf4j.LoggerFactory


object Findfacts_Variable {
  sealed trait Status
  case object Init extends Status
  case object Indexing extends Status
  case class Error(exn: Throwable) extends Status
  case class Ready(num_theories: Int, indexed: Document.Version) extends Status

  class Context(
    importer: ImporterModule,
    service: QueryService,
    private var _indexed_theories: Map[Context.Node, SHA1.Shasum]
  ) {
    import isabelle.jedit_findfacts.Findfacts_Variable.Context.*

    private def do_import(
      session_name: String,
      shasum: SHA1.Shasum,
      node_name: Document.Node.Name,
      theory_context: Export.Theory_Context,
      xml: XML.Body
    ): Unit = {
      val wrapper = new Local_Wrapper(session_name, shasum)
      val theory = wrapper.map_theory(node_name, Export_Theory.read_theory(theory_context), xml)
      importer.importTheory(Context.INDEX, theory)
    }

    def import_draft_theory(
       node_name: Document.Node.Name,
       session_name: String,
       session_context: Export.Session_Context,
       snapshot: Document.Snapshot,
     ): Unit = {
      val node = Node(session_name, node_name)
      val do_update =
        _indexed_theories.get(node) match {
          case Some(DRAFT_SHASUM) => snapshot.state.node_initialized(snapshot.version, node_name)
          case Some(_) => false
          case None => true
        }

      if (do_update) {
        service.deleteBlock(List(
          FieldFilter(EtField.SessionFacet, Exact(session_name)),
          FieldFilter(EtField.SourceTheoryFacet, Exact(node.theory_base_name)),
          FieldFilter(EtField.Version, Exact(DRAFT_SHASUM.toString))))

        val xml = snapshot.state.xml_markup(snapshot.version, node_name)
        val theory_context = session_context.theory(node_name.theory)
        do_import(session_name, DRAFT_SHASUM, node_name, theory_context, xml)

        _indexed_theories = _indexed_theories + (node -> DRAFT_SHASUM)
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

      val node_names =
        proper_theories.flatMap(theory_name => PIDE.resources.find_theory_node(theory_name).map(theory_name -> _)).toMap
      val nodes = node_names.values.toList.map(Node(session_name, _))

      val (indexed, missing) = nodes.partition(_indexed_theories.contains)
      val (draft, regular) = indexed.partition(_indexed_theories.get(_).contains(DRAFT_SHASUM))
      val old_version = regular.filterNot(_indexed_theories.get(_).contains(info.meta_info))

      def theory_filter(node: Node): Filter = Exact(node.theory_base_name)
      def theories_filter(nodes: List[Node]): Option[Filter] = nodes match{
        case Nil => None
        case node :: Nil => Some(theory_filter(node))
        case node1 :: node2 :: nodes =>
          Some(core.Or(theory_filter(node1), theory_filter(node2), nodes.map(theory_filter): _*))
      }

      theories_filter(draft).foreach(filter => service.deleteBlock(
        List(
          FieldFilter(EtField.SessionFacet, Exact(session_name)),
          FieldFilter(EtField.SourceTheoryFacet, filter),
          FieldFilter(EtField.Version, Exact(DRAFT_SHASUM.toString)))))

      val import_nodes =
        if (old_version.isEmpty) missing ::: draft
        else {
          service.deleteBlock(List(FieldFilter(EtField.Version, Exact(info.meta_info.toString))))
          nodes
        }

      for {
        node <- import_nodes
        theory_name = PIDE.resources.theory_name(session_name, node.theory_base_name)
        node_name <- node_names.get(theory_name)
      } {
        val theory_context = session_context.theory(theory_name)
        val xml = theory_context.yxml(Export.MARKUP)
        do_import(session_name, info.meta_info, node_name, theory_context, xml)
      }

      _indexed_theories = _indexed_theories ++ nodes.map(_ -> info.meta_info)
    }

    def search(query: FilterQuery): Try[ResultList[ShortBlock]] = service.getResultShortlist(query)(INDEX)
    def num_indexed: Int = _indexed_theories.size
    def debug_info: String =
      _indexed_theories.map {
        case (k,v) => k.toString + ": " + v.digest.toString
      }.mkString("\n")
  }

  object Context {
    private val DRAFT_SHASUM = SHA1.fake_shasum("Draft")
    private implicit val INDEX: String = "local"

    case class Node(session_name: String, theory_base_name: String, file: Path) {
      override def toString: String = session_name + "." + theory_base_name + " ("+ file.implode + ")"
    }
    object Node {
      def apply(session_name: String, node_name: Document.Node.Name): Node =
        new Node(session_name, node_name.theory_base_name, node_name.path)
      def apply(block: ShortBlock): Node = new Node(block.session, block.theory, Path.explode(block.file))
    }

    def is_draft(block: ShortBlock): Boolean = block.version == DRAFT_SHASUM.toString

    def load(dir: Path): Context = {
      // setup index
      Isabelle_System.make_directory(dir)
      val repo = LocalSolr(dir.absolute_file)
      if (!repo.listIndexes.contains(INDEX)) {
        repo.createIndex(INDEX)
      }

      val importer = new SolrImporterModule(repo)
      val service = new SolrQueryService(repo, new SolrQueryMapper(new SolrFieldFilterMapper(new SolrFilterMapper())))

      val query = FilterQuery(List(FieldFilter(EtField.StartLine, core.Term("1"))), pageSize = Int.MaxValue)
      def is_missing(b: ShortBlock): Boolean = is_draft(b) && !Path.explode(b.file).file.exists()

      val (missing, present) = service.getResultShortlist(query).get.values.partition(is_missing)
      missing.toList.map(b => Exact(b.file)) match {
        case Nil =>
        case elem :: Nil => service.deleteBlock(List(FieldFilter(EtField.SourceFile, elem)))
        case elem1 :: elem2 :: elems =>
          service.deleteBlock(List(FieldFilter(EtField.SourceFile, core.Or(elem1, elem2, elems: _*))))
      }

      val indexed = present.map(block => Node(block) -> SHA1.fake_shasum(block.version)).toMap
      new Context(importer, service, indexed)
    }
  }
}

class Findfacts_Variable {
  import Findfacts_Variable.*

  val context = Context.load(Path.explode("$ISABELLE_HOME_USER/findfacts"))

  private var _status: Status = Init
  def status: Status = _status
  def indexed: Option[Document.Version] = _status match {
    case Ready(_, indexed) => Some(indexed)
    case _ => None
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
          } context.import_draft_theory(node_name, session_name, session_context, snapshot)

          // import sessions from db
          for {
            session_name <- session_context.session_stack
            if session_name != Thy_Header.PURE
            info <- structure.get(session_name)
          } context.try_import_session(background, session_name, info, session_context)
        } match {
          case Exn.Exn(exn) => _status = Error(exn)
          case Exn.Res(_) => _status = Ready(context.num_indexed, snapshot.version)
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
