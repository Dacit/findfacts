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

  val search_service: QueryService = new SolrQueryService(repo, new SolrQueryMapper(new SolrFieldFilterMapper(new SolrFilterMapper())))
  private var _indexed_sessions: Map[String, SHA1.Digest] = {
    val query = FilterQuery(List(FieldFilter(EtField.StartLine, core.Term("1"))))
    search_service.getResultShortlist(query).get.values.map(v => v.session -> SHA1.fake_digest(v.version)).toMap
  }

  val progress = new Console_Progress()

  private def handle_update(): Unit = synchronized {
    for {
      snapshot <- PIDE.maybe_snapshot()
      if !snapshot.is_outdated
    } {
      val store = Sessions.store(PIDE.options.value)
      val base = PIDE.resources.session_base_info

      using(Export.open_session_context(store, base, Some(snapshot))) { session_context =>
        // session context contains static + snapshot exports
        val importer = new SolrImporterModule(repo)

        // import static base
        val structure = session_context.sessions_structure

        val nodes_theories = snapshot.version.nodes.domain.toList.map(_.theory)
        val sessions = session_context.session_stack ::: nodes_theories.map(structure.theory_qualifier)

        for {
          session_name <- sessions.distinct.filter(_ != Thy_Header.PURE)
          info <- structure.get(session_name)
          if !_indexed_sessions.get(session_name).contains(info.meta_digest)
        } {
          progress.echo("Importing session " + quote(session_name))

          Exn.capture {
            val proper_theories =
              if (base.base.session_name == session_name) base.base.proper_session_theories.map(_.theory)
              else for {
                theory_name <- session_context.theory_names(session_name)
                if structure.theory_qualifier(theory_name) == session_name
              } yield theory_name

            val wrapper = new Local_Wrapper(session_name, info, PIDE.resources)
            val theories = proper_theories.map(session_context.theory(_)).map(wrapper.map_theory)

            // TODO delete session first
            importer.importSession(INDEX, theories)

            _indexed_sessions += session_name -> info.meta_digest
            progress.echo("Finished importing session " + quote(session_name))
          } match {
            case Exn.Exn(exn) => progress.echo("Error indexing: " + exn.toString)
            case Exn.Res(_) => _indexed = Some(snapshot.version)
          }
        }

        // TODO import theories in draft session
      }
    }

    GUI_Thread.later(post_update())
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
