package isabelle.jedit_findfacts


import isabelle._

import de.qaware.findfacts._
import de.qaware.findfacts.common.solr._
import de.qaware.findfacts.core.{FacetQuery, QueryService}
import de.qaware.findfacts.core.solrimpl.{SolrFieldFilterMapper, SolrFilterMapper, SolrQueryMapper, SolrQueryService}
import isabelle.{Console_Progress, Document, Export, Export_Theory, Isabelle_System, Long_Name, Session}
import isabelle.jedit.PIDE
import org.slf4j.LoggerFactory


class Findfacts_Variable {
  private val solr_dir = Isabelle_System.tmp_dir("findfacts")
  private val repo: SolrRepository = LocalSolr(solr_dir)

  private var _state: Option[Document.Version] = None
  def state: Option[Document.Version] = _state

  val search_service: QueryService = new SolrQueryService(repo, new SolrQueryMapper(new SolrFieldFilterMapper(new SolrFilterMapper())))

  val progress = new Console_Progress()

  private val main =
    Session.Consumer[Any](getClass.getName) {
      case _ =>
        for {
          snapshot <- PIDE.maybe_snapshot()
          theory_name = snapshot.node_name.theory
          if !snapshot.is_outdated
          if snapshot.version.nodes.theory_name(theory_name).isDefined
        } {
          progress.echo("Importing " + theory_name)
          val index_name = "Local"
          if (!repo.listIndexes.contains(index_name)) {
            progress.echo("Creating index " + index_name)
            repo.createIndex(index_name)
          }

          val store = Sessions.store(PIDE.options.value)
          val base = PIDE.resources.session_base_info

          using(Export.open_session_context(store, base, Some(snapshot))) { session_context =>
            // TODO proper import
          }

          _state = Some(snapshot.version)

          PIDE.session.caret_focus.post(Session.Caret_Focus)
        }
    }

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
