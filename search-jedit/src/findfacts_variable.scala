package isabelle.jedit_findfacts

import de.qaware.findfacts._
import de.qaware.findfacts.importer.Importer
import de.qaware.findfacts.common.solr._
import de.qaware.findfacts.core.{FacetQuery, QueryService}
import de.qaware.findfacts.core.solrimpl.{SolrFieldFilterMapper, SolrFilterMapper, SolrQueryMapper, SolrQueryService}
import isabelle.{Console_Progress, Document, Export, Export_Theory, Isabelle_System, Long_Name, Session}
import isabelle.jedit.PIDE
import org.slf4j.LoggerFactory


class Findfacts_Variable
{
  private val solr_dir = Isabelle_System.tmp_dir("findfacts")
  private val repo: SolrRepository = LocalSolr(solr_dir)

  private var _state: Option[(Document.Snapshot, String)] = None
  def state: Option[(Document.Snapshot, String)] = _state

  val search_service: QueryService = new SolrQueryService(repo, new SolrQueryMapper(new SolrFieldFilterMapper(new SolrFilterMapper())))

  val logger = new Console_Progress()

  private val main =
    Session.Consumer[Any](getClass.getName) {
      case _ =>
        for {
          snapshot <- PIDE.maybe_snapshot()
          theory_name = snapshot.node_name.theory
          if (!snapshot.is_outdated)
          if (snapshot.version.nodes.theory_name(theory_name).isDefined)
        } {
          logger.echo("Importing " + theory_name)
          val index_name = snapshot.version.nodes.topological_order.mkString("_") + "_" + snapshot.version.id
          if (!repo.listIndexes.contains(index_name)) {
            logger.echo("Creating index " + index_name)
            repo.createIndex(index_name)
          }

          val base = PIDE.resources.session_base_info
          using(Export.Session_Context.open_session(base, Some(snapshot))) { session_context =>
            logger.echo("Importing..")
            Build_Importer.solr_import(index_name, session_context, repo)
          }
          // TODO delete old index
          logger.echo("Finished importing")
          _state = Some((snapshot, "Updated to revision " + snapshot.version))

          PIDE.session.caret_focus.post(Session.Caret_Focus)
        }
    }

  def install_handlers(): Unit =
  {
    PIDE.session.global_options += main
    PIDE.session.commands_changed += main
  }

  def uninstall_handlers(): Unit =
  {
    PIDE.session.global_options -= main
    PIDE.session.commands_changed -= main
  }
}
