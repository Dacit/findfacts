/*  Title:      findfacts/build_importer.scala
    Author:     Fabian Huch, TU Munich

Isabelle importer.
 */

package de.tum.in.isabelle.search.importer


import isabelle._

import Theory_Wrapper._

import de.qaware.findfacts.importer.{ImporterModule, TheoryView}
import de.qaware.findfacts.importer.solrimpl.SolrImporterModule
import de.qaware.findfacts.common.solr.{LocalSolr, RemoteSolr, SolrRepository}

import scala.collection.mutable


object Build_Importer {
  /* import a session with a generic importer */

  def import_session(
    session_name: String,
    link_base: String,
    index_name: String,
    store: Store,
    deps: Sessions.Deps,
    importer: ImporterModule,
    progress: Progress = new Progress,
    verbose: Boolean = false
  ): Unit = using(Export.open_session_context(store, deps.background(session_name))) { context =>
    val proper_session_theories = context.session_base.proper_session_theories.map(_.theory).toSet
    val theory_names = context.theory_names().filter(proper_session_theories.contains)
    progress.echo("importing " + context.session_name + " with " + theory_names.length + " theories...")

    val document_info = Document_Info.read(context.database_context, deps, List(session_name))
    val wrapper = new HTML_Wrapper(session_name, link_base, context.sessions_structure,  document_info)

    val theories = theory_names.map { theory_name =>
      val node_name = Document.Node.Name.loaded_theory(theory_name)
      val theory_context = context.theory(theory_name)
      val isabelle_theory = Export_Theory.read_theory(theory_context)
      val xml_markup = theory_context.yxml(Export.MARKUP)
      wrapper.map_theory(node_name, isabelle_theory, xml_markup)
    }

    progress.echo_if(verbose, "finished loading theories, importing...")
    val errors = importer.importSession(index_name, theories)

    errors foreach { error =>
      val message = session_name + ": " + error.step.getClass + ": " + error.causeEntity + ": " + error.errorMsg
      progress.echo_error_message(message)
    }

    if (errors.isEmpty) {
      progress.echo("finished importing " + session_name)
    } else {
      progress.echo("finished importing " + session_name + " with " + errors.size + " errors.")
    }
  }

  /* Isabelle tool wrapper */

  val isabelle_tool = Isabelle_Tool(
    "build_importer",
    "Import build db into solr",
    Scala_Project.here,
    args => {
      /* arguments */

      var index_name = "theorydata"
      var configset = Isabelle_System.getenv("SOLR_CONFIGSET")
      var local_solr = ""
      var remote_solr: List[String] = Nil
      var verbose = false

      var afp_root: Option[Path] = None
      val base_sessions = new mutable.ListBuffer[String]
      val select_dirs = new mutable.ListBuffer[Path]
      val build_hosts = new mutable.ListBuffer[Build_Cluster.Host]
      var numa_shuffling = false
      var requirements = false
      var soft_build = false
      val exclude_session_groups = new mutable.ListBuffer[String]
      var all_sessions = false
      var clean_build = false
      val dirs = new mutable.ListBuffer[Path]
      var fresh_build = false
      val session_groups = new mutable.ListBuffer[String]
      var max_jobs: Option[Int] = None
      var options = Options.init(specs = Options.Spec.ISABELLE_BUILD_OPTIONS)
      val exclude_sessions = new mutable.ListBuffer[String]

      val getopts = Getopts(
        """
Usage: isabelle build_importer [OPTIONS] SESSIONS...

  Importer options are:
    -i NAME         index NAME to import into
    -C NAME         Solr configset NAME
    -l SOLRDIR      local Solr repository at SOLRDIR
    -r HOST:PORT    remote Solr connection at HOST:PORT
    -v              verbose

  Build options are:
    -A ROOT      include AFP with given root directory (":" for """ + AFP.BASE.implode + """)
    -B NAME      include session NAME and all descendants
    -D DIR       include session directory and select its sessions
    -H HOSTS     additional cluster host specifications of the form
                 NAMES:PARAMETERS (separated by commas)
    -N           cyclic shuffling of NUMA CPU nodes (performance tuning)
    -R           refer to requirements of selected sessions
    -S           soft build: only observe changes of sources, not heap images
    -X NAME      exclude sessions from group NAME and all descendants
    -a           select all sessions
    -c           clean build
    -d DIR       include session directory
    -f           fresh build
    -g NAME      select session group NAME
    -j INT       maximum number of parallel jobs
                 (default: 1 for local build, 0 for build cluster)
    -o OPTION    override Isabelle system OPTION (via NAME=VAL or NAME)
    -x NAME      exclude session NAME and all descendants

  Import Isabelle dump from DUMPDIR into Solr db. Only one Solr connection
  may be used. For remote connections, a configset must be set (either via
  argument or environment variable 'SOLR_CONFIGSET').
  Index name usually has form '${NAME}_${ISABELLE_VERSION}_${AFP_VERSION}'.
""",
        "i:" -> (arg => index_name = arg),
        "C:" -> (arg => configset = arg),
        "l:" -> (arg => local_solr = arg),
        "r:" -> (arg => remote_solr = Library.distinct(space_explode(':', arg))),
        "v" -> (_ => verbose = true),
        "A:" -> (arg => afp_root = Some(if (arg == ":") AFP.BASE else Path.explode(arg))),
        "B:" -> (arg => base_sessions += arg),
        "D:" -> (arg => select_dirs += Path.explode(arg)),
        "H:" -> (arg => build_hosts ++= Build_Cluster.Host.parse(Registry.global, arg)),
        "N" -> (_ => numa_shuffling = true),
        "R" -> (_ => requirements = true),
        "S" -> (_ => soft_build = true),
        "X:" -> (arg => exclude_session_groups += arg),
        "a" -> (_ => all_sessions = true),
        "c" -> (_ => clean_build = true),
        "d:" -> (arg => dirs += Path.explode(arg)),
        "f" -> (_ => fresh_build = true),
        "g:" -> (arg => session_groups += arg),
        "j:" -> (arg => max_jobs = Some(Value.Nat.parse(arg))),
        "o:" -> (arg => options = options + arg),
        "x:" -> (arg => exclude_sessions += arg))

      options = options + "export_theory"
      val afp_link = "https://www.isa-afp.org/browser_info"

      val sessions = getopts(args)

      val solr_repository = (local_solr, remote_solr) match {
        case (dir, _) if !dir.isBlank => LocalSolr(Path.explode(dir).absolute_file)
        case (_, host :: port :: Nil) if !host.isBlank && !configset.isBlank =>
          RemoteSolr(host, Value.Int.parse(port), configset)
        case _ => getopts.usage()
      }

      using(solr_repository) { solr_repository =>
        val importer_module = new SolrImporterModule(solr_repository)

        val progress = new Console_Progress()

        val selection = Sessions.Selection(
          requirements = requirements,
          all_sessions = all_sessions,
          base_sessions = base_sessions.toList,
          exclude_session_groups = exclude_session_groups.toList,
          exclude_sessions = exclude_sessions.toList,
          session_groups = session_groups.toList,
          sessions = sessions.toList)

        // Build
        val res = Build.build(
          options,
          selection = selection,
          progress = progress,
          clean_build = clean_build,
          afp_root = afp_root,
          dirs = dirs.toList,
          select_dirs = select_dirs.toList,
          numa_shuffling = Host.numa_check(progress, numa_shuffling),
          max_jobs = max_jobs,
          fresh_build = fresh_build,
          soft_build = soft_build,
          build_hosts = build_hosts.toList)

        if (!res.ok) error("Build failed")

        val full_sessions = Sessions.load_structure(options, dirs = dirs.toList, select_dirs = select_dirs.toList)

        val sessions_structure = full_sessions.selection(selection)
        val deps = Sessions.deps(sessions_structure)

        val store = Store(options)

        // Import
        val session_names = sessions_structure.build_topological_order.filter(_ != Thy_Header.PURE)
        session_names.map(session_name => Future.fork {
          progress.echo("Importing session " + session_name)
          import_session(session_name, afp_link, index_name, store, deps, importer_module, progress, verbose)
        }).foreach(_.join)
      }
    }
  )
}
