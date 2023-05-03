/*  Title:      findfacts/build_importer.scala
    Author:     Fabian Huch, TU Munich

Isabelle importer.
 */

package de.tum.in.isabelle.search.importer


import isabelle._

import Theory._

import de.qaware.findfacts.importer.{ImporterModule, TheoryView}
import de.qaware.findfacts.importer.solrimpl.SolrImporterModule
import de.qaware.findfacts.common.solr.{LocalSolr, RemoteSolr, SolrRepository}


object Build_Importer {

  /* import a session to solr */

  def solr_import(
    index_name: String,
    context: Export.Session_Context,
    solr_repository: SolrRepository,
    progress: Progress = new Progress
  ): Unit = {
    val importer = new SolrImporterModule(solr_repository)
    import_session(index_name, context, importer, progress)
  }

  /* import a session with a generic importer */

  def import_session(
    index_name: String,
    context: Export.Session_Context,
    importer: ImporterModule,
    progress: Progress = new Progress,
    verbose: Boolean = false
  ): Unit = {
    val session_name = context.session_name
    val proper_session_theories = context.session_base.proper_session_theories.map(_.theory).toSet
    val theory_names = context.theory_names().filter(proper_session_theories.contains)

    progress.echo("importing " + context.session_name + " with " + theory_names.length + " theories...")

    val theories = theory_names flatMap { theory_name =>
      progress.echo_if(verbose, "loading theory " + theory_name + "...")

      val theory_context = context.theory(theory_name)

      val isabelle_theory = Export_Theory.read_theory(theory_context)
      val markup_xml = theory_context.uncompressed_yxml(Export.MARKUP)
      val markup_blocks = Markup_Blocks.from_XML(markup_xml)

      // Create accessor for importer

      Some(Theory.map_theory(session_name, isabelle_theory, markup_blocks))
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

      val build_options = Word.explode(Isabelle_System.getenv("ISABELLE_BUILD_OPTIONS"))

      var index_name = "theorydata"
      var configset = Isabelle_System.getenv("SOLR_CONFIGSET")
      var local_solr = ""
      var remote_solr: List[String] = Nil
      var verbose = false

      var base_sessions: List[String] = Nil
      var select_dirs: List[Path] = Nil
      var numa_shuffling = false
      var requirements = false
      var soft_build = false
      var exclude_session_groups: List[String] = Nil
      var all_sessions = false
      var clean_build = false
      var dirs: List[Path] = Nil
      var export_files = false
      var fresh_build = false
      var session_groups: List[String] = Nil
      var max_jobs = 1
      var options = Options.init(opts = build_options)
      var exclude_sessions: List[String] = Nil

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
    -B NAME      include session NAME and all descendants
    -D DIR       include session directory and select its sessions
    -N           cyclic shuffling of NUMA CPU nodes (performance tuning)
    -R           refer to requirements of selected sessions
    -S           soft build: only observe changes of sources, not heap images
    -X NAME      exclude sessions from group NAME and all descendants
    -a           select all sessions
    -c           clean build
    -d DIR       include session directory
    -f           fresh build
    -g NAME      select session group NAME
    -j INT       maximum number of parallel jobs (default 1)
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
        "B:" -> (arg => base_sessions = base_sessions ::: List(arg)),
        "D:" -> (arg => select_dirs = select_dirs ::: List(Path.explode(arg))),
        "N" -> (_ => numa_shuffling = true),
        "R" -> (_ => requirements = true),
        "S" -> (_ => soft_build = true),
        "X:" -> (arg => exclude_session_groups = exclude_session_groups ::: List(arg)),
        "a" -> (_ => all_sessions = true),
        "c" -> (_ => clean_build = true),
        "d:" -> (arg => dirs = dirs ::: List(Path.explode(arg))),
        "f" -> (_ => fresh_build = true),
        "g:" -> (arg => session_groups = session_groups ::: List(arg)),
        "j:" -> (arg => max_jobs = Value.Int.parse(arg)),
        "o:" -> (arg => options = options + arg),
        "x:" -> (arg => exclude_sessions = exclude_sessions ::: List(arg))
      )

      options = options + ("export_theory", "true")

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
          base_sessions = base_sessions,
          exclude_session_groups = exclude_session_groups,
          exclude_sessions = exclude_sessions,
          session_groups = session_groups,
          sessions = sessions)

        // Build
        val res = Build.build(
          options,
          selection = selection,
          progress = progress,
          clean_build = clean_build,
          dirs = dirs,
          select_dirs = select_dirs,
          numa_shuffling = numa_shuffling,
          max_jobs = max_jobs,
          fresh_build = fresh_build,
          soft_build = soft_build)

        if (!res.ok) error("Build failed")

        val full_sessions = Sessions.load_structure(options, dirs = dirs, select_dirs = select_dirs)

        val sessions_structure = full_sessions.selection(selection)
        val deps = Sessions.deps(sessions_structure)

        val store = Sessions.store(options)
        val cache = XML.Cache.make()

        // Import
        val session_names = sessions_structure.build_selection(selection).filter(_ != "Pure")
        session_names.map(session_name => Future.fork {
          progress.echo("Importing session " + session_name)
          using(Export.open_session_context(store, deps.base_info(session_name))) { session_context =>
            import_session(index_name, session_context, importer_module, progress, verbose)
          }
        }).foreach(_.join)
      }
    }
  )
}
