/*  Title:      findfacts/build_importer.scala
    Author:     Fabian Huch, TU Munich/QAware GmbH

Isabelle dump importer.
 */

package de.tum.in.isabelle.search.importer

import de.qaware.findfacts.common.solr.{LocalSolr, RemoteSolr}
import de.qaware.findfacts.importer.Importer
import de.qaware.findfacts.importer.solrimpl.SolrImporterModule
import isabelle.Build.build
import isabelle.Export.Provider
import isabelle._

object Build_Importer {

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
      var options = Options.init(opts = build_options) + "export_theory"
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
        val res = build(
          options,
          selection = selection,
          progress = progress,
          clean_build = clean_build,
          dirs = dirs,
          select_dirs = select_dirs,
          numa_shuffling = numa_shuffling,
          max_jobs = max_jobs,
          fresh_build = fresh_build,
          soft_build = soft_build,
          export_files = true
        )

        if (!res.ok) error("Build failed")

        val selected_sessions = Sessions.load_structure(options, dirs = dirs, select_dirs = select_dirs)
          .imports_selection(selection)

        // Import
        selected_sessions foreach { session_name =>
          val store = Sessions.store(options)

          using(store.open_database(session_name)) { db =>
            val provider = Provider.database(db, XML.Cache.make(), session_name, "dummy")
            val theories = store.read_theories(db, session_name)
            Importer.import_session(index_name, provider, session_name, theories, importer_module, progress, verbose)
          }
        }
      }
    }
  )
}
