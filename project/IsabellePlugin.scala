import scala.sys.process.Process

import sbt.Keys._
import sbt._
import sbt.complete.DefaultParsers._

/**
 * Isabelle plugin wrapper. Run the repo Isabelle Instance only using this wrapper.
 */
object IsabellePlugin extends AutoPlugin {
  val USER_HOME = "USER_HOME"

  private def runIsabelle(bin: File, root: File, args: Seq[String], logger: Logger): Unit = {
    val cmd = (bin.getAbsolutePath +: args).mkString(" ")
    val process = Process(cmd, root, USER_HOME -> root.getAbsolutePath).run(logger)
    val resultCode = process.exitValue()
    // Throw exception to trigger build breaks and the such
    if (resultCode != 0) {
      throw new IllegalStateException(s"Running Isabelle command failed with exit code $resultCode")
    }
  }

  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    publish / skip := true,
    scalaSource in Compile := baseDirectory.value / "src", // Register sources for IDE support
    compile / skip := true, // Skip actual compilation (since isabelle scripts will do that)
    Compile / doc := { file("") }, // Skip doc compilation
    run := {
      val isabelleExecutable = baseDirectory.value / "bin" / "isabelle"
      val projectDir = baseDirectory.value / ".."
      val logger = streams.value.log

      // Parse user invocation
      val args = spaceDelimited("<arg>").parsed
      logger.info("Running isabelle " + args.mkString(" "))

      // Run
      runIsabelle(isabelleExecutable, projectDir, List("components", "-a"), logger)
      runIsabelle(isabelleExecutable, projectDir, args, logger)
    }
  )
}
