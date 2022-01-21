import sbt.Keys._
import sbt._
import sbt.complete.DefaultParsers._
import sbtassembly.AssemblyKeys.assembly
import sbtassembly.AssemblyPlugin

/** Plugin for sbt projects defining a tool as Isabelle component. */
object IsabelleToolPlugin extends AutoPlugin {
  override def requires: Plugins = AssemblyPlugin

  object autoImport {
    lazy val isabelleProject = settingKey[Project]("isabelle project")
    lazy val isabelleCommand =
      settingKey[String]("isabelle tool command")
  }

  import autoImport._

  override def projectSettings: Seq[Def.Setting[_]] =
    Seq(
      compile / skip := true, // Skip actual compilation (since isabelle scripts will do that)
      Compile / doc := { file("") }, // Skip doc compilation
      run := {
        // Execute isabelle run config for custom tool
        Def.inputTaskDyn {
          // build jar assembly
          assembly.value
          // Parse tool args
          val args = spaceDelimited("<arg>").parsed
          // Run
          (run in isabelleProject.value).toTask(" " + (isabelleCommand.value +: args).mkString(" "))
        }.evaluated
      }
    )
}
