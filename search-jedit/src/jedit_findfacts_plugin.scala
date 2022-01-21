package isabelle.jedit_findfacts

import isabelle.Path
import isabelle.Scala_Project


class JEdit_Findfacts_Plugin extends
  Scala_Project.Plugin(Path.explode("$FINDFACTS_HOME/jedit_findfacts_plugin"))
