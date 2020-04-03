package de.qaware.findfacts.it

import better.files.{File, Resource}
import de.qaware.findfacts.common.dt.CodeblockEt
import de.qaware.findfacts.common.solr.LocalSolr
import de.qaware.findfacts.common.solr.mapper.FromSolrDoc
import de.qaware.findfacts.it
import de.qaware.findfacts.scala.Using
import fastparse.Parsed
import io.github.classgraph.ClassGraph
import org.apache.solr.client.solrj.SolrQuery
import org.scalatest.{FunSuite, Matchers, Suite}

import scala.collection.JavaConverters._
import scala.collection.immutable.IndexedSeq
import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror, universe}
import scala.tools.reflect.ToolBox

/** Context for spec tests.
  *
  * @param name of the spec test
  * @param block created from Isabelle import
  * @param src Isabelle theory source code
  * @param startLine at which Isabelle theory source code starts
  */
case class SpecTestContext(name: String, block: CodeblockEt, src: String, startLine: Int)

/** Parent test suite, that builds a nested suite for each Spec-theory. */
class SpecTestExecutor extends FunSuite with Matchers {
  private final val Session = "IAS-Example"

  test("Solr core present") {
    Using.resource(LocalSolr(File(Resource.getUrl("solrdir/")).toJava)) { solr =>
      val indexes = solr.listIndexes
      indexes should have size 1
    }
  }

  override lazy val nestedSuites: IndexedSeq[Suite] = {
    // Discover theory spec files
    val thyFiles = Using.resource(new ClassGraph().whitelistPathsNonRecursive("").scan) { scan =>
      scan.getResourcesWithExtension(".thy").getPaths.asScala.toIndexedSeq
    }

    // Setup JIT compiler
    val toolbox = currentMirror.mkToolBox()

    // Get imported entities
    val query = new SolrQuery("{!parent which=kind:Block}").setFields("*", "[child parentFilter=kind:Block]")
    val mapper = FromSolrDoc[CodeblockEt]
    val blocks = Using.resource(LocalSolr(File(Resource.getUrl("solrdir/")).toJava)) { solr =>
      solr.query(query).getResults.asScala.map(mapper.fromSolrDoc(_).get).toList
    }

    // Build test suite for each file
    thyFiles.map { file =>
      buildTestsuite(file, blocks.filter(_.theory == s"$Session.${File(file).name}"), toolbox)
    }
  }

  /** Builds a test suite from a theory spec file.
    *
    * @param thyFile Isabelle thy file with SPEC tests
    * @param blocks code blocks generated by the import of the Isabelle theory
    * @param toolbox for JIT compilation
    * @return Compiled test suite
    */
  private def buildTestsuite(thyFile: String, blocks: List[CodeblockEt], toolbox: ToolBox[universe.type]): Suite = {
    // Read spec tests from theory file
    val file = Resource.getAsString(thyFile)

    // Try to parse 'SPEC' format
    val testSpecs = fastparse.parse(file, SpecTestParser.file(_), verboseFailures = true) match {
      case Parsed.Success(value, _) => value
      case Parsed.Failure(label, index, _) =>
        return new FunSuite() { test(s"Spec test $thyFile")(fail(s"Could not parse spec file at $index: $label")) }
    }

    // Try to join spec and block
    val blockByLine = blocks.map(b => b.startLine -> b).toMap
    val contexts = ListBuffer.empty[SpecTestContext]
    val testCaseAsts = ListBuffer.empty[Tree]
    testSpecs.foreach(spec =>
      blockByLine.get(spec.specLine + 1) match {
        case Some(block) =>
          testCaseAsts += buildTest(contexts.size, spec, toolbox)
          contexts += it.SpecTestContext(spec.name, block, spec.thyCode, spec.specLine + 1)
        case None =>
          val specNameAst = toolbox.parse(s""" "${spec.name}" """).asInstanceOf[Literal]
          testCaseAsts += q"""test("Find block for spec")(fail("Could not find block for " + $specNameAst))"""
    })

    // Build test suite creator (function that takes all the contexts and makes them available in the testsuite)
    val buildTestSuiteAst =
      q"""(contexts: List[de.qaware.findfacts.it.SpecTestContext]) =>
          new org.scalatest.FunSuite with org.scalatest.Matchers { ..${testCaseAsts.toList} }"""

    val buildTestSuite = toolbox.eval(buildTestSuiteAst).asInstanceOf[List[SpecTestContext] => FunSuite]
    buildTestSuite(contexts.toList)
  }

  /** Build a single test abstract syntax tree for a given theory spec.
    *
    * @param idx of the context for the test
    * @param spec of the text
    * @param toolbox for JIT compilation
    * @return AST of test method call
    */
  private def buildTest(idx: Int, spec: Spec, toolbox: ToolBox[universe.type]): Tree = {
    val ctxIndexAst = toolbox.parse(idx.toString).asInstanceOf[Literal]
    val testNameAst = toolbox.parse(s""" "${spec.name}" """).asInstanceOf[Literal]

    val testCodeAst = try {
      toolbox.parse(s"{${spec.testCode}}")
    } catch {
      case _: Exception => q"""fail("Could not compile test spec")"""
    }

    q"""test("SPEC test " + $testNameAst) {
          val ctx = contexts($ctxIndexAst)
          $testCodeAst
        }
     """
  }
}
