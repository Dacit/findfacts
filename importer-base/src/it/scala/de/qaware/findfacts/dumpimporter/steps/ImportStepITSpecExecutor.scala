package de.qaware.findfacts.theoryimporter.steps

import de.qaware.findfacts.common.solr.ConstRecord
import de.qaware.findfacts.theoryimporter.StepContext
import org.scalatest.FunSuite

case class TestSpecPosition(id: String, startLine: Int, endLine: Int) {
  def toFQNString: String = s"""de.qaware.dumpimporter.steps.TestSpecPosition("${id}", ${startLine}, ${endLine})"""
}

case class TestSpecContext(
    entityById: Map[String, ConstRecord],
    startLineById: Map[String, Int],
    endLineById: Map[String, Int])

class ImportStepITSpecExecutor(
    val tests: TestSpecContext => FunSuite,
    val specPositions: Seq[TestSpecPosition],
    step: ImportStep) {
  def buildSuite(): FunSuite = {
    // Results are stored in contexts
    val context = StepContext.empty
    // Execute step on empty context
    step(context)

    val entityByPos = context.consts.groupBy { const =>
      (const.startPosition, const.endPosition)
    }

    val entityById = (specPositions map { pos =>
      (pos.id, entityByPos((pos.startLine, pos.endLine)).toSeq match {
        case Seq(entity) => entity
        case _ => return new FunSuite() { test("Check spec entities") { fail(s"No entity found for spec $pos") } }
      })
    }
      groupBy (_._1)
      mapValues (_.head._2))

    val byId = specPositions.groupBy(_.id)
    val startById = byId.mapValues(_.head.startLine)
    val endById = byId.mapValues(_.head.endLine)

    tests(TestSpecContext(entityById, startById, endById))
  }
}