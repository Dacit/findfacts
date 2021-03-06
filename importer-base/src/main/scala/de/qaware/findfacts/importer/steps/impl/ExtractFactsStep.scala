package de.qaware.findfacts.importer.steps.impl

import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.common.dt.{FactEt, Kind}
import de.qaware.findfacts.importer.steps.impl.thy.{ProofExtractor, PropExtractor}
import de.qaware.findfacts.importer.steps.impl.util.IdBuilder
import de.qaware.findfacts.importer.steps.{ImportStep, StepContext}
import de.qaware.findfacts.importer.{ImportError, TheoryView}

/**
 * Extracts the facts from a theory view.
 *
 * @param idBuilder to build entity ids
 * @param propExtractor to extract proposition data
 * @param proofExtractor to extract proof data
 */
class ExtractFactsStep(idBuilder: IdBuilder, propExtractor: PropExtractor, proofExtractor: ProofExtractor)
  extends ImportStep {
  private val logger = Logger[ExtractFactsStep]

  override def execute(theory: TheoryView.Theory)(implicit ctx: StepContext): List[ImportError] = {
    logger.debug(s"Importing ${theory.axioms.size} axioms and ${theory.thms.size} thms...")

    // Filter out definition axioms as they are aggregated in their entity types
    val defNames: Set[String] = (theory.constdefs.map(_.axiomName) ++ theory.typedefs.map(_.axiomName)).toSet

    // Add axioms
    theory.axioms.filterNot(ax => defNames.contains(ax.entity.name)) foreach { axiom =>
      val uses = idBuilder.getIds(Kind.Type, propExtractor.referencedTypes(axiom.prop).toList) ++
        idBuilder.getIds(Kind.Constant, propExtractor.referencedConsts(axiom.prop).toList)

      val fullName = axiom.entity.name
      val name = fullName.split('.').drop(1).mkString(".")
      ctx.putFact(axiom.entity.pos, FactEt(idBuilder.theoryEtId(Kind.Fact, fullName), name, uses))
    }

    // Add theorems
    theory.thms foreach { thm =>
      val fullName = thm.entity.name

      // Find usages
      val usedTypes = idBuilder.getIds(
        Kind.Type,
        (propExtractor.referencedTypes(thm.prop) ++ proofExtractor.referencedTypes(thm.proof)).toList)

      val usedConsts = idBuilder.getIds(
        Kind.Constant,
        (propExtractor.referencedConsts(thm.prop) ++ proofExtractor.referencedConsts(thm.proof)).toList)

      val usedFacts = idBuilder.getIds(
        Kind.Fact,
        (thm.deps ++ proofExtractor.referencedFacts(thm.proof).filterNot(fullName.equals)).distinct)

      val name = fullName.split('.').drop(1).mkString(".")
      ctx.putFact(
        thm.entity.pos,
        FactEt(idBuilder.theoryEtId(Kind.Fact, fullName), name, usedTypes ++ usedConsts ++ usedFacts))
    }

    logger.debug("Finished importing facts")
    List.empty
  }
}
