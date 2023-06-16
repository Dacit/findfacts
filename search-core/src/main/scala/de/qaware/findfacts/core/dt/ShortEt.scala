package de.qaware.findfacts.core.dt

import io.circe.generic.auto._

import de.qaware.findfacts.common.dt.Children
import de.qaware.findfacts.common.dt.EtField._

/** Children field for short theory entities. */
case object ShortChildren extends Children[ShortThyEt] {
  override implicit val implicits: FieldImplicits[ShortThyEt] = FieldImplicits()
}

/**
 * Short source code block.
 *
 * @param id unique identifier
 * @param version of the containing theory
 * @param session home session of theory
 * @param theory source theory
 * @param file link for theory
 * @param startLine line at which block starts
 * @param srcBefore source code before this block
 * @param src source text
 * @param srcAfter source text after this block
 * @param entities child entities
 * @param command type of the command
 */
final case class ShortBlock(
    id: Id.T,
    version: Version.T,
    session: Session.T,
    theory: SourceTheory.T,
    file: SourceFile.T,
    startLine: StartLine.T,
    srcBefore: SourceCodeBefore.T,
    src: SourceCode.T,
    srcAfter: SourceCodeAfter.T,
    entities: ShortChildren.T,
    command: Command.T)

/**
 * Short theory entity.
 *
 * @param id unique identifier
 * @param kind of theory entity
 * @param name of theory entity
 */
final case class ShortThyEt(id: Id.T, kind: Kind.T, name: Name.T)
