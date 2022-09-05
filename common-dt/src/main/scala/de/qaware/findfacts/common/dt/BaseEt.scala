package de.qaware.findfacts.common.dt

import io.circe.generic.auto._
import io.swagger.annotations.ApiModel
import de.qaware.findfacts.common.da.api
import de.qaware.findfacts.common.da.api.DocumentKind
import de.qaware.findfacts.common.da.api.Variant.Discriminator
import de.qaware.findfacts.common.dt
import de.qaware.findfacts.common.dt.EtField._

/** Children for base theory blocks. */
case object TheoryChildren extends Children[TheoryEt] {
  override implicit val implicits: TheoryChildren.FieldImplicits[TheoryEt] = FieldImplicits()
}

/** Fields for all entities. */
sealed trait BaseEt {

  /** Unique id. */
  val id: Id.T
}

/** Fields for Entities that from the semantic theory. */
@ApiModel(description = "TheoryEt variants", subTypes = Array(classOf[ConstantEt], classOf[FactEt], classOf[TypeEt]))
sealed trait TheoryEt extends BaseEt {

  /** Name of the entity. */
  val name: Name.T

  /** Document Kind of the entity. */
  val docKind: DocKind.T

  /** Other entities that the entity uses. */
  val uses: Uses.T
}

/**
 * Any code block.
 *
 * @param theory source in which entity was defined
 * @param startLine line at which code block starts
 * @param srcBefore source code before this entity
 * @param src source code of the entity
 * @param srcAfter source code after this entity
 * @param entities entities from this block
 */
final case class CodeblockEt(
    override val id: Id.T,
    session: Session.T,
    theory: SourceTheory.T,
    startLine: StartLine.T,
    command: Command.T,
    srcBefore: SourceCodeBefore.T,
    src: SourceCode.T,
    srcAfter: SourceCodeAfter.T,
    entities: TheoryChildren.T)
  extends BaseEt
  with Discriminator[DocumentKind, EtField.DocKind.type, api.DocumentKind.Parent.type]

/**
 * Constant entity.
 *
 * @param constantType type of the constant
 */
final case class ConstantEt(
    override val id: Id.T,
    override val name: Name.T,
    override val uses: Uses.T,
    constantType: ConstantType.T,
    override val docKind: DocKind.T = DocumentKind.Child)
  extends TheoryEt
  with Discriminator[Kind, EtField.Kind.type, dt.Kind.Constant.type]

/** Fact entity. */
final case class FactEt(
    override val id: Id.T,
    override val name: Name.T,
    override val uses: Uses.T,
    override val docKind: DocKind.T = DocumentKind.Child)
  extends TheoryEt
  with Discriminator[Kind, EtField.Kind.type, dt.Kind.Fact.type]

/** Type entity. */
final case class TypeEt(
    override val id: Id.T,
    override val name: Name.T,
    override val uses: Uses.T,
    override val docKind: DocKind.T = DocumentKind.Child)
  extends TheoryEt
  with Discriminator[Kind, EtField.Kind.type, dt.Kind.Type.type]
