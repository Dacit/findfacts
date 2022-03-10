/*  Title:      findfacts/theory.scala
    Author:     Fabian Huch, TU Munich/QAware GmbH

Isabelle Export_Theory -> findfacts TheoryView mapping.
*/

package de.qaware.findfacts.importer


import _root_.scala.language.implicitConversions
import de.qaware.findfacts.importer.TheoryView.Source
import isabelle._


object Theory
{

  /* mappers for sealed traits */

  def map_term(term: isabelle.Term.Term): TheoryView.Term = term match
  {
    case t: Term.Const => new ConstTerm_Wrapper(t)
    case t: Term.Free => new Free_Wrapper(t)
    case t: Term.Var => new Var_Wrapper(t)
    case t: Term.Bound => new Bound_Wrapper(t)
    case t: Term.Abs => new Abs_Wrapper(t)
    case t: Term.App => new App_Wrapper(t)
  }

  def map_typ(typ: isabelle.Term.Typ): TheoryView.Typ = typ match
  {
    case t: Term.Type => new TypeTyp_Wrapper(t)
    case t: Term.TFree => new TFree_Wrapper(t)
    case t: Term.TVar => new TVar_Wrapper(t)
  }

  def map_proof(proof: isabelle.Term.Proof): TheoryView.Proof = proof match
  {
    case p: Term.MinProof.type => new MinProof_Wrapper(p)
    case p: Term.PBound => new PBound_Wrapper(p)
    case p: Term.Abst => new Abst_Wrapper(p)
    case p: Term.AbsP => new AbsP_Wrapper(p)
    case p: Term.Appt => new Appt_Wrapper(p)
    case p: Term.AppP => new AppP_Wrapper(p)
    case p: Term.Hyp => new Hyp_Wrapper(p)
    case p: Term.PAxm => new PAxm_Wrapper(p)
    case p: Term.PClass => new OfClass_Wrapper(p)
    case p: Term.Oracle => new Oracle_Wrapper(p)
    case p: Term.PThm => new PThm_Wrapper(p)
  }

  /* value classes as mappers for concrete types */

  class Prop_Wrapper(val inner: Export_Theory.Prop) extends AnyVal with TheoryView.Prop
  {
    override def typargs: List[(String, List[String])] = inner.typargs
    override def args: List[(String, TheoryView.Typ)] = inner.args map
    {
      case (s, t) => (s, map_typ(t))
    }
    override def term: TheoryView.Term = map_term(inner.term)
  }

  class Indexname_Wrapper(val inner: Term.Indexname) extends AnyVal with TheoryView.Indexname
  {
    override def name: String = inner.name
    override def index: Int = inner.index
  }

  class TypeTyp_Wrapper(val inner: Term.Type) extends AnyVal with TheoryView.TypeTyp
  {
    override def name: String = inner.name
    override def args: List[TheoryView.Typ] = inner.args.map(map_typ)
  }

  class TFree_Wrapper(val inner: Term.TFree) extends AnyVal with TheoryView.TFree
  {
    override def name: String = inner.name
    override def sort: List[String] = inner.sort
  }

  class TVar_Wrapper(val inner: Term.TVar) extends AnyVal with TheoryView.TVar
  {
    override def name: TheoryView.Indexname = new Indexname_Wrapper(inner.name)
    override def sort: List[String] = inner.sort
  }

  class ConstTerm_Wrapper(val inner: Term.Const) extends AnyVal with TheoryView.ConstTerm
  {
    override def name: String = inner.name
    override def typargs: List[TheoryView.Typ] = inner.typargs.map(map_typ)
  }

  class Free_Wrapper(val inner: Term.Free) extends AnyVal with TheoryView.Free
  {
    override def name: String = inner.name
    override def typ: TheoryView.Typ = map_typ(inner.typ)
  }

  class Var_Wrapper(val inner: Term.Var) extends AnyVal with TheoryView.Var
  {
    override def name: TheoryView.Indexname = new Indexname_Wrapper(inner.name)
    override def typ: TheoryView.Typ = map_typ(inner.typ)
  }

  class Bound_Wrapper(val inner: Term.Bound) extends AnyVal with TheoryView.Bound
  {
    override def index: Int = inner.index
  }

  class Abs_Wrapper(val inner: Term.Abs) extends AnyVal with TheoryView.Abs
  {
    override def name: String = inner.name
    override def typ: TheoryView.Typ = map_typ(inner.typ)
    override def body: TheoryView.Term = map_term(inner.body)
  }

  class App_Wrapper(val inner: Term.App) extends AnyVal with TheoryView.App
  {
    override def fun: TheoryView.Term = map_term(inner.fun)
    override def arg: TheoryView.Term = map_term(inner.arg)
  }

  class MinProof_Wrapper(val inner: Term.MinProof.type) extends AnyVal with TheoryView.MinProof

  class PBound_Wrapper(val inner: Term.PBound) extends AnyVal with TheoryView.PBound
  {
    override def index: Int = inner.index
  }

  class Abst_Wrapper(val inner: Term.Abst) extends AnyVal with TheoryView.Abst
  {
    override def name: String = inner.name
    override def typ: TheoryView.Typ = map_typ(inner.typ)
    override def body: TheoryView.Proof = map_proof(inner.body)
  }

  class AbsP_Wrapper(val inner: Term.AbsP) extends AnyVal with TheoryView.AbsP
  {
    override def name: String = inner.name
    override def hyp: TheoryView.Term = map_term(inner.hyp)
    override def body: TheoryView.Proof = map_proof(inner.body)
  }

  class Appt_Wrapper(val inner: Term.Appt) extends AnyVal with TheoryView.Appt
  {
    override def fun: TheoryView.Proof = map_proof(inner.fun)
    override def arg: TheoryView.Term = map_term(inner.arg)
  }

  class AppP_Wrapper(val inner: Term.AppP) extends AnyVal with TheoryView.AppP
  {
    override def fun: TheoryView.Proof = map_proof(inner.fun)
    override def arg: TheoryView.Proof = map_proof(inner.arg)
  }

  class Hyp_Wrapper(val inner: Term.Hyp) extends AnyVal with TheoryView.Hyp
  {
    override def hyp: TheoryView.Term = map_term(inner.hyp)
  }

  class PAxm_Wrapper(val inner: Term.PAxm) extends AnyVal with TheoryView.PAxm
  {
    override def name: String = inner.name
    override def types: List[TheoryView.Typ] = inner.types.map(map_typ)
  }

  class OfClass_Wrapper(val inner: Term.PClass) extends AnyVal with TheoryView.OfClass
  {
    override def typ: TheoryView.Typ = map_typ(inner.typ)
    override def cls: String = inner.cls
  }

  class Oracle_Wrapper(val inner: Term.Oracle) extends AnyVal with TheoryView.Oracle
  {
    override def name: String = inner.name
    override def prop: TheoryView.Term = map_term(inner.prop)
    override def types: List[TheoryView.Typ] = inner.types.map(map_typ)
  }

  class PThm_Wrapper(val inner: Term.PThm) extends AnyVal with TheoryView.PThm
  {
    override def theoryName: String = inner.theory_name
    override def name: String = inner.name
    override def types: List[TheoryView.Typ] = inner.types.map(map_typ)
  }

  class Position_Wrapper(val inner: isabelle.Position.T) extends AnyVal with TheoryView.Position
  {
    override def offset: Int = Properties.get(inner, Markup.OFFSET).getOrElse(error("Position missing")).toInt
    override def endOffset: Int = Properties.get(inner, Markup.END_OFFSET).getOrElse(error("Position missing")).toInt
  }

  class Entity_Wrapper[A <: Export_Theory.Content[A]](val inner: Export_Theory.Entity[A]) extends AnyVal with TheoryView.Entity
  {
    override def name: String = inner.name
    override def pos: TheoryView.Position = new Position_Wrapper(inner.pos)
  }

  class Type_Wrapper(val inner: Export_Theory.Entity[Export_Theory.Type]) extends AnyVal with TheoryView.Type
  {
    override def entity: TheoryView.Entity = new Entity_Wrapper(inner)
    override def args: List[String] = inner.content.getOrElse(error("Exports missing")).args
    override def abbrev: Option[TheoryView.Typ] = inner.content.getOrElse(error("Exports missing")).abbrev.map(map_typ)
  }

  class Const_Wrapper(val inner: Export_Theory.Entity[Export_Theory.Const]) extends AnyVal with TheoryView.Const
  {
    override def entity: TheoryView.Entity = new Entity_Wrapper(inner)
    override def typargs: List[String] = inner.content.getOrElse(error("Exports missing")).typargs
    override def typ: TheoryView.Typ = map_typ(inner.content.getOrElse(error("Exports missing")).typ)
    override def abbrev: Option[TheoryView.Term] = inner.content.flatMap(_.abbrev).map(map_term)
  }

  class Axiom_Wrapper(val inner: Export_Theory.Entity[Export_Theory.Axiom]) extends AnyVal with TheoryView.Axiom
  {
    override def entity: TheoryView.Entity = new Entity_Wrapper(inner)
    override def prop: TheoryView.Prop = new Prop_Wrapper(inner.content.getOrElse(error("Exports missing")).prop)
  }

  class Thm_Wrapper(val inner: Export_Theory.Entity[Export_Theory.Thm]) extends AnyVal with TheoryView.Thm
  {
    override def entity: TheoryView.Entity = new Entity_Wrapper(inner)
    override def prop: TheoryView.Prop = new Prop_Wrapper(inner.content.getOrElse(error("Exports missing")).prop)
    override def deps: List[String] = inner.content.getOrElse(error("Exports missing")).deps
    override def proof: TheoryView.Proof = map_proof(inner.content.getOrElse(error("Exports missing")).proof)
  }

  class Constdef_Wrapper(val inner: Export_Theory.Constdef) extends AnyVal with TheoryView.Constdef
  {
    override def name: String = inner.name
    override def axiomName: String = inner.axiom_name
  }

  class Typedef_Wrapper(val inner: Export_Theory.Typedef) extends AnyVal with TheoryView.Typedef
  {
    override def name: String = inner.name
    override def axiomName: String = inner.axiom_name
  }

  class Block_wrapper(val inner: Markup_Blocks.Block) extends AnyVal with TheoryView.Block
  {
    override def startPos: Int = inner.range.start
    override def endPos: Int = inner.range.stop
    override def startLine: Int = inner.start_line
    override def text: String = inner.body
    override def contains(entity: TheoryView.Entity): Boolean =
      inner.range.contains(Text.Range(entity.pos.offset, entity.pos.endOffset))
  }

  class Source_Wrapper(val inner: Markup_Blocks) extends AnyVal with TheoryView.Source
  {
    override def blocks: List[TheoryView.Block] = inner.blocks.map(new Block_wrapper(_))
    override def get(position: TheoryView.Position): Option[TheoryView.Block] =
      inner.get_containing(Text.Range(position.offset, position.endOffset)).map(new Block_wrapper(_))
  }

  def map_theory(
    session_name: String,
    isabelle_theory: Export_Theory.Theory,
    markup_Blocks: Markup_Blocks): TheoryView.Theory =
  {

    new TheoryView.Theory
    {
      override val name: String = isabelle_theory.name
      override val session: String = session_name
      override val source: Source = new Source_Wrapper(markup_Blocks)
      override val types: List[TheoryView.Type] = isabelle_theory.types.map(new Type_Wrapper(_))
      override val consts: List[TheoryView.Const] = isabelle_theory.consts.map(new Const_Wrapper(_))
      override val axioms: List[TheoryView.Axiom] = isabelle_theory.axioms.map(new Axiom_Wrapper(_))
      override val thms: List[TheoryView.Thm] = isabelle_theory.thms.map(new Thm_Wrapper(_))
      override val constdefs: List[TheoryView.Constdef] = isabelle_theory.constdefs.map(new Constdef_Wrapper(_))
      override val typedefs: List[TheoryView.Typedef] = isabelle_theory.typedefs.map(new Typedef_Wrapper(_))
    }
  }
}
