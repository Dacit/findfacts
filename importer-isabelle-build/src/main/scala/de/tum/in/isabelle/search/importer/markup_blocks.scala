/*  Title:      findfacts/markup_tree.scala
    Author:     Fabian Huch, TU Munich/QAware GmbH

Markup block partitioning by command-spans.
*/

package de.tum.in.isabelle.search.importer

import isabelle._


object Markup_Blocks {
  object Append_Token extends Enumeration {
    val ALSO = Value("also")
    val AND = Value("and")
    val APPLY = Value("apply")
    val ASSUME = Value("assume")
    val BACK = Value("back")
    val BY = Value("by")
    val CASE = Value("case")
    val CLOSE = Value("}")
    val CONSIDER = Value("consider")
    val DEFER = Value("defer")
    val DEFINE = Value("define")
    val DONE = Value("done")
    val DOT = Value(".")
    val DOTDOT = Value("..")
    val FINALLY = Value("finally")
    val FIX = Value("fix")
    val FOCUS = Value("focus")
    val FROM = Value("from")
    val GUESS = Value("guess")
    val HAVE = Value("have")
    val HENCE = Value("hence")
    val LET = Value("let")
    val MOREOVER = Value("moreover")
    val NEXT = Value("next")
    val NITPICK = Value("nitpick")
    val NOTE = Value("note")
    val OBTAIN = Value("obtain")
    val OOPS = Value("oops")
    val PREFER = Value("prefer")
    val PRESUME = Value("presume")
    val PROOF = Value("proof")
    val QED = Value("qed")
    val QUICKCHECK = Value("quickcheck")
    val REFUTE = Value("refute")
    val SCHEMATIC_GOAL = Value("schematic_goal")
    val SHOW = Value("show")
    val SLEDGEHAMMER = Value("sledgehammer")
    val SORRY = Value("sorry")
    val SUBGOAL = Value("subgoal")
    val SUPPLY = Value("supply")
    val TERMINATION = Value("termination")
    val THEN = Value("then")
    val THUS = Value("thus")
    val TRY0 = Value("try0")
    val ULTIMATELY = Value("ultimately")
    val UNFOLDING = Value("unfolding")
    val USING = Value("using")
    val WHERE = Value("where")
    val WITH = Value("with")
  }

  sealed case class Block(range: Text.Range, start_line: Int, text: String, raw: XML.Body) {
    def append(other: Block): Block = {
      require(range.stop == other.range.start)
      Block(range.try_join(other.range).get, start_line, text + other.text, raw ::: other.raw)
    }
  }

  def from_XML(body: XML.Body): Markup_Blocks = {
    var start_line = 1

    val blocks = body.foldLeft(Nil: List[Block]) {
      case (acc, elem) =>
        val start_index = if (acc.isEmpty) 1 else acc.last.range.stop

        val content = XML.content(elem)
        val text = Symbol.decode(content)
        val block = Block(Text.Range(start_index, start_index + Symbol.length(content)), start_line, text, List(elem))

        start_line += content.count {
          case '\n' => true
          case _ => false
        }

        val isAppendToken = content.isBlank || Append_Token.values.exists { tkn =>
          content.trim.startsWith(tkn.toString) || content.trim.startsWith("@" + tkn.toString)
        }

        if (acc.nonEmpty && isAppendToken) {
          acc.dropRight(1) :+ acc.last.append(block)
        } else {
          acc :+ block
        }
    }

    new Markup_Blocks(blocks)
  }
}

final class Markup_Blocks private(val blocks: List[Markup_Blocks.Block]) {
  import Markup_Blocks._

  def get_containing(range: Text.Range): Option[Block] =
    blocks.find(_.range.contains(range))

  override def toString: String = blocks match {
    case Nil => "Empty"
    case list => list.mkString("Block(", ",", ")")
  }
}
