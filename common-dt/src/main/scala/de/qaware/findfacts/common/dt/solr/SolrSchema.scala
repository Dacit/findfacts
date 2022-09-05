package de.qaware.findfacts.common.dt.solr

/** All solr fields. */
object SolrSchema {
  final val Id = "id"
  final val DocKind = "doc_kind"
  final val ParentFlag = "parent_flag"
  final val TheoryKind = "kind"
  final val Command = "command"
  final val Session = "session"
  final val SessionFacet = "session_facet"
  final val SourceTheory = "theory"
  final val SourceTheoryFacet = "theory_facet"
  final val StartLine = "start_line"
  final val SourceCodeBefore = "src_before"
  final val SourceCode = "src"
  final val SourceCodeAfter = "src_after"
  final val Name = "name"
  final val NameFacet = "name_facet"
  final val Uses = "uses"
  final val ConstantType = "type"
  final val ConstantTypeFacet = "type_facet"
  final val Children = "children"
}
