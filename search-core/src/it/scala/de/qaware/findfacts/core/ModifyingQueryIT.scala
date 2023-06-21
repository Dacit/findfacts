package de.qaware.findfacts.core

import de.qaware.findfacts.common.dt.EtField.{StartLine, Uses}
import de.qaware.findfacts.common.dt._
import de.qaware.findfacts.common.solr.mapper.ToSolrDoc
import de.qaware.findfacts.common.solr.{LocalSolr, SolrRepository}
import de.qaware.findfacts.core.solrimpl.SolrQueryModule
import org.apache.solr.client.solrj.SolrQuery
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterEach, Inside}

/** Test base functionality of query module with a setup that's as simple as possible.  */
class ModifyingQueryIT extends AnyFunSuite with BeforeAndAfterEach with Matchers with Inside {
  implicit val index: String = LocalSolr.DEFAULT_CORE_NAME
  var itSolr: SolrRepository = _
  var queryModule: QueryModule = _

  override def beforeEach(): Unit = {
    itSolr = ITSolr()
    queryModule = new SolrQueryModule {
      override lazy val solr: SolrRepository = itSolr
    }
    itSolr.createIndex(LocalSolr.DEFAULT_CORE_NAME)

    // Add integration test data set
    val const1 = ConstantEt("Const.ExampleThy.Const1", "Const1", List("someId"), "'a => 'b")
    val block1 =
      CodeblockEt(
        "ExampleSession.ExampleThy.1.11",
        "Isabelle",
        "ExampleSession",
        "ExampleThy",
        "~~/ExampleThy.thy",
        1,
        "fun",
        "\n",
        "fun Example = ...",
        "\n...",
        "<span>fun Example = ...</span>",
        List(const1))
    val fact1 = FactEt("Fact.ExampleThy.ConstIsFact", "ConstIsFact", List(const1.id))
    val block2 =
      CodeblockEt(
        "ExampleSession.ExampleThy.12.14",
        "Isabelle",
        "ExampleSession",
        "ExampleThy",
        "~~/ExampleThy.thy",
        3,
        "lemma",
        "\n...",
        "lemma example_lem...",
        "(* stuff *)",
        "<span>lemma example_lem...</span>",
        List(fact1))

    val mapper = ToSolrDoc[BaseEt]
    itSolr.add(mapper.toSolrDoc(block1))
    itSolr.add(mapper.toSolrDoc(block2))
    itSolr.commit().getStatus should (be(200) or be(0))
  }

  override def afterEach(): Unit = itSolr.close()

  test("Delete child query") {
    val delete_query = List(FieldFilter(EtField.Kind, Exact(Kind.Constant.entryName)))
    val res = queryModule.service.deleteBlock(delete_query)
    res.isFailure should be(true)
  }

  test("Delete parent query") {
    queryModule.service.getResultShortlist(FilterQuery(Nil)).get.values should have size 2

    val delete_query = List(FieldFilter(EtField.Command, Exact("lemma")))
    val res = queryModule.service.deleteBlock(delete_query)
    res.get should be(())

    queryModule.service.getResultShortlist(FilterQuery(Nil)).get.values should have size 1
  }

  test("Delete multi query") {
    queryModule.service.getResultShortlist(FilterQuery(Nil)).get.values should have size 2

    val delete_query = List(
      FieldFilter(EtField.Session, Exact("ExampleSession")),
      FieldFilter(EtField.Command, Exact("lemma")))
    val res = queryModule.service.deleteBlock(delete_query)
    res.get should be(())

    queryModule.service.getResultShortlist(FilterQuery(Nil)).get.values should have size 1
  }
}
