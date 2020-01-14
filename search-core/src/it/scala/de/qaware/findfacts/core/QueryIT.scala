package de.qaware.findfacts.core

import de.qaware.findfacts.common.dt.EtField.{Kind, Name, PropositionUses, StartPosition}
import de.qaware.findfacts.common.dt.{EtField, EtKind, FactEt}
import de.qaware.findfacts.common.solr.{ConstRecord, FactRecord}
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FunSuite, Matchers, TryValues}

class QueryIT extends FunSuite with BeforeAndAfterEach with BeforeAndAfterAll with Matchers with TryValues {
  val queryModule: ITSolrQueryModule = new ITSolrQueryModule {}

  override def beforeEach(): Unit = {
    // Reset solr data
    val solr = queryModule.repository.solrConnection()
    // First delete all
    solr.deleteByQuery("*:*")
    var status = solr.commit()
    status.getStatus should (be(200) or be(0))

    // Then add integration test data set
    val const1 = ConstRecord(
      "Example",
      1,
      11,
      "Const1",
      "'a => 'b",
      Array(),
      "prop",
      Array(),
      Array(),
      "fun Const1 = ..."
    )
    val fact1 =
      FactRecord("Example", 20, 22, "ConstIsFact", "IsFact Const1", Array(const1.id), Array(), Array(), "lemma ...")
    solr.addBean(const1)
    solr.addBean(fact1)
    status = solr.commit()
    status.getStatus should (be(200) or be(0))
  }

  override def afterAll(): Unit = {
    queryModule.repository.solrConnection().close()
  }

  test("Filter query") {
    val query = FilterQuery(Filter(Map(StartPosition -> InRange(10, 30))))
    val result = queryModule.service.getResults(query)

    val resList = result.success.value
    resList should have size 1
    resList should matchPattern {
      case Vector(_: FactEt) =>
    }
    resList match {
      case Vector(f: FactEt) =>
        f.name should equal("ConstIsFact")
        f.startPosition should equal(20)
        f.propositionUses should have size 1
        f.related should have size 0
    }
  }

  test("Filter query shortlist") {
    val query = FilterQuery(Filter(Map(Kind -> StringExpression(EtKind.Constant.toString))))
    val result = queryModule.service.getShortResults(query)

    val resList = result.success.value
    resList should have size 1

    val untypedRes = resList.head.asUntyped
    untypedRes.kind should equal(EtKind.Constant)
    untypedRes.sourceFile should equal("Example")
    untypedRes.startPosition should equal(1)
    untypedRes.shortDescription should equal("Const1 :: 'a => 'b")
  }

  test("Recursive query") {
    val innerQuery = Filter(Map(Kind -> StringExpression(EtKind.Constant.toString)))
    val query = FilterQuery(Filter(Map(PropositionUses -> AnyInResult(innerQuery))))
    val result = queryModule.service.getShortResults(query)

    result.success.value should have size 1
    result.success.value.head.startPosition should be(20)
  }

  test("Query set operations") {
    val noMatchQuery = Filter(Map(Name -> StringExpression("does not exist")))
    val query1 = Filter(Map(Kind -> AllInResult(noMatchQuery)))
    val query2 = Filter(Map(Kind -> StringExpression(EtKind.Constant.toString)))
    val query = FilterQuery(FilterIntersection(query1, query2))
    val result = queryModule.service.getShortResults(query)

    result.success.value should have size 1
    result.success.value.head.startPosition should be(1)
  }

  test("Facet query") {
    val query = FacetQuery(Filter(Map.empty), EtField.StartPosition)
    val result = queryModule.service.getFacetResults(query)

    val resultFacet = result.success.value
    resultFacet should equal(Map(1 -> 1, 20 -> 1))
  }
}