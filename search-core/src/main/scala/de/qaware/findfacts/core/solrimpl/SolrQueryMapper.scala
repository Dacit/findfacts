package de.qaware.findfacts.core.solrimpl

import scala.jdk.CollectionConverters._
import scala.util.{Failure, Try, Success}

import org.apache.solr.client.solrj
import org.apache.solr.client.solrj.SolrQuery.ORDER
import org.apache.solr.client.solrj.request.json.{DomainMap, JsonQueryRequest, TermsFacetMap}
import org.apache.solr.common.params.CursorMarkParams

import de.qaware.findfacts.common.dt.EtField
import de.qaware.findfacts.core.solrimpl.SolrQueryLiterals.{ALL, AND, QUERY_ALL, QUERY_PARENT}
import de.qaware.findfacts.core.solrimpl.SolrQueryMapper._
import de.qaware.findfacts.core.{FacetQuery, FieldFilter, FilterQuery}

/**
 * Mappers to map queries to solr query string.
 *
 * @param fieldFilterMapper to map filter queries
 */
class SolrQueryMapper(fieldFilterMapper: SolrFieldFilterMapper) {

  /**
   * Builds single-string solr query for filters (parenty filters only, no sorting).
   *
   * @param index for recursive calls
   * @param queryService for recursive calls
   * @param filters to map (may only restrict parent fields)
   * @return string query representation
   */
  def buildParentFilters(
      filters: List[FieldFilter])(implicit index: String, queryService: SolrQueryService): Try[String] = {
    fieldFilterMapper.mapSimpleFieldFilters(filters) flatMap { filters =>
      if (filters.childFqs.nonEmpty) Failure(new IllegalArgumentException("Query contained child filters"))
      else if (filters.fqs.nonEmpty) Success(filters.fqs.mkString(s" $AND "))
      else Success(QUERY_ALL)
    }
  }

  /**
   * Builds solr query (that retrieves parent blocks) for a filter query.
   *
   * @param queryService for recursive calls
   * @param index for recursive calls
   * @param query to map
   * @return solrquery representation that can be fed to a solrJ client
   */
  def buildBlockFilterQuery(
      query: FilterQuery)(implicit index: String, queryService: SolrQueryService): Try[solrj.SolrQuery] = {
    fieldFilterMapper.mapFieldFilters(query.filters) map { filters =>
      val solrQuery = new solrj.SolrQuery()
        .setFacet(false)
        .addField(CHILD_FIELD)
        .setRows(query.pageSize)
        .addSort(SCORE_FIELD, ORDER.desc)
        .addSort(EtField.Id.name, ORDER.asc)

      var queryStr = s"$PARENT_QUERY_COMMON $SCORE_FIELD=max"

      if (filters.fqs.nonEmpty) {
        // Add filter queries in a literal query (v) for scoring
        queryStr = s"_query_:{!query v=$$fq} $AND _query_:$queryStr"
        solrQuery.setFilterQueries(filters.fqs: _*)
      }
      if (filters.childFqs.nonEmpty) {
        solrQuery
          // Add child query to filters for caching and as literal query (v) for scoring.
          .setQuery(s"$queryStr filters=$$child.fq v=$$child.fq}")
          .set(CHILD_FQ, filters.childFqs: _*)
      } else {
        solrQuery
          .setQuery(s"$queryStr}")
      }

      // Set cursor to start or next cursor for paging
      solrQuery.set(
        CursorMarkParams.CURSOR_MARK_PARAM,
        query.cursor match {
          case None => CursorMarkParams.CURSOR_MARK_START
          case Some(cursor) => cursor
        })
      solrQuery
    }
  }

  /**
   * Builds solr query for a filter query. This will NOT resolve parent/child relations.
   *
   * @param queryService for recursive calls
   * @param index for recursive calls
   * @param query to map
   * @return solrquery representation that can be fed to a solrJ client
   */
  def buildFilterQuery(
      query: FilterQuery)(implicit index: String, queryService: SolrQueryService): Try[solrj.SolrQuery] = {
    fieldFilterMapper.mapFieldFilters(query.filters) map { filters =>
      val solrQuery = new solrj.SolrQuery()
        .setQuery("_query_:{!query v=$fq}")
        .setFacet(false)
        .addField(CHILD_FIELD)
        .setRows(query.pageSize)
        .addSort(SCORE_FIELD, ORDER.desc)
        .addSort(EtField.Id.name, ORDER.asc)

      solrQuery.setFilterQueries(filters.fqs ++ filters.childFqs: _*)

      // Set cursor to start or next cursor for paging
      solrQuery.set(
        CursorMarkParams.CURSOR_MARK_PARAM,
        query.cursor match {
          case None => CursorMarkParams.CURSOR_MARK_START
          case Some(cursor) => cursor
        })
      solrQuery
    }
  }

  /**
   * Builds solr query for a facet query.
   *
   * @param queryService for recursive calls
   * @param index for recursive calls
   * @param facetQuery to transform
   * @return solr query
   */
  def buildBlockFacetQuery(
      facetQuery: FacetQuery)(implicit index: String, queryService: SolrQueryService): Try[JsonQueryRequest] = {
    fieldFilterMapper.mapFieldFilters(facetQuery.filters) map { filters =>
      val jsonRequest = new JsonQueryRequest()
        .setLimit(0)

      // Add params
      if (filters.fqs.nonEmpty) {
        jsonRequest.withParam("fq", filters.fqs.toList.asJava)
      }
      if (filters.childFqs.nonEmpty) {
        jsonRequest
          .setQuery(s"$PARENT_QUERY_COMMON filters=$$child.fq}")
          .withParam(CHILD_FQ, filters.childFqs.toList.asJava)
      } else {
        jsonRequest.setQuery(s"$PARENT_QUERY_COMMON}")
      }

      facetQuery.fields foreach { field =>
        val facet = new TermsFacetMap(field.name).setMinCount(1).setLimit(facetQuery.maxFacets + 1)
        if (field.isChild) {
          val domain = new DomainMap()
            .withTagsToExclude("top")
            .withFilter(s"{!filters param=$$child.fq excludeTags=${field.name}}")
            .withFilter(if (filters.fqs.nonEmpty) s"$CHILD_QUERY_COMMON filters=$$fq}" else s"$CHILD_QUERY_COMMON}")

          // For child fields, go to child documents domain and then count unique parent blocks.
          facet.withDomain(domain).withStatSubFacet(COUNT_FIELD, "uniqueBlock(_root_)")
        } else {
          facet.withDomain(new DomainMap().withTagsToExclude(field.name))
        }
        // Add facet
        jsonRequest.withFacet(field.name, facet)
      }

      jsonRequest
    }
  }
}

/** Companion object. */
object SolrQueryMapper {

  /** Child filter query field. */
  final val CHILD_FQ = "child.fq"

  /** Query name of '_childDocuments_' field. */
  final val CHILD_FIELD = s"[child parentFilter=$QUERY_PARENT limit=-1]"

  /** Name of the field which scores results, on which the result set is sorted. */
  final val SCORE_FIELD = "score"

  /** Common part of parent queries. */
  final val PARENT_QUERY_COMMON = s"{!parent tag=top which=$QUERY_PARENT"

  /** Common part of child queries. */
  final val CHILD_QUERY_COMMON = s"{!child of=$QUERY_PARENT"

  /** Name of field for block aggregation subfacet. Overrides the default 'count' field. */
  final val COUNT_FIELD = "count"
}
