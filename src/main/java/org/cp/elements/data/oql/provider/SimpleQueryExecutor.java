/*
 * Copyright 2017-Present Author or Authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.cp.elements.data.oql.provider;

import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalStateException;

import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Stream;

import org.cp.elements.data.oql.Oql;
import org.cp.elements.data.oql.Oql.GroupBy;
import org.cp.elements.data.oql.Oql.OrderBy;
import org.cp.elements.data.oql.Oql.Projection;
import org.cp.elements.data.oql.Oql.TransformingProjection;
import org.cp.elements.data.oql.Oql.Where;
import org.cp.elements.data.oql.Query;
import org.cp.elements.data.oql.QueryArgument;
import org.cp.elements.data.oql.QueryArguments;
import org.cp.elements.data.oql.QueryContext;
import org.cp.elements.data.oql.QueryExecutor;
import org.cp.elements.data.oql.QueryFunction;
import org.cp.elements.data.oql.QueryResult;
import org.cp.elements.data.oql.QueryResultSet;
import org.cp.elements.data.oql.support.Groups;
import org.cp.elements.data.oql.support.OqlUtils;
import org.cp.elements.function.CannedPredicates;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.util.CollectionUtils;
import org.cp.elements.util.stream.StreamUtils;

/**
 * Provider implementation of {@link QueryExecutor}.
 *
 * @author John Blum
 * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
 * @param <T> {@link Class type} of the {@link Object projected objects}.
 * @see org.cp.elements.data.oql.Oql
 * @see QueryExecutor
 * @see org.cp.elements.data.oql.Query
 * @since 2.0.0
 */
public class SimpleQueryExecutor<S, T> implements QueryExecutor<S, T> {

  @Override
  @SuppressWarnings("all")
  public Iterable<T> execute(@NotNull Query<S, T> query, Iterable<QueryArgument<?>> arguments) {

    Assert.notNull(query, "Query to execute is required");

    QueryArguments queryArguments = QueryArguments.of(arguments);

    QueryContext<S, T> queryContext = queryContext(query);

    Iterable<S> collection = query.collection();

    Groups<T> groups = query.groupBy()
      .map(Groups::from)
      .orElseGet(Groups::noop);

    Function<T, T> groupFunction = groups::group;

    Stream<T> stream = stream(collection) // From
      .filter(resolveQueryPredicate(query, queryArguments)) // Where
      .map(resolveProjectionMapping(queryContext)) // Selection Projection
      .map(groupFunction); // Group By

    Stream<T> processedStream = query.groupBy()
      .map(it -> groupsToStream(queryContext, queryArguments, stream, groups))
      .orElseGet(() -> ifSelectDistinctElse(query, stream))
      .sorted(resolveSort(query)) // Order By (Sort before Limit)
      .limit(resolveLimit(query)); // Limit

    List<T> results = processedStream.toList();

    return results;
  }

  @NullSafe
  protected <E> Stream<E> stream(Iterable<E> iterable) {
    return StreamUtils.stream(CollectionUtils.nullSafeIterable(iterable));
  }

  private QueryContext<S, T> queryContext(Query<S, T> query) {
    return QueryContext.from(query);
  }

  @SuppressWarnings({ "all", "rawtypes", "unchecked" })
  private Stream<T> groupsToStream(QueryContext<S, T> queryContext, QueryArguments queryArguments,
      Stream<T> originalStream, Groups<T> groups) {

    Oql.Projection<S, T> projection = resolveProjection(queryContext);

    if (!(projection instanceof TransformingProjection transformingProjection)) {
      throw newIllegalStateException("Expected OQL Projection to be a [%s]; but was [%s]",
        TransformingProjection.class.getSimpleName(), ObjectUtils.getClassSimpleName(projection));
    }

    originalStream.count();

    GroupBy<S, T> groupBy = queryContext.query().groupBy()
      .orElseThrow(() -> newIllegalStateException("GroupBy not present"));

    List<QueryFunction<T, Object>> queryFunctions = transformingProjection.stream().toList();

    Set<QueryResult<T>> queryResults = new HashSet<>();

    stream(groups).forEach(group -> {

      Map<String, Object> namedValues = new HashMap<>(queryFunctions.size());

      for (QueryFunction<T, Object> queryFunction : queryFunctions) {
        namedValues.put(queryFunction.getName(), queryFunction.apply(group));
      }

      QueryResult<T> queryResult = QueryResult.<T>builder()
        .withMap(namedValues)
        .build();

      queryResults.add(queryResult);
    });

    Predicate<T> groupByPredicate = resolveGroupPredicate(groupBy, queryArguments);

    QueryResultSet<T> queryResultSet = QueryResultSet.of(queryResults);

    Stream<T> stream = queryResultSet.stream()
      .map(queryResult -> (T) transformingProjection.remap(queryContext, queryResult))
      .filter(groupByPredicate);

    return stream;
  }

  private Stream<T> ifSelectDistinctElse(Query<S, T> query, Stream<T> stream) {
    return query.selection().isDistinct() ? stream.distinct() : stream;
  }

  private long resolveLimit(Query<S, T> query) {
    return query.limit();
  }

  private Predicate<T> resolveGroupPredicate(GroupBy<S, T> groupBy, QueryArguments queryArguments) {
    return OqlUtils.asPredicate(groupBy.getPredicate(), queryArguments);
  }

  private Projection<S, T> resolveProjection(QueryContext<S, T> queryContext) {
    return queryContext.getProjection();
  }

  private Function<S, T> resolveProjectionMapping(QueryContext<S, T> queryContext) {
    return target -> resolveProjection(queryContext).map(queryContext, target);
  }

  @SuppressWarnings("unchecked")
  private Predicate<S> resolveQueryPredicate(Query<S, T> query, QueryArguments arguments) {

    return query.predicate()
      .map(Where::getPredicate)
      .map(predicate -> OqlUtils.asPredicate(predicate, arguments))
      .orElseGet(() -> (Predicate<S>) CannedPredicates.ACCEPT_ALL);
  }

  private Comparator<T> defaultSort() {
    return (comparableOne, comparableTwo) -> 0;
  }

  private Comparator<T> resolveSort(Query<S, T> query) {

    return query.orderBy()
      .map(OrderBy::getOrder)
      .orElseGet(this::defaultSort);
  }
}
