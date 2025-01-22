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

import java.util.Comparator;
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Stream;

import org.cp.elements.data.oql.Oql.OrderBy;
import org.cp.elements.data.oql.Oql.Projection;
import org.cp.elements.data.oql.Oql.Select;
import org.cp.elements.data.oql.Oql.Where;
import org.cp.elements.data.oql.Query;
import org.cp.elements.data.oql.QueryContext;
import org.cp.elements.data.oql.QueryExecutor;
import org.cp.elements.function.CannedPredicates;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.util.CollectionUtils;
import org.cp.elements.util.stream.StreamUtils;

/**
 * Provider implementation of {@link QueryExecutor}.
 *
 * @author John Blum
 * @see org.cp.elements.data.oql.Oql
 * @see QueryExecutor
 * @see org.cp.elements.data.oql.Query
 * @since 2.0.0
 */
public class SimpleQueryExecutor<S, T> implements QueryExecutor<S, T> {

  @Override
  @SuppressWarnings("all")
  public Iterable<T> execute(@NotNull Query<S, T> query) {

    Assert.notNull(query, "Query to execute is required");

    Iterable<S> collection = query.collection();

    QueryContext<S, T> queryContext = QueryContext.from(query);

    Select<S, T> selection = query.selection();

    Stream<T> stream = stream(collection)
      .filter(resolvePredicate(query))
      .sorted(resolveSort(query))
      .map(resolveProjectionMapping(queryContext));

    if (selection.isDistinct()) {
      stream = stream.distinct();
    }

    stream = stream.limit(query.limit());

    List<T> results = stream.toList();

    return results;
  }

  @SuppressWarnings("unchecked")
  private Predicate<S> resolvePredicate(Query<S, T> query) {

    return query.predicate()
      .map(Where::getPredicate)
      .orElseGet(() -> (Predicate<S>) CannedPredicates.ACCEPT_ALL);
  }

  private Projection<S, T> resolveProjection(QueryContext<S, T> queryContext) {
    return queryContext.getProjection();
  }

  private Function<S, T> resolveProjectionMapping(QueryContext<S, T> queryContext) {
    return target -> resolveProjection(queryContext).map(queryContext, target);
  }

  private Comparator<S> resolveSort(Query<S, T> query) {

    return query.orderBy()
      .map(OrderBy::getOrder)
      .orElseGet(this::defaultSort);
  }

  private Comparator<S> defaultSort() {
    return (comparableOne, comparableTwo) -> 0;
  }

  private <S> Stream<S> stream(Iterable<S> collection) {
    return StreamUtils.stream(CollectionUtils.nullSafeIterable(collection));
  }
}
