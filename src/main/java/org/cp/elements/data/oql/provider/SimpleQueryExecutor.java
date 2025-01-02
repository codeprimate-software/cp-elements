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

import org.cp.elements.data.oql.Oql;
import org.cp.elements.data.oql.Oql.OrderBy;
import org.cp.elements.data.oql.Oql.Projection;
import org.cp.elements.data.oql.Oql.Select;
import org.cp.elements.data.oql.Oql.Where;
import org.cp.elements.data.oql.Query;
import org.cp.elements.function.CannedPredicates;
import org.cp.elements.util.CollectionUtils;
import org.cp.elements.util.stream.StreamUtils;

/**
 * Provider implementation of {@link Oql.QueryExecutor}.
 *
 * @author John Blum
 * @see org.cp.elements.data.oql.Oql
 * @see org.cp.elements.data.oql.Oql.QueryExecutor
 * @see org.cp.elements.data.oql.Query
 * @since 2.0.0
 */
public class SimpleQueryExecutor<S, T> implements Oql.QueryExecutor<S, T> {

  @Override
  @SuppressWarnings("all")
  public Iterable<T> execute(Query<S, T> query) {

    Iterable<S> collection = query.collection();

    Select<S, T> selection = query.getSelection();

    Stream<T> stream = stream(collection)
      .filter(resolvePredicate(query))
      .sorted(resolveSort(query))
      .map(resolveProjectionMapping(selection));

    if (selection.isDistinct()) {
      stream = stream.distinct();
    }

    List<T> results = stream.toList();

    return results;
  }

  @SuppressWarnings("unchecked")
  private Predicate<S> resolvePredicate(Query<S, T> query) {

    return query.getPredicate()
      .map(Where::getPredicate)
      .orElseGet(() -> (Predicate<S>) CannedPredicates.ACCEPT_ALL);
  }

  private Projection<S, T> resolveProjection(Select<S, T> selection) {
    return selection.getProjection();
  }

  private Function<S, T> resolveProjectionMapping(Select<S, T> selection) {
    return resolveProjection(selection)::map;
  }

  private Comparator<S> resolveSort(Query<S, T> query) {

    return query.getOrder()
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
