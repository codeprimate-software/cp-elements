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
package org.cp.elements.data.oql.support;

import java.util.Comparator;
import java.util.Iterator;

import org.cp.elements.data.oql.Oql;
import org.cp.elements.data.oql.Oql.From;
import org.cp.elements.data.oql.Oql.OrderBy;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.util.ArrayUtils;

/**
 * Default implementation of {@link Oql.OrderBy}
 *
 * @author John Blum
 * @see java.util.Comparator
 * @see org.cp.elements.data.oql.Oql
 * @see org.cp.elements.data.oql.Oql.From
 * @see org.cp.elements.data.oql.Oql.OrderBy
 * @since 2.0.0
 */
@SuppressWarnings({ "unchecked", "unused" })
public record OrderByClause<S, T>(@NotNull From<S, T> from, Comparator<S>... comparators)
    implements Oql.OrderBy<S, T> {

  public static <S, T> OrderByClause<S, T> copy(OrderBy<S, T> orderBy) {

    Assert.notNull(orderBy, "OrderBy clause to copy is required");

    From<S, T> from = orderBy.getFrom();
    Comparator<S>[] comparators = OqlUtils.asArray(orderBy).build();
    OrderByClause<S, T> orderByClause = of(from, comparators);

    if (from instanceof FromClause<S,T> fromClause) {
      fromClause.withOrderBy(orderByClause);
    }

    return orderByClause;
  }

  @SuppressWarnings("unchecked")
  public static <S, T> OrderByClause<S, T> noOrder(@NotNull From<S, T> from) {
    return new OrderByClause<>(from);
  }

  @SafeVarargs
  public static <S, T> OrderByClause<S, T> of(@NotNull From<S, T> from, Comparator<S>... comparators) {
    return new OrderByClause<>(from, comparators);
  }

  public OrderByClause {
    Assert.notNull(from, "From is required");
    Assert.notEmpty(comparators, "Comparators used to sort are required");
  }

  @Override
  public From<S, T> getFrom() {
    return from();
  }

  @Override
  public OrderBy<S, T> descending() {
    return copy(OrderBy.super.descending());
  }

  @Override
  @SuppressWarnings("all")
  public Iterator<Comparator<S>> iterator() {
    return ArrayUtils.asIterator(comparators());
  }

  @Override
  public OrderBy<S, T> thenOrderBy(Comparator<S> comparator) {
    return copy(OrderBy.super.thenOrderBy(comparator));
  }

  protected static class NoOrder<S> implements Comparator<S> {

    @Override
    public int compare(S comparableOne, S comparableTwo) {
      return 0;
    }

    @Override
    public String toString() {
      return "No Order";
    }
  }
}
