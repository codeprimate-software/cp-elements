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

import java.io.Serial;
import java.io.Serializable;
import java.util.Comparator;
import java.util.Iterator;

import org.cp.elements.data.oql.Oql;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.util.ArrayBuilder;
import org.cp.elements.util.ArrayUtils;

/**
 * Default implementation of {@link Oql.OrderBy}.
 *
 * @author John Blum
 * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
 * @param <T> {@link Class type} of the {@link Object projected objects}.
 * @param from {@link Oql.From} clause in which this {@link Oql.OrderBy} clause is based.
 * @param comparators array of {@link Comparator Comparators} defining the (sort) order.
 * @see java.util.Comparator
 * @see org.cp.elements.data.oql.Oql
 * @see org.cp.elements.data.oql.Oql.From
 * @see org.cp.elements.data.oql.Oql.OrderBy
 * @since 2.0.0
 */
@SuppressWarnings({ "unchecked", "unused" })
public record OrderByClause<S, T>(@NotNull Oql.From<S, T> from, Comparator<T>... comparators)
    implements Oql.OrderBy<S, T> {

  /**
   * Record constructor used to perform additional initialization and argument/stated validation.
   *
   * @throws IllegalArgumentException if the {@link Oql.From} clause is {@literal null}
   * or the array of {@link Comparator Comparators} are {@literal null} or {@literal empty}.
   */
  public OrderByClause {
    Assert.notNull(from, "From is required");
    Assert.notEmpty(comparators, "Comparators used to sort are required");
  }

  /**
   * Factory method used to construct a new {@link OrderByClause} copied (cloned) from the existing {@link Oql.OrderBy}.
   *
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
   * @param <T> {@link Class type} of the {@link Object projected objects}.
   * @param orderBy {@link Oql.OrderBy} clause to copy; required.
   * @return a new {@link OrderByClause} copied form the existing {@link Oql.OrderBy}.
   * @throws IllegalArgumentException if the {@link Oql.OrderBy} clause to copy is {@literal null}.
   * @see Oql.OrderBy
   */
  public static <S, T> OrderByClause<S, T> copy(Oql.OrderBy<S, T> orderBy) {

    Assert.notNull(orderBy, "OrderBy clause to copy is required");

    Oql.From<S, T> from = orderBy.getFrom();
    Comparator<T>[] comparators = ArrayBuilder.from(orderBy).build();
    OrderByClause<S, T> orderByClause = of(from, comparators);

    if (from instanceof FromClause<S, T> fromClause) {
      fromClause.withOrderBy(orderByClause);
    }

    return orderByClause;
  }

  /**
   * Factory method used to construct a new {@link OrderByClause} with no sort order.
   *
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
   * @param <T> {@link Class type} of the {@link Object projected objects}.
   * @param from {@link Oql.From} clause in the OQL query; required.
   * @return a new {@link OrderByClause} defining no sort order.
   * @throws IllegalArgumentException if the {@link Oql.From} clause is {@literal null}.
   * @see Oql.From
   */
  @SuppressWarnings("unchecked")
  public static <S, T> OrderByClause<S, T> noOrder(@NotNull Oql.From<S, T> from) {
    return new OrderByClause<>(from);
  }

  /**
   * Factory method used to construct a new {@link OrderByClause} initialized with
   * the given array of {@link Comparator Comparators} to order (sort) the query result set.
   *
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
   * @param <T> {@link Class type} of the {@link Object projected objects}.
   * @param from {@link Oql.From} clause in the OQL query; required.
   * @param comparators array of {@link Comparator Comparators} used to order (sort) the results
   * in the query result set.
   * @return a new {@link OrderByClause}.
   * @throws IllegalArgumentException if the {@link Oql.From} clause is {@literal null}
   * or the array of {@link Comparator Comparators} are {@literal null} or {@literal empty}.
   * @see java.util.Comparator
   * @see Oql.From
   */
  @SafeVarargs
  public static <S, T> OrderByClause<S, T> of(@NotNull Oql.From<S, T> from, Comparator<T>... comparators) {
    return new OrderByClause<>(from, comparators);
  }

  @Override
  public Oql.From<S, T> getFrom() {
    return from();
  }

  @Override
  public Oql.OrderBy<S, T> descending() {
    return copy(Oql.OrderBy.super.descending());
  }

  @Override
  @SuppressWarnings("all")
  public Iterator<Comparator<T>> iterator() {
    return ArrayUtils.asIterator(comparators());
  }

  @Override
  public Oql.OrderBy<S, T> thenOrderBy(Comparator<T> comparator) {
    return copy(Oql.OrderBy.super.thenOrderBy(comparator));
  }

  protected static class NoOrder<S> implements Comparator<S>, Serializable {

    @Serial
    private static final long serialVersionUID = 6130342585715959145L;

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
