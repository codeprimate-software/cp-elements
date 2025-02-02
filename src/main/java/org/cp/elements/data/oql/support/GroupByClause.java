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
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Predicate;

import org.cp.elements.data.oql.Oql;
import org.cp.elements.data.oql.Query;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Default implementation of {@link Oql.GroupBy}.
 *
 * @author John Blum
 * @see org.cp.elements.data.oql.Oql
 * @see org.cp.elements.data.oql.Oql.GroupBy
 * @see org.cp.elements.data.oql.support.Group
 * @see org.cp.elements.data.oql.support.Grouping
 * @since 2.0.0
 */
public class GroupByClause<S, T> implements Oql.GroupBy<S, T> {

  /**
   * Factory method used to copy the given {@link GroupByClause} and instantiate a new instance.
   * <p>
   * Additionally, associates the new {@link GroupByClause} to the {@link Query Query's} {@link Oql.From} clause.
   *
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} being queried.
   * @param <T> {@link Class type} of the {@literal projected elements}.
   * @param groupBy {@link GroupByClause} to copy; required.
   * @return a new {@link GroupByClause} copied from the given {@link GroupByClause}.
   * @throws IllegalArgumentException if the {@link GroupByClause} to copy is {@literal null}.
   */
  public static <S, T> GroupByClause<S, T> copy(@NotNull GroupByClause<S, T> groupBy) {

    Assert.notNull(groupBy, "GroupBy clause is required");

    Oql.From<S, T> from = groupBy.getFrom();
    Grouping<S> grouping = groupBy.getGrouping();
    GroupByClause<S, T> copy = of(from, grouping);

    copy.having(groupBy.getPredicate());

    if (from instanceof FromClause<S,T> fromClause) {
      fromClause.withGroupBy(copy);
    }

    return copy;
  }

  /**
   * Factory method used to construct a new {@link GroupByClause} initialized with the given {@link Oql.From} clause
   * and {@link Grouping}.
   *
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} being queried.
   * @param <T> {@link Class type} of the {@literal projected elements}.
   * @param from {@link Oql.From} clause to which the {@link GroupByClause} is associated.
   * @param grouping {@link Grouping} defining how {@link Object objects} from the {@link Iterable collection}
   * will be grouped, or combined.
   * @return a new {@link GroupByClause}.
   * @throws IllegalArgumentException if {@link Oql.From} or {@link Grouping} are {@literal null}.
   * @see Grouping
   * @see org.cp.elements.data.oql.Oql.From
   */
  public static <S, T> GroupByClause<S, T> of(@NotNull Oql.From<S, T> from, @NotNull Grouping<S> grouping) {
    return new GroupByClause<>(from, grouping);
  }

  private final Oql.From<S, T> from;

  private final Grouping<S> grouping;

  private final Map<Integer, Group<S, T>> groups = new ConcurrentHashMap<>();

  private volatile Predicate<T> predicate;

  /**
   * Constructs a new {@link Oql.GroupBy} clause initialized with the given {@link Oql.From} clause
   * and {@link Grouping}.
   *
   * @param from {@link Oql.From} clause to which the {@link GroupByClause} is associated.
   * @param grouping {@link Grouping} defining how {@link Object objects} from the {@link Iterable collection}
   * @throws IllegalArgumentException if {@link Oql.From} or {@link Grouping} are {@literal null}.
   * @see Grouping
   * @see org.cp.elements.data.oql.Oql.From
   */
  public GroupByClause(@NotNull Oql.From<S, T> from, @NotNull Grouping<S> grouping) {
    this.from = ObjectUtils.requireObject(from, "From clause is required");
    this.grouping = ObjectUtils.requireObject(grouping, "Grouping is required");
  }

  @Override
  public Oql.From<S, T> getFrom() {
    return this.from;
  }

  @Override
  public Grouping<S> getGrouping() {
    return this.grouping;
  }

  @Override
  public Predicate<T> getPredicate() {
    return this.predicate;
  }

  @Override
  public S group(@NotNull S target) {

    Assert.notNull(target, "Target object to group is required");

    this.groups.computeIfAbsent(getGrouping().group(target), groupNumber -> Group.with(this, groupNumber))
      .include(target);

    return target;
  }

  @Override
  public Oql.GroupBy<S, T> having(@Nullable Predicate<T> predicate) {
    this.predicate = predicate;
    return this;
  }

  @Override
  public Oql.OrderBy<S, T> orderBy(@NotNull Comparator<T> comparator) {
    return OrderByClause.copy(Oql.OrderBy.of(getFrom(), comparator));
  }
}
