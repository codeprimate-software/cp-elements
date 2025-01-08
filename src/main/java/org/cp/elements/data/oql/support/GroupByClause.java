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
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Predicate;

import org.cp.elements.data.oql.Oql;
import org.cp.elements.data.oql.Oql.From;
import org.cp.elements.data.oql.Oql.Grouping;
import org.cp.elements.data.oql.Oql.OrderBy;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Numbered;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;

/**
 * Default implementation of {@link Oql.GroupBy}.
 *
 * @author John Blum
 * @see org.cp.elements.data.oql.Oql
 * @see org.cp.elements.data.oql.Oql.GroupBy
 * @see org.cp.elements.data.oql.Oql.Grouping
 * @see org.cp.elements.data.oql.support.GroupByClause.Group
 * @since 2.0.0
 */
public class GroupByClause<S, T> implements Oql.GroupBy<S, T> {

  public static <S, T> GroupByClause<S, T> copy(@NotNull GroupByClause<S, T> groupBy) {

    Assert.notNull(groupBy, "GroupBy clause is required");

    From<S, T> from = groupBy.getFrom();
    Grouping<S> grouping = groupBy.getGrouping();
    GroupByClause<S, T> copy = of(from, grouping);

    copy.having(groupBy.getPredicate());

    if (from instanceof FromClause<S,T> fromClause) {
      fromClause.withGroupBy(copy);
    }

    return copy;
  }

  public static <S, T> GroupByClause<S, T> of(@NotNull From<S, T> from, @NotNull Grouping<S> grouping) {
    return new GroupByClause<>(from, grouping);
  }

  private final From<S, T> from;

  private final Grouping<S> grouping;

  private final Map<Integer, Group<S, T>> groups = new ConcurrentHashMap<>();

  private volatile Predicate<T> predicate;

  public GroupByClause(@NotNull From<S, T> from, @NotNull Grouping<S> grouping) {
    this.from = ObjectUtils.requireObject(from, "From is required");
    this.grouping = ObjectUtils.requireObject(grouping, "Grouping is required");
  }

  @Override
  public From<S, T> getFrom() {
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
  public S compute(S target) {

    Assert.notNull(target, "Target object to group is required");

    this.groups.computeIfAbsent(getGrouping().group(target), groupNumber -> Group.with(this, groupNumber))
      .apply(target);

    return target;
  }

  @Override
  public Oql.GroupBy<S, T> having(Predicate<T> predicate) {
    this.predicate = predicate;
    return this;
  }

  @Override
  public OrderBy<S, T> orderBy(Comparator<S> comparator) {
    return OrderByClause.copy(OrderBy.of(getFrom(), comparator));
  }

  public static class Group<S, T> implements Numbered {

    static <S, T> Group<S, T> with(Oql.GroupBy<S, T> groupBy, int number) {
      return new Group<>(groupBy, number);
    }

    private final int number;

    private final AtomicLong count = new AtomicLong(0L);

    private final Oql.GroupBy<S, T> groupBy;

    Group(Oql.GroupBy<S, T> groupBy, int number) {
      this.groupBy = ObjectUtils.requireObject(groupBy, "GroupBy is required");
      this.number = number;
    }

    protected Oql.GroupBy<S, T> getGroupBy() {
      return this.groupBy;
    }

    public long getNumber() {
      return this.number;
    }

    public S apply(S target) {
      this.count.incrementAndGet();
      return target;
    }
  }
}
