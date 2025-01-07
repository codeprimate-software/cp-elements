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
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Predicate;

import org.cp.elements.data.oql.Oql;
import org.cp.elements.data.oql.Oql.Executable;
import org.cp.elements.data.oql.Oql.From;
import org.cp.elements.data.oql.Oql.GroupBy;
import org.cp.elements.data.oql.Oql.Grouping;
import org.cp.elements.data.oql.Oql.OrderBy;
import org.cp.elements.data.oql.Oql.Projection;
import org.cp.elements.data.oql.Oql.Select;
import org.cp.elements.data.oql.Oql.Where;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.TypeResolver;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.util.CollectionUtils;

/**
 * Defaul implementation of {@link Oql.From}.
 *
 * @author John Blum
 * @see org.cp.elements.data.oql.Oql
 * @see org.cp.elements.data.oql.Oql.From
 * @since 2.0.0
 */
@SuppressWarnings("unused")
public class FromClause<S, T> implements Oql.From<S, T> {

  public static <S, T> FromClause.Builder<S, T> collection(Iterable<S> collection) {
    return new Builder<>(collection);
  }

  private volatile long limit = Oql.From.DEFAULT_LIMIT;

  private final AtomicReference<Class<S>> elementType = new AtomicReference<>();

  private final Iterable<S> collection;

  private volatile Select<S, T> selection;

  private volatile Where<S, T> where;

  private volatile OrderBy<S, T> orderBy;

  private volatile GroupBy<S, T> groupBy;

  protected FromClause(@NotNull Iterable<S> collection) {
    this(collection, null);
  }

  protected FromClause(@NotNull Iterable<S> collection, Class<S> elementType) {
    this.collection = ObjectUtils.requireObject(collection, "Collection is required");
    this.elementType.set(elementType);
  }

  public Iterable<S> getCollection() {
    return CollectionUtils.unmodifiableIterable(this.collection);
  }

  @Override
  public Select<S, T> getSelection() {
    Select<S, T> selection = this.selection;
    Assert.state(selection != null, "Selection was not initialized");
    return selection;
  }

  @Override
  @SuppressWarnings("unchecked")
  public Class<S> getType() {
    return this.elementType.updateAndGet(type -> type != null ? type
      : (Class<S>) TypeResolver.getInstance().resolveType(getCollection()));
  }

  public Optional<Where<S, T>> getWhere() {

    Where<S, T> where = this.where;
    Where<S, T> resolvedWhere = where != null ? where : WhereClause.all(this);

    return Optional.of(resolvedWhere);
  }

  @Override
  public Optional<OrderBy<S, T>> getOrderBy() {
    return Optional.ofNullable(this.orderBy);
  }

  @Override
  public long getLimit() {
    return this.limit;
  }

  @Override
  public Optional<GroupBy<S, T>> getGroupBy() {
    return Optional.ofNullable(this.groupBy);
  }

  @Override
  public Where<S, T> where(@NotNull Predicate<S> predicate) {
    Where<S, T> where = WhereClause.where(this, predicate);
    withWhere(where);
    return where;
  }

  @Override
  public OrderBy<S, T> orderBy(@NotNull Comparator<S> comparator) {
    OrderBy<S, T> orderBy = OrderByClause.of(this, comparator);
    withOrderBy(orderBy);
    return orderBy;
  }

  @Override
  public Executable<T> limit(long limit) {
    Assert.isTrue(limit > 0, "Limit [%d] must be greater than 0");
    this.limit = limit;
    return this;
  }

  @Override
  public GroupBy<S, T> groupBy(@NotNull Grouping<S> grouping) {
    return From.super.groupBy(grouping);
  }

  protected FromClause<S, T> withSelection(Select<S, T> selection) {
    this.selection = ObjectUtils.requireObject(selection, "Selection is required");
    initProjectionFromType(selection);
    return this;
  }

  private void initProjectionFromType(Select<S, T> selection) {

    Projection<S, T> projection = selection.getProjection();
    Class<S> fromType = selection.getProjection().getFromType();

    if (Object.class.equals(fromType)) {
      projection.fromType(getType());
    }
  }

  protected FromClause<S, T> withWhere(Where<S, T> where) {
    this.where = where;
    return this;
  }

  protected FromClause<S, T> withOrderBy(OrderBy<S, T> orderBy) {
    this.orderBy = orderBy;
    return this;
  }

  protected FromClause<S, T> withGroupBy(GroupBy<S, T> groupBy) {
    this.groupBy = groupBy;
    return this;
  }

  public record Builder<S, T>(Iterable<S> collection)
      implements org.cp.elements.lang.Builder<FromClause<S, T>> {

    public Builder {
      ObjectUtils.requireObject(collection, "Collection is required");
    }

    public FromClause<S, T> of(Class<S> elementType) {
      return new FromClause<>(collection(), elementType);
    }

    public FromClause<S, T> build() {
      return new FromClause<>(collection());
    }
  }
}
