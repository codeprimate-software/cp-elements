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
import java.util.function.BiPredicate;

import org.cp.elements.data.oql.Oql;
import org.cp.elements.data.oql.Oql.ExecutableQuery;
import org.cp.elements.data.oql.Oql.GroupBy;
import org.cp.elements.data.oql.Oql.OrderBy;
import org.cp.elements.data.oql.Oql.Projection;
import org.cp.elements.data.oql.Oql.Select;
import org.cp.elements.data.oql.Oql.Where;
import org.cp.elements.data.oql.QueryArguments;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.TypeResolver;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.util.CollectionUtils;

/**
 * Default implementation of {@link Oql.From}.
 *
 * @author John Blum
 * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
 * @param <T> {@link Class type} of the {@link Object projected objects}.
 * @see java.lang.Iterable
 * @see org.cp.elements.data.oql.Oql
 * @see org.cp.elements.data.oql.Oql.From
 * @since 2.0.0
 */
@SuppressWarnings("unused")
public class FromClause<S, T> implements Oql.From<S, T> {

  /**
   * Factory method used to build a new {@link FromClause} initialized with the given {@link Iterable collection}.
   *
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
   * @param <T> {@link Class type} of the {@link Object projected objects}.
   * @param collection {@link Iterable} from which {@link Object Objects} are selected.
   * @return a new {@link FromClause} initialized with the given {@link Iterable collection}.
   * @throws IllegalArgumentException if {@link Iterable collection} is {@literal null}.
   * @see java.lang.Iterable
   */
  public static <S, T> FromClause.Builder<S, T> collection(@NotNull Iterable<S> collection) {
    return new Builder<>(collection);
  }

  private volatile long limit = Oql.From.DEFAULT_LIMIT;

  private final AtomicReference<Class<S>> elementType = new AtomicReference<>();

  private final Iterable<S> collection;

  private volatile Select<S, T> selection;

  private volatile Where<S, T> where;

  private volatile OrderBy<S, T> orderBy;

  private volatile GroupBy<S, T> groupBy;

  /**
   * Constructs a new {@link FromClause} initialized with the given {@link Iterable collection}.
   *
   * @param collection {@link Iterable} from which {@link Object Objects} are selected.
   * @throws IllegalArgumentException if {@link Iterable collection} is {@literal null}.
   * @see java.lang.Iterable
   */
  protected FromClause(@NotNull Iterable<S> collection) {
    this(collection, null);
  }

  /**
   * Constructs a new {@link FromClause} initialized with the given {@link Iterable collection}
   * and {@link Class element type}.
   *
   * @param collection {@link Iterable} from which {@link Object Objects} are selected; required.
   * @param elementType {@link Class type} of {@link Object elements} in the {@link Iterable collection}.
   * @throws IllegalArgumentException if {@link Iterable collection} is {@literal null}.
   * @see java.lang.Iterable
   */
  protected FromClause(@NotNull Iterable<S> collection, Class<S> elementType) {
    this.collection = ObjectUtils.requireObject(collection, "Collection is required");
    this.elementType.set(elementType);
  }

  /**
   * Gets the {@link Iterable collection} to query.
   *
   * @return the {@link Iterable collection} to query.
   * @see java.lang.Iterable
   */
  public Iterable<S> getCollection() {
    return CollectionUtils.unmodifiableIterable(this.collection);
  }

  /**
   * Gets the {@link Select selection} on this {@link Iterable collection}.
   *
   * @return the {@link Select selection} on this {@link Iterable collection}.
   * @throws IllegalStateException if {@link Select selection} is {@literal null}.
   * @see org.cp.elements.data.oql.Oql.Select
   */
  @Override
  public Select<S, T> getSelection() {
    Select<S, T> selection = this.selection;
    Assert.state(selection != null, "Selection was not initialized");
    return selection;
  }

  /**
   * Gets the {@link Class type} of {@link Object objects} being queried in this {@link Iterable collections}.
   * <p>
   * If the {@link Class element type} was not specified (configured), then the {@link Class element type}
   * will be resolved. The result of the {@link Class element type} resolution is cached.
   *
   * @return the {@link Class type} of {@link Object objects} being queried in this {@link Iterable collections}.
   */
  @Override
  @SuppressWarnings("unchecked")
  public Class<S> getType() {
    return this.elementType.updateAndGet(type -> type != null ? type
      : (Class<S>) TypeResolver.getInstance().resolveType(getCollection()));
  }

  /**
   * Returns an {@link Optional} {@link Oql.Where} clause of the OQL query.
   *
   * @return an {@link Optional} {@link Oql.Where} clause of the OQL query.
   * @see org.cp.elements.data.oql.Oql.Where
   * @see java.util.Optional
   */
  @Override
  public Optional<Where<S, T>> getWhere() {

    Where<S, T> where = this.where;
    Where<S, T> resolvedWhere = where != null ? where : WhereClause.all(this);

    return Optional.of(resolvedWhere);
  }

  /**
   * Returns an {@link Optional} {@link Oql.OrderBy} clause of the OQL query.
   *
   * @return an {@link Optional} {@link Oql.OrderBy} clause of the OQL query.
   * @see org.cp.elements.data.oql.Oql.OrderBy
   * @see java.util.Optional
   */
  @Override
  public Optional<OrderBy<S, T>> getOrderBy() {
    return Optional.ofNullable(this.orderBy);
  }

  /**
   * Returns a {@link Long limit} on the number of results returned by the OQL query.
   *
   * @return a {@link Long limit} on the number of results returned by the OQL query.
   */
  @Override
  public long getLimit() {
    return this.limit;
  }

  /**
   * Returns an {@link Optional} {@link Oql.GroupBy} clause of the OQL query.
   *
   * @return an {@link Optional} {@link Oql.GroupBy} clause of the OQL query.
   * @see org.cp.elements.data.oql.Oql.GroupBy
   * @see java.util.Optional
   */
  @Override
  public Optional<GroupBy<S, T>> getGroupBy() {
    return Optional.ofNullable(this.groupBy);
  }

  @Override
  public Where<S, T> where(@NotNull BiPredicate<QueryArguments, S> predicate) {
    Where<S, T> where = WhereClause.where(this, predicate);
    withWhere(where);
    return where;
  }

  @Override
  public OrderBy<S, T> orderBy(@NotNull Comparator<T> comparator) {
    OrderBy<S, T> orderBy = OrderByClause.of(this, comparator);
    withOrderBy(orderBy);
    return orderBy;
  }

  @Override
  public ExecutableQuery<S, T> limit(long limit) {
    Assert.isTrue(limit > 0, "Limit [%d] must be greater than 0");
    this.limit = limit;
    return this;
  }

  @Override
  public GroupBy<S, T> groupBy(@NotNull Grouping<T> grouping) {
    GroupBy<S, T> groupBy = GroupByClause.of(this, grouping);
    withGroupBy(groupBy);
    return groupBy;
  }

  /**
   * Builder method used to configure the {@link Select} clause in the OQL query.
   *
   * @param selection {@link Select} clause of the OQL query.
   * @return this {@link FromClause}
   * @throws IllegalArgumentException if {@link Select} clause is {@literal null}.
   * @see Select
   */
  protected FromClause<S, T> withSelection(@NotNull Select<S, T> selection) {
    this.selection = ObjectUtils.requireObject(selection, "Selection is required");
    initProjectionFromType(selection);
    return this;
  }

  /**
   * Builder method used to configure the {@link Where} clause in the OQL query.
   *
   * @param where {@link Where} clause of the OQL query.
   * @return this {@link FromClause}
   * @see Where
   */
  protected FromClause<S, T> withWhere(@Nullable Where<S, T> where) {
    this.where = where;
    return this;
  }

  /**
   * Builder method used to configure the {@link OrderBy} clause in the OQL query.
   *
   * @param orderBy {@link OrderBy} clause of the OQL query.
   * @return this {@link FromClause}
   * @see OrderBy
   */
  protected FromClause<S, T> withOrderBy(@Nullable OrderBy<S, T> orderBy) {
    this.orderBy = orderBy;
    return this;
  }

  /**
   * Builder method used to configure the {@link GroupBy} clause in the OQL query.
   *
   * @param groupBy {@link GroupBy} clause of the OQL query.
   * @return this {@link FromClause}
   * @see GroupBy
   */
  protected FromClause<S, T> withGroupBy(@Nullable GroupBy<S, T> groupBy) {
    this.groupBy = groupBy;
    return this;
  }

  private void initProjectionFromType(Select<S, T> selection) {
    initProjectionFromType(selection.getProjection());
  }

  private void initProjectionFromType(Projection<S, T> projection) {

    Class<S> fromType = projection.getFromType();

    if (ObjectUtils.isUnclassified(fromType)) {
      if (projection instanceof SelectClause.ProjectionWrapper<S, T> projectionWrapper) {
        projectionWrapper.usingFromType(getType());
      }
    }
  }

  /**
   * {@literal Builder} used to construct a new {@link FromClause}.
   *
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
   * @param <T> {@link Class type} of the {@link Object projected objects}.
   * @param collection {@link Iterable collection} to query; required.
   * @see java.lang.Iterable
   */
  public record Builder<S, T>(@NotNull Iterable<S> collection)
      implements org.cp.elements.lang.Builder<FromClause<S, T>> {

    /**
     * @param collection {@link Iterable collection} to query; required.
     * @throws IllegalArgumentException if {@link Iterable collection} is {@literal null}.
     */
    public Builder {
      ObjectUtils.requireObject(collection, "Collection is required");
    }

    /**
     * Builder method used to configure the {@link Class type} of {@link S object} being queried.
     *
     * @param elementType {@link Class type} of the {@link T object} being queried.
     * @return a new {@link FromClause}.
     * @see #collection()
     */
    public FromClause<S, T> of(Class<S> elementType) {
      return new FromClause<>(collection(), elementType);
    }

    /**
     * Builds a new {@link FromClause} from the {@link Iterable collection} to query.
     *
     * @return a new {@link FromClause}.
     * @throws IllegalArgumentException if {@link Iterable collection} to query is {@literal null}.
     * @see #collection()
     * @see FromClause
     */
    public FromClause<S, T> build() {
      return new FromClause<>(collection());
    }
  }
}
