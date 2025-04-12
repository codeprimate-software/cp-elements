/*
 * Copyright 2011-Present Author or Authors.
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

import java.util.Iterator;

import org.cp.elements.data.oql.Oql;
import org.cp.elements.data.oql.QueryContext;
import org.cp.elements.data.oql.QueryFunction;
import org.cp.elements.data.oql.QueryResult;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.ThreadSafe;

/**
 * Default implementation of {@link Oql.Select}.
 *
 * @author John Blum
 * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
 * @param <T> {@link Class type} of the {@link Object projected objects}.
 * @see org.cp.elements.data.oql.Oql
 * @see org.cp.elements.data.oql.Oql.Distinct
 * @see org.cp.elements.data.oql.Oql.From
 * @see org.cp.elements.data.oql.Oql.Projection
 * @see org.cp.elements.data.oql.Oql.Select
 * @see org.cp.elements.lang.annotation.ThreadSafe
 * @since 2.0.0
 */
@ThreadSafe
public class SelectClause<S, T> implements Oql.Select<S, T> {

  /**
   * Factory method used to construct a new {@link SelectClause} with the given {@link Oql.Projection}.
   *
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
   * @param <T> {@link Class type} of the {@link Object projected objects}.
   * @param projection {@link Oql.Projection} selected in the OQL query.
   * @return a new {@link SelectClause}.
   * @see Oql.Projection
   */
  public static <S, T> SelectClause<S, T> select(@NotNull Oql.Projection<S, T> projection) {
    return new SelectClause<>(projection);
  }

  private volatile boolean distinct = Oql.Select.DEFAULT_DISTINCT;

  private final Oql.Projection<S, T> projection;

  /**
   * Constructs a new {@link SelectClause} with the given {@link Oql.Projection}.
   *
   * @param projection {@link Oql.Projection} selected in the OQL query.
   * @see Oql.Projection
   */
  public SelectClause(@NotNull Oql.Projection<S, T> projection) {
    this.projection = ProjectionWrapper.wrap(projection);
  }

  @Override
  public boolean isDistinct() {
    return this.distinct;
  }

  @Override
  public Oql.Projection<S, T> getProjection() {
    return this.projection;
  }

  /**
   * Applies uniqueness to (distinct results in) the query result set.
   *
   * @return the {@link Oql.Distinct} clause in the OQL query.
   * @see Oql.Distinct
   */
  @Override
  public Oql.Distinct<S, T> distinct() {
    this.distinct = true;
    return new DistinctBuilder<>(this);
  }

  @Override
  public Oql.From<S, T> from(Iterable<S> collection) {
    return buildFrom(this, collection);
  }

  private static <S, T> Oql.From<S, T> buildFrom(Oql.Select<S, T> select, Iterable<S> collection) {
    return FromClause.<S, T>collection(collection).build().withSelection(select);
  }

  record DistinctBuilder<S, T>(@NotNull Oql.Select<S, T> select) implements Oql.Distinct<S, T> {

    DistinctBuilder {
      ObjectUtils.requireObject(select, "Select is required");
    }

    @Override
    public Oql.From<S, T> from(Iterable<S> collection) {
      return buildFrom(select(), collection);
    }
  }

  static class ProjectionWrapper<S, T> implements Oql.Projection<S, T> {

    static <S, T> ProjectionWrapper<S, T> wrap(Oql.Projection<S, T> projection) {

      Assert.notNull(projection, "Projection is required");

      return projection instanceof Oql.TransformingProjection
        ? new TransformingProjectionWrapper<>(projection)
        : new ProjectionWrapper<>(projection);
    }

    private final Oql.Projection<S, T> projection;

    private Class<S> fromType;

    ProjectionWrapper(@NotNull Oql.Projection<S, T> projection) {
      this.projection = ObjectUtils.requireObject(projection, "Projection is required");
    }

    @SuppressWarnings("unchecked")
    protected <P extends Oql.Projection<S, T>> P getProjection() {
      return (P) this.projection;
    }

    @Override
    public Class<T> getType() {
      return getProjection().getType();
    }

    @Override
    public Class<S> getFromType() {
      Class<S> fromType = this.fromType;
      return fromType != null ? fromType : getProjection().getFromType();
    }

    @Override
    public T map(QueryContext<S, T> queryContext, S target) {
      return getProjection().map(queryContext, target);
    }

    @SuppressWarnings("unchecked")
    public <P extends Oql.Projection<S, T>> P usingFromType(Class<S> fromType) {
      this.fromType = fromType;
      return (P) this;
    }
  }

  static class TransformingProjectionWrapper<S, T> extends ProjectionWrapper<S, T>
      implements Oql.TransformingProjection<S, T> {

    TransformingProjectionWrapper(@NotNull Oql.Projection<S, T> projection) {
      super(projection);
    }

    @Override
    public T remap(QueryContext<S, T> queryContext, QueryResult<T> result) {
      return this.<Oql.TransformingProjection<S, T>>getProjection().remap(queryContext, result);
    }

    @Override
    @SuppressWarnings("all")
    public Iterator<QueryFunction<T, ?>> iterator() {
      return this.<Oql.TransformingProjection<S, T>>getProjection().iterator();
    }
  }
}
