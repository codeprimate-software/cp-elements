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

import org.cp.elements.data.oql.Oql;
import org.cp.elements.data.oql.Oql.Distinct;
import org.cp.elements.data.oql.Oql.From;
import org.cp.elements.data.oql.Oql.Projection;
import org.cp.elements.data.oql.Oql.Select;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.ThreadSafe;

/**
 * Default implementation of {@link Oql.Select}.
 *
 * @author John Blum
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

  public static <S, T> SelectClause<S, T> select(@NotNull Projection<S, T> projection) {
    return new SelectClause<>(projection);
  }

  private volatile boolean distinct = Select.DEFAULT_DISTINCT;

  private final Projection<S, T> projection;

  public SelectClause(@NotNull Projection<S, T> projection) {
    this.projection = ObjectUtils.requireObject(projection, "Projection is required");
  }

  @Override
  public boolean isDistinct() {
    return this.distinct;
  }

  @Override
  public Projection<S, T> getProjection() {
    return this.projection;
  }

  @Override
  public Distinct<S, T> distinct() {
    this.distinct = true;
    return new DistinctBuilder<>(this);
  }

  @Override
  public From<S, T> from(Iterable<S> collection) {
    return buildFrom(this, collection);
  }

  private static <S, T> From<S, T> buildFrom(Select<S, T> select, Iterable<S> collection) {
    return FromClause.<S, T>collection(collection).build().withSelection(select);
  }

  record DistinctBuilder<S, T>(@NotNull Select<S, T> select) implements Oql.Distinct<S, T> {

    DistinctBuilder {
      ObjectUtils.requireObject(select, "Select is required");
    }

    @Override
    public From<S, T> from(Iterable<S> collection) {
      return buildFrom(select(), collection);
    }
  }
}
