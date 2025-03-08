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
package org.cp.elements.data.oql;

import java.util.Optional;

import org.cp.elements.data.oql.Oql.From;
import org.cp.elements.data.oql.Oql.GroupBy;
import org.cp.elements.data.oql.Oql.OrderBy;
import org.cp.elements.data.oql.Oql.Projection;
import org.cp.elements.data.oql.Oql.Select;
import org.cp.elements.data.oql.Oql.Where;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;

/**
 * Abstract Data Type (ADT) modeling the query of an {@literal OQL} statement.
 *
 * @author John Blum
 * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
 * @param <T> {@link Class type} of the {@link Object projected objects}.
 * @see java.lang.FunctionalInterface
 * @see org.cp.elements.data.oql.Oql
 * @see org.cp.elements.data.oql.Oql.Select
 * @see org.cp.elements.data.oql.Oql.From
 * @see org.cp.elements.data.oql.Oql.Where
 * @see org.cp.elements.data.oql.Oql.OrderBy
 * @see org.cp.elements.data.oql.Oql.LimitSpec
 * @see org.cp.elements.data.oql.Oql.GroupBy
 * @see org.cp.elements.data.oql.Oql.Executable
 * @since 2.0.0
 */
@FunctionalInterface
@SuppressWarnings("unused")
public interface Query<S, T> extends Oql.Executable<T> {

  /**
   * Factory method used to construct a new {@link Query} from the given {@link From} clause of an OQL query.
   *
   * @param <S> {@link Class type} of {@link Object objects} in the {@link Iterable collection} to query.
   * @param <T> {@link Class type} of the {@link Object projected objects}.
   * @param from {@link From} clause of the OQL query; required
   * @return a new {@link Query} from the given {@link From} clause of an OQL query.
   * @throws IllegalArgumentException if {@link From} is {@literal null}.
   */
  static <S, T> Query<S, T> from(@NotNull From<S, T> from) {
    Assert.notNull(from, "From is required");
    return () -> from;
  }

  /**
   * Returns a reference to the {@link From} clause of the {@link Query OQL query}.
   *
   * @return a reference to the {@link From} clause of the {@link Query OQL query}.
   * @see org.cp.elements.data.oql.Oql.From
   */
  From<S, T> getFrom();

  /**
   * Gets the {@link Select} clause of the OQL query.
   *
   * @return the {@link Select} clause of the OQL query.
   * @see #getFrom()
   * @see Select
   */
  default Select<S, T> selection() {
    return getFrom().getSelection();
  }

  /**
   * Gets the selected {@link Projection} of the OQL query.
   *
   * @return the selected {@link Projection} of the OQL query.
   * @see #selection()
   * @see Projection
   */
  default Projection<S, T> projection() {
    return selection().getProjection();
  }

  /**
   * Gets the {@link Optional} {@link Where} clause of the OQL query.
   *
   * @return the {@link Optional} {@link Where} clause of the OQL query.
   * @see java.util.Optional
   * @see #getFrom()
   * @see Where
   */
  default Optional<Where<S, T>> predicate() {
    return getFrom().getWhere();
  }

  /**
   * Gets the {@link Optional} {@link OrderBy} clause of the OQL query.
   *
   * @return the {@link Optional} {@link OrderBy} clause of the OQL query.
   * @see java.util.Optional
   * @see #getFrom()
   * @see OrderBy
   */
  default Optional<OrderBy<S, T>> orderBy() {
    return getFrom().getOrderBy();
  }

  /**
   * Gets the {@link Long limit} for the query results returned by the OQL query.
   *
   * @return the {@link Long limit} for the query results returned by the OQL query.
   * @see #getFrom()
   */
  default long limit() {
    return getFrom().getLimit();
  }

  /**
   * Gets the {@link Optional} {@link GroupBy} clause of the OQL query.
   *
   * @return the {@link Optional} {@link GroupBy} clause of the OQL query.
   * @see java.util.Optional
   * @see #getFrom()
   * @see GroupBy
   */
  default Optional<GroupBy<S, T>> groupBy() {
    return getFrom().getGroupBy();
  }

  /**
   * Returns the {@link Iterable collection} to query.
   *
   * @return the {@link Iterable collection} to query.
   * @see java.lang.Iterable
   */
  default Iterable<S> collection() {
    return getFrom().getCollection();
  }

  /**
   * Executes this {@link Query} with the given {@link Iterable query arguments}.
   *
   * @param arguments {@link Iterable} of {@link QueryArgument QueryArguments} passed to the OQL query.
   * @return the {@link Iterable result set} from excuting this {@link Query}.
   * @see java.lang.Iterable
   * @see QueryArgument
   */
  @NullSafe
  @Override
  default Iterable<T> execute(Iterable<QueryArgument<?>> arguments) {
    return Oql.defaultProvider().<S, T>executor().execute(this, QueryArguments.of(arguments));
  }
}
