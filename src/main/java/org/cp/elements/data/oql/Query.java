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
import org.cp.elements.data.oql.Oql.Select;
import org.cp.elements.data.oql.Oql.Where;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.NotNull;

/**
 * Abstract Data Type (ADT) modeling the query of an {@literal OQL} statement.
 *
 * @author John Blum
 * @see java.lang.FunctionalInterface
 * @see org.cp.elements.data.oql.Oql
 * @see org.cp.elements.data.oql.Oql.Select
 * @see org.cp.elements.data.oql.Oql.From
 * @see org.cp.elements.data.oql.Oql.Where
 * @see org.cp.elements.data.oql.Oql.OrderBy
 * @see org.cp.elements.data.oql.Oql.GroupBy
 * @since 2.0.0
 */
@FunctionalInterface
@SuppressWarnings("unused")
public interface Query<S, T> {

  static <S, T> Query<S, T> from(@NotNull From<S, T> from) {
    Assert.notNull(from, "From is required");
    return () -> from;
  }

  From<S, T> getFrom();

  default Select<S, T> selection() {
    return getFrom().getSelection();
  }

  default Optional<Where<S, T>> predicate() {
    return getFrom().getWhere();
  }

  default Optional<OrderBy<S, T>> orderBy() {
    return getFrom().getOrderBy();
  }

  default long limit() {
    return getFrom().getLimit();
  }

  default Optional<GroupBy<S, T>> groupBy() {
    return getFrom().getGroupBy();
  }

  default Iterable<S> collection() {
    return getFrom().getCollection();
  }

  default Iterable<T> execute() {

    return Oql.Provider.getLoader()
      .getServiceInstance()
      .<S, T>executor()
      .execute(this);
  }
}
