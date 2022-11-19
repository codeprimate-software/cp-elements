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
package org.cp.elements.lang.support;

import org.cp.elements.lang.Builder;
import org.cp.elements.lang.Filter;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * {@link Builder} implementation of the {@literal Builder Software Design Pattern} used to compose
 * {@link Filter objects} with the help of the {@link ComposableFilter} class.
 *
 * This {@link Builder} class can be used in place of the {@link ComposableFilter}
 * if the {@literal Composite Software Design Pattern} is less desirable.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.Builder
 * @see org.cp.elements.lang.Filter
 * @see org.cp.elements.lang.support.ComposableFilter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class FilterBuilder<T> implements Builder<Filter<T>> {

  private Filter<T> filterInstance;

  /**
   * Adds the specified Filter to the composition of Filters joined using the AND operator.
   *
   * @param filter the Filter to add to the composition joined using the AND operator.
   * @return this FilterBuilder instance.
   * @see org.cp.elements.lang.Filter
   */
  public @NotNull FilterBuilder<T> addWithAnd(@Nullable Filter<T> filter) {
    this.filterInstance = ComposableFilter.and(this.filterInstance, filter);
    return this;
  }

  /**
   * Adds the specified Filter to the composition of Filters joined using the OR operator.
   *
   * @param filter the Filter to add to the composition joined using the OR operator.
   * @return this FilterBuilder instance.
   * @see org.cp.elements.lang.Filter
   */
  public @NotNull FilterBuilder<T> addWithOr(@Nullable Filter<T> filter) {
    this.filterInstance = ComposableFilter.or(this.filterInstance, filter);
    return this;
  }

  /**
   * Builds a composition of Filters combined from the addWithAnd and addWithOr methods.
   *
   * @return a Filter composition.
   * @see org.cp.elements.lang.Filter
   */
  public @Nullable Filter<T> build() {
    return this.filterInstance;
  }
}
