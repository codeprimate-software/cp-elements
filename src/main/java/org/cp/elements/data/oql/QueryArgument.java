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
package org.cp.elements.data.oql;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Nameable;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Abstract Data Type (ADT) modeling a OQL {@link Query} argument.
 *
 * @author John Blum
 * @param <T> {@link Class type} of the query argument value.
 * @param name {@link String name} for the query argument.
 * @param value {@link T value} for the query argument.
 * @see org.cp.elements.lang.Nameable
 * @since 2.0.0
 */
public record QueryArgument<T>(@NotNull String name, @Nullable T value) implements Nameable<String> {

  /**
   * Factory method used to construct a new {@link QueryArgument} with the given {@link String argument name}
   * and {@link T argument value}.
   *
   * @param <T> {@link Class type} of the {@link Object query argument}.
   * @param name {@link String} containing a name for the query argument.
   * @param value {@link T value} of the query argument.
   * @return a new {@link QueryArgument} with the given {@link String argument name} and {@link T argument value}.
   * @throws IllegalArgumentException if {@link String name} is {@literal null} or {@literal empty}.
   */
  static <T> QueryArgument<T> from(@NotNull String name, @Nullable T value) {
    return new QueryArgument<>(name, value);
  }

  /**
   * Constructs a new {@link QueryArgument} with the given {@link String argument name} and {@link T argument value}.
   *
   * @param name {@link String} containing a name for the query argument.
   * @param value {@link T value} of the query argument.
   * @throws IllegalArgumentException if {@link String name} is {@literal null} or {@literal empty}.
   */
  public QueryArgument {
    Assert.hasText(name, "Name [%s] is required", name);
  }

  /**
   * Returns the {@link String name} of the query argument.
   *
   * @return the {@link String name} of the query argument.
   * @see #name()
   */
  @Override
  public String getName() {
    return name();
  }
}
