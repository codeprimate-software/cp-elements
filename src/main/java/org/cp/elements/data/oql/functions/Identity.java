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
package org.cp.elements.data.oql.functions;

import java.util.function.Function;

import org.cp.elements.data.oql.QueryFunction;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.ThreadSafe;

/**
 * {@link QueryFunction} implementation that returns the {@link Object result} of the {@link Function}
 * applied to a single target.
 *
 * @author John Blum
 * @param <T> {@link Class type} of {@link Object} on which this function is applied.
 * @param <V> {@link Class type} of {@link Object value resulting} from the computation of this function
 * @see org.cp.elements.lang.annotation.ThreadSafe
 * @see org.cp.elements.data.oql.QueryFunction
 * @since 2.0.0
 */
@ThreadSafe
public class Identity<T, V> implements QueryFunction<T, V> {

  /**
   * Factory method used to construct a new {@link Identity} query function.
   *
   * @param <T> {@link Class type} of {@link Object} from which the value is extracted.
   * @param <V> {@link Class type} of the extracted {@link Object value}.
   * @param function {@link Function} used to extract the vale from an {@link Object} of type {@link T}.
   * @return a new {@link Identity} query function.
   * @throws IllegalArgumentException if {@link Function} is {@literal null}.
   * @see java.util.function.Function
   */
  public static <T, V> Identity<T, V> of(@NotNull Function<T, V> function) {
    return new Identity<>(function);
  }

  protected static final String DEFAULT_NAME = "Identity";

  private String name;

  private final Function<T, V> function;

  /**
   * Constructs a new {@link Identity} query function.
   *
   * @param function {@link Function} used to extract the vale from an {@link Object} of type {@link T}.
   * @throws IllegalArgumentException if {@link Function} is {@literal null}.
   * @see java.util.function.Function
   */
  protected Identity(@NotNull Function<T, V> function) {
    this.function = ObjectUtils.requireObject(function, "Function is required");
  }

  /**
   * Returns the {@link String name} given to this query function.
   *
   * @return the {@link String name} given to this query function.
   */
  @Override
  public String getName() {
    return StringUtils.defaultIfBlank(this.name, DEFAULT_NAME);
  }

  @Override
  public V apply(@NotNull Iterable<T> iterable) {
    Assert.notEmpty(iterable, "Iterable is required");
    T element = iterable.iterator().next();
    return this.function.apply(element);
  }

  /**
   * Builder method used to assign a {@link String name} to this query function.
   *
   * @param name {@link String} containing the name given to this query function.
   * @return this query function.
   */
  public Identity<T, V> named(String name) {
    this.name = name;
    return this;
  }
}
