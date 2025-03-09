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
package org.cp.elements.data.oql.functions;

import java.util.function.Function;

import org.cp.elements.data.oql.QueryFunction;
import org.cp.elements.data.support.Iterables;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Constants;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.ThreadSafe;

/**
 * {@link QueryFunction} used to concatenate multiple values.
 *
 * @author John Blum
 * @param <T> {@link Class type} of {@link Object} on which this function is applied.
 * @see org.cp.elements.lang.annotation.ThreadSafe
 * @see org.cp.elements.data.oql.QueryFunction
 * @since 2.0.0
 */
@ThreadSafe
public class Concat<T> implements QueryFunction<T, String> {

  protected static final String DEFAULT_DELIMITER = StringUtils.COMMA_SPACE_DELIMITER;

  /**
   * Factory method used to construct a new {@link Concat} query function.
   *
   * @param <S> {@link Class type} of {@link Object} from which the value to concatenate is extracted.
   * @param function {@link Function} used to extract the value from the {@link S type} used in concatenation operation.
   * @return a new {@link Concat} query function.
   * @throws IllegalArgumentException if {@link Function} is {@literal null}.
   * @see java.util.function.Function
   */
  public static <S> Concat<S> of(@NotNull Function<S, ?> function) {
    return new Concat<>(function);
  }

  private String delimiter = DEFAULT_DELIMITER;
  private String name;

  private final Function<T, String> function;

  /**
   * Construct a new {@link Concat} query function.
   *
   * @param function {@link Function} used to extract the value from the {@link T type} used in concatenation operation.
   * @throws IllegalArgumentException if {@link Function} is {@literal null}.
   * @see java.util.function.Function
   */
  protected Concat(@NotNull Function<T, ?> function) {
    Assert.notNull(function, "Function is required");
    this.function = function.andThen(String::valueOf);
  }

  /**
   * Returns the {@link String name} given to this query function.
   *
   * @return the {@link String name} given to this query function.
   */
  @Override
  public String getName() {
    return StringUtils.defaultIfBlank(this.name, Constants.UNKNOWN);
  }

  /**
   * Concatenates the collection of values in the {@link Iterable}.
   *
   * @param iterable {@link Iterable} of {@link T Objects} to concatenate.
   * @return the {@link String} resulting from the concatenation.
   * @see java.lang.Iterable
   */
  @NullSafe
  @Override
  public String apply(Iterable<T> iterable) {

    String concatenation = StringUtils.EMPTY_STRING;

    for (T result : Iterables.nullSafeIterable(iterable)) {
      concatenation = StringUtils.hasText(concatenation) ? concatenation.concat(this.delimiter) : concatenation;
      concatenation = concatenation.concat(this.function.apply(result));
    }

    return concatenation;
  }

  /**
   * Builder method used to configure the {@link String delimiter} used when concatenating the values
   * into a {@link String}.
   *
   * @param delimiter {@link String} containing the delimiter used in concatenation; defaults to comma.
   * @return this {@link Concat} query function.
   */
  public Concat<T> delimitedWith(String delimiter) {
    this.delimiter = ObjectUtils.returnValueOrDefaultIfNull(delimiter, DEFAULT_DELIMITER);
    return this;
  }

  /**
   * Builder method used to assign a {@link String name} to this query function.
   *
   * @param name {@link String} containing the name given to this query function.
   * @return this query function.
   */
  public Concat<T> named(String name) {
    this.name = name;
    return this;
  }
}
