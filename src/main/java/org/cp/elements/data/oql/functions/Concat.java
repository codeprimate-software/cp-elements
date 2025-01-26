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

/**
 * {@link QueryFunction} used to concatenate multiple values.
 *
 * @author John Blum
 * @param <S> {@link Class type} of {@link Object} on which this function is applied.
 * @see org.cp.elements.data.oql.QueryFunction
 * @since 2.0.0
 */
public class Concat<S> implements QueryFunction<S, String> {

  protected static final String DEFAULT_DELIMITER = StringUtils.COMMA_SPACE_DELIMITER;

  public static <S> Concat<S> of(@NotNull Function<S, ?> function) {
    return new Concat<>(function);
  }

  private String delimiter = DEFAULT_DELIMITER;
  private String name;

  private final Function<S, String> function;

  protected Concat(@NotNull Function<S, ?> function) {
    Assert.notNull(function, "Function is required");
    this.function = function.andThen(String::valueOf);
  }

  @Override
  public String getName() {
    return StringUtils.defaultIfBlank(this.name, Constants.UNKNOWN);
  }

  @Override
  public String apply(Iterable<S> iterable) {

    String concatenation = StringUtils.EMPTY_STRING;

    for (S result : Iterables.nullSafeIterable(iterable)) {
      concatenation = StringUtils.hasText(concatenation) ? concatenation.concat(this.delimiter) : concatenation;
      concatenation = concatenation.concat(this.function.apply(result));
    }

    return concatenation;
  }

  public Concat<S> delimitedWith(String delimiter) {
    this.delimiter = ObjectUtils.returnValueOrDefaultIfNull(delimiter, DEFAULT_DELIMITER);
    return this;
  }

  public Concat<S> named(String name) {
    this.name = name;
    return this;
  }
}
