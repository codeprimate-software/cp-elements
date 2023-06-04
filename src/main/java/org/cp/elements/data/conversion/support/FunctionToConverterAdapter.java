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
package org.cp.elements.data.conversion.support;

import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalArgumentException;

import java.util.Optional;
import java.util.function.Function;

import org.cp.elements.data.conversion.AbstractConverter;
import org.cp.elements.data.conversion.Converter;

/**
 * {@link FunctionToConverterAdapter} is an Adapter class used to adapt a {@link Function} into an instance of
 * the {@link Converter} interface to be used in data conversions.
 *
 * @author John Blum
 * @param <T> {@link Class source type} to convert from.
 * @param <R> {@link Class target type} to convert to.
 * @see java.util.function.Function
 * @see org.cp.elements.data.conversion.AbstractConverter
 * @see org.cp.elements.data.conversion.Converter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class FunctionToConverterAdapter<T, R> extends AbstractConverter<T, R> {

  /**
   * Factory method used to construct an instance of the {@link FunctionToConverterAdapter} initialized
   * with the required {@link Function} to adapt into an instance of the {@link Converter} interface
   * to be used on data conversions.
   *
   * @param <T> {@link Class type} of the {@link Object value} to convert from.
   * @param <R> {@link Class type} of the {@link Object value} to convert to.
   * @param function {@link Function} to adapt into an instance of {@link Converter}; must not be {@literal null}.
   * @return an instance of {@link FunctionToConverterAdapter} adapting the given {@link Function}.
   * @throws IllegalArgumentException if {@link Function} is {@literal null}.
   * @see #FunctionToConverterAdapter(Function)
   * @see java.util.function.Function
   */
  public static <T, R> FunctionToConverterAdapter<T, R> of(Function<T, R> function) {
    return new FunctionToConverterAdapter<>(function);
  }

  private final Function<T, R> function;

  /**
   * Constructs a new {@link FunctionToConverterAdapter} initialized with the required {@link Function}
   * to adapt into an instance of the {@link Converter} interface to be used in data conversions.
   *
   * @param function {@link Function} to adapt into an instance of {@link Converter}; must not be {@literal null}.
   * @throws IllegalArgumentException if {@link Function} is {@literal null}.
   * @see java.util.function.Function
   */
  public FunctionToConverterAdapter(Function<T, R> function) {

    super(Optional.ofNullable(function)
      .map(Object::getClass)
      .orElseThrow(() -> newIllegalArgumentException("Function is required")));

    this.function = function;
  }

  /**
   * Returns a reference to the adapted {@link Function} used in data conversions.
   *
   * @return a reference to the adapted {@link Function} used in data conversions.
   * @see java.util.function.Function
   */
  protected Function<T, R> getFunction() {
    return this.function;
  }

  /**
   * Converts the given {@link Object value} of {@link Class type T} into a {@link Object value}
   * of {@link Class type R}.
   *
   * @param value {@link Object} of {@link Class type T} to convert.
   * @return a {@link Object converted value} of {@link Class type R}.
   * @see java.util.function.Function#apply(Object)
   * @see #getFunction()
   */
  @Override
  public R convert(T value) {
    return getFunction().apply(value);
  }
}
