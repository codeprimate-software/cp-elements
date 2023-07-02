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

import java.util.function.Function;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Transformer;

/**
 * Adapter used to adapt a Java {@link Function} as a Elements {@link Transformer}.
 *
 * @author John Blum
 * @param <T> {@link Class type} of the {@link Object} to transform.
 * @see java.util.function.Function
 * @see org.cp.elements.lang.Transformer
 * @see <a href="https://en.wikipedia.org/wiki/Adapter_pattern">Adapter Software Design Pattern</a>
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class FunctionToTransformerAdapter<T> implements Transformer<T> {

  /**
   * Factory method use to construct a new {@link FunctionToTransformerAdapter} initialized with
   * the given {@link Function} to adapt as an instance of the {@link Transformer} interface.
   *
   * @param <T> {@link Class type} of {@link Object objects} processed by the {@link Function}.
   * @param function {@link Function} to adapt as a {@link Transformer}.
   * @return a new instance of {@link FunctionToTransformerAdapter} initialized with the given {@link Function}
   * to adapt as a {@link Transformer}.
   * @throws IllegalArgumentException if {@link Function} is {@literal null}.
   * @see #FunctionToTransformerAdapter(Function)
   * @see java.util.function.Function
   */
  public static <T> FunctionToTransformerAdapter<T> of(Function<T, T> function) {
    return new FunctionToTransformerAdapter<>(function);
  }

  private final Function<T, T> function;

  /**
   * Constructs an instance of the {@link FunctionToTransformerAdapter} initialized with the given {@link Function}
   * to adapt as an instance of the {@link Transformer} interface.
   *
   * @param function {@link Function} to adapt as a {@link Transformer}.
   * @throws IllegalArgumentException if {@link Function} is {@literal null}.
   * @see java.util.function.Function
   */
  public FunctionToTransformerAdapter(Function<T, T> function) {

    Assert.notNull(function, "Function is required");

    this.function = function;
  }

  /**
   * Returns a reference to the adapt {@link Function} backing this {@link Transformer}.
   *
   * @return a reference to the adapt {@link Function} backing this {@link Transformer}.
   * @see java.util.function.Function
   */
  protected Function<T, T> getFunction() {
    return this.function;
  }

  /**
   * Transforms the given {@link Object} by applying the {@link Function} of this {@link Transformer}.
   *
   * @param value {@link T value} to transform.
   * @return the {@link T transformed value}.
   * @see java.util.function.Function#apply(Object)
   * @see #getFunction()
   */
  @Override
  public T transform(T value) {
    return getFunction().apply(value);
  }
}
