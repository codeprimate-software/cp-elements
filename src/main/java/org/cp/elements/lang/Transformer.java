/*
 * Copyright 2016 Author or Authors.
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

package org.cp.elements.lang;

import java.util.function.Function;

import org.cp.elements.data.conversion.Converter;

/**
 * The {@link Transformer} interface defines a contract for implementing {@link Class classes}
 * who's {@link Object implementations} transform data from one value to another value
 * of the same {@link Class type}.
 *
 * The transformed value {@link Class type T} could be a more qualified, precise or accurate {@link Object value}
 * than the {@link Object original value}.  For example, converting an {@link Integer} into a {@link Double}
 * using a {@link Transformer Transformer<Number>}.
 *
 * Another use case for a {@link Transformer} might be to {@literal Decorate}, or enhance the functionality
 * of an {@link Object} yet still retain the same {@link Class interface}.
 *
 * Use a {@link Converter Converter<S, T>} if you want to "convert", or adapt a value from one {@link Class type}
 * to another {@link Class type}.
 *
 * @author John J. Blum
 * @param <T> {@link Class type} of the data value (datum) to transform as well as the transformed data value (datum).
 * @see java.lang.FunctionalInterface
 * @since 1.0.0
 */
@FunctionalInterface
public interface Transformer<T> {

  /**
   * Transforms the given value of {@link Class type T} into another value of the same {@link Class type T}.
   *
   * @param value {@link T value} to transform.
   * @return the {@link T transformed value}.
   */
  T transform(T value);

  /**
   * Returns this {@link Transformer} as an instance of a {@link Function}.
   *
   * @return a {@link Function} implementation wrapping this {@link Transformer}.
   * @see java.util.function.Function
   */
  default Function<T, T> asFunction() {
    return this::transform;
  }
}
