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

package org.cp.elements.lang.support;

import java.util.Iterator;

import org.cp.elements.lang.Transformer;
import org.cp.elements.util.ArrayUtils;

/**
 * The ComposableTransformer class is a Transformer implementation combining two or more Transformer objects
 * in a composition delegate.
 *
 * @author John J. Blum
 * @param <T> the Class type of the object to transform.
 * @see java.lang.Iterable
 * @see org.cp.elements.lang.Transformer
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ComposableTransformer<T> implements Transformer<T>, Iterable<Transformer<T>> {

  private final Iterable<Transformer<T>> transformers;

  /**
   * Constructs an instance of the ComposableTransformer class composed of the specified Transformers delegated to
   * in the transformation operation.
   *
   * @param transformers the array of Transformers used as delegates in this Transformer composition.
   */
  @SafeVarargs
  private ComposableTransformer(final Transformer<T>... transformers) {
    this.transformers = ArrayUtils.iterable(transformers.clone());
  }

  /**
   * Composes the array of Transformers into a Transformer composition.
   *
   * @param <T> the Class type of the values transformed by the Transformers.
   * @param transformers the array of Transformers to combine into a composition.
   * @return a Transformer composition composed of the specified Transformers.  Returns null if the array reference
   * is null, or a single Transformer if the array is of length 1, otherwise a ComposableTransformer composed
   * of the Transformers in the array.
   */
  @SafeVarargs
  public static <T> Transformer<T> compose(final Transformer<T>... transformers) {
    return (ArrayUtils.isEmpty(transformers) ? null : (transformers.length == 1 ? transformers[0]
      : new ComposableTransformer<>(transformers)));
  }

  /**
   * Iterates over the Transformers in this composition.
   *
   * @return an Iterator over the Transformers in this composition.
   * @see java.lang.Iterable#iterator()
   */
  public Iterator<Transformer<T>> iterator() {
    return transformers.iterator();
  }

  /**
   * Transforms the given value of Class type T with the delegating Transformers.
   *
   * @param value the value to transform.
   * @return the transformed value.
   */
  public T transform(T value) {
    for (Transformer<T> transformer : transformers) {
      value = transformer.transform(value);
    }

    return value;
  }

}
