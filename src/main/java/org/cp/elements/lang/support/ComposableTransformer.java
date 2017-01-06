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

import static org.cp.elements.util.ArrayUtils.asArray;
import static org.cp.elements.util.ArrayUtils.asIterable;
import static org.cp.elements.util.ArrayUtils.isEmpty;
import static org.cp.elements.util.ArrayUtils.nullSafeArray;

import java.util.Iterator;

import org.cp.elements.lang.NullSafe;
import org.cp.elements.lang.Transformer;
import org.cp.elements.util.CollectionUtils;

/**
 * The ComposableTransformer class is a Transformer implementation combining two or more Transformer objects
 * in a composition delegate.
 *
 * @author John J. Blum
 * @param <T> {@link Class} type of the object to transform.
 * @see java.lang.Iterable
 * @see org.cp.elements.lang.Transformer
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ComposableTransformer<T> implements Transformer<T>, Iterable<Transformer<T>> {

  private final Iterable<Transformer<T>> transformers;

  /**
   * Constructs an instance of the {@link ComposableTransformer} class composed of the given array
   * of {@link Transformer} objects delegated to in the transformation operation.
   *
   * @param transformers array of {@link Transformer Transformer} to compose.
   * @see org.cp.elements.lang.Transformer
   */
  @SafeVarargs
  private ComposableTransformer(Transformer<T>... transformers) {
    this.transformers = asIterable(nullSafeArray(transformers, Transformer.class).clone());
  }

  /**
   * Composes the array of {@link Transformer Transformers} into a composite.
   *
   * @param <T> {@link Class} type of the values transformed by the {@link Transformer Transformers}.
   * @param transformers array of {@link Transformer Transformers} to compose.
   * @return a composite {@link Transformer} composed of the given array of {@link Transformer Transformers}.
   * Returns {@literal null} if the array is {@literal null} or empty.  Returns a single {@link Transformer}
   * if the array has a length of 1, otherwise returns an instance of {@link ComposableTransformer} composed
   * of all {@link Transformer Transformers} in the array.
   * @see org.cp.elements.lang.Transformer
   * @see #ComposableTransformer(Transformer[])
   */
  @SafeVarargs
  public static <T> Transformer<T> compose(Transformer<T>... transformers) {
    return (isEmpty(transformers) ? null : (transformers.length == 1 ? transformers[0]
      : new ComposableTransformer<>(transformers)));
  }

  /**
   * Composes the {@link Iterable} object of {@link Transformer Transformers} into a composite.
   *
   * @param <T> {@link Class} type of the values transformed by the {@link Transformer Transformers}.
   * @param transformers {@link Iterable} object containing the {@link Transformer Transformers} to compose.
   * @return a composite {@link Transformer} composed of the given {@link Iterable} object
   * of {@link Transformer Transformers}.  Returns {@literal null} if the {@link Iterable} is {@literal null} or empty.
   * Returns a single {@link Transformer} if the {@link Iterable} object contains only a single {@link Transformer},
   * otherwise returns an instance of {@link ComposableTransformer} composed of all {@link Transformer Transformers}
   * in the {@link Iterable} object.
   * @see java.lang.Iterable
   * @see org.cp.elements.lang.Transformer
   * @see #compose(Transformer[])
   */
  @SuppressWarnings("unchecked")
  public static <T> Transformer<T> compose(Iterable<Transformer<T>> transformers) {
    return compose(asArray((Iterable) transformers, Transformer.class));
  }

  /**
   * Iterates over the {@link Transformer Transformers} in this composite.
   *
   * @return an {@link Iterator} over the {@link Transformer Transformers} in this composite.
   * @see java.lang.Iterable#iterator()
   */
  @NullSafe
  public Iterator<Transformer<T>> iterator() {
    return CollectionUtils.nullSafeIterable(transformers).iterator();
  }

  /**
   * Transforms the given value of {@link Class} type T with the {@link Transformer Transformers}.
   *
   * @param value {@link Object} value to transform.
   * @return the transformed value.
   * @see #iterator()
   */
  public T transform(T value) {
    for (Transformer<T> transformer : this) {
      value = transformer.transform(value);
    }

    return value;
  }
}
