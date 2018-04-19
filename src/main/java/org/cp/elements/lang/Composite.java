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

import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.util.ArrayUtils;
import org.cp.elements.util.CollectionUtils;

/**
 * The {@link Composite} interface defines a contract for objects implementing the Composite Software Design Pattern.
 *
 * @author John Blum
 * @see java.lang.FunctionalInterface
 * @see <a href="https://en.wikipedia.org/wiki/Composite_pattern">Compsite Software Design Pattern</a>
 * @since 1.0.0
 */
@FunctionalInterface
@SuppressWarnings("unused")
public interface Composite<T> {

  /**
   * Composes two {@link Object objects} of type {@link T} into a single {@link Composite} object of type {@link T}.
   *
   * @param one {@link Object} to compose.
   * @param two {@link Object} to compose.
   * @return an {@link Object} of type {@link T} composed of the two given {@link Object objects},
   * both of type {@link T}.
   */
  T compose(T one, T two);

  /**
   * Composes the array of {@link Object objects} of type {@link T} into a single {@link Composite}
   * object of type {@link T} acting like a single {@link Object} of type {@link T}.
   *
   * @param objects array of {@link Object objects} to compose.
   * @return an {@link Object} of type {@link T} composed of the array of {@link Object objects} of type {@link T}.
   * Returns {@literal null} if the array is {@literal null} or empty.
   * @see #compose(Object, Object)
   */
  @NullSafe
  @SuppressWarnings({ "unchecked", "varargs" })
  default T compose(T... objects) {

    T composite = null;

    for (T object : ArrayUtils.nullSafeArray(objects)) {
      composite = compose(composite, object);
    }

    return composite;
  }

  /**
   * Composes an {@link Iterable} collection of {@link Object objects} of type {@link T} into a single
   * {@link Composite} object of type {@link T} acting like a single {@link Object} of type {@link T}.
   *
   * @param iterable {@link Iterable} collection of objects to compose.
   * @return an {@link Object} of type {@link T} composed of the {@link Iterable} collection of {@link Object objects}
   * of type {@link T}. Returns {@literal null} if the {@link Iterable} collection of {@link Object objects}
   * is {@literal null} or empty.
   * @see #compose(Object, Object)
   */
  @NullSafe
  default T compose(Iterable<T> iterable) {

    T composite = null;

    for (T object : CollectionUtils.nullSafeIterable(iterable)) {
      composite = compose(composite, object);
    }

    return composite;
  }
}
