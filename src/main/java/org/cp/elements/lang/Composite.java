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
package org.cp.elements.lang;

import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.util.ArrayUtils;
import org.cp.elements.util.CollectionUtils;

/**
 * Interface defining a contract for {@link Object objects} whose {@link Class types}
 * implement the {@literal Composite Software Design Pattern}.
 *
 * @author John Blum
 * @param <T> {@link Class type} of {@link Object} to {@literal compose}.
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
   * @return an {@link Object} of type {@link T} composed of the two given, non-required {@link Object objects},
   * of type {@link T}.
   */
  T compose(T one, T two);

  /**
   * Composes the array of {@link Object objects} of type {@link T} into a single {@link Composite}
   * object of type {@link T} acting as a single {@link Object} of type {@link T}.
   *
   * @param array array of {@link Object objects} to compose.
   * @return an {@link Object} of type {@link T} composed of the array of {@link Object objects} of type {@link T}.
   * Returns {@literal null} if the array is {@literal null} or {@literal empty}.
   * @see #compose(Object, Object)
   */
  @NullSafe
  @SuppressWarnings({ "unchecked", "varargs" })
  default @Nullable T compose(T... array) {

    T composite = null;

    for (T object : ArrayUtils.nullSafeArray(array)) {
      composite = compose(composite, object);
    }

    return composite;
  }

  /**
   * Composes an {@link Iterable} collection of {@link Object objects} of type {@link T} into a single
   * {@link Composite} object of type {@link T} acting as a single {@link Object} of type {@link T}.
   *
   * @param iterable {@link Iterable} collection of {@link Object objects} to compose.
   * @return an {@link Object} of type {@link T} composed of the {@link Iterable} collection of {@link Object objects}
   * of type {@link T}. Returns {@literal null} if the {@link Iterable} collection of {@link Object objects}
   * is {@literal null} or {@literal empty}.
   * @see #compose(Object, Object)
   */
  @NullSafe
  default @Nullable T compose(@Nullable Iterable<T> iterable) {

    T composite = null;

    for (T object : CollectionUtils.nullSafeIterable(iterable)) {
      composite = compose(composite, object);
    }

    return composite;
  }
}
