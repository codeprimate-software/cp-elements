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

import java.util.Iterator;
import java.util.Optional;

import org.cp.elements.lang.annotation.NullSafe;

/**
 * The {@link Registry} interface defines a contract for implementing {@link Object objects} that can register
 * and unregister other interesting {@link Object Objects}.
 *
 * @author John Blum
 * @see java.lang.Iterable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Registry<T> extends Iterable<T> {

  /**
   * Determines whether the {@link Object} is registered with this {@link Registry}.
   *
   * @param obj {@link Object} to evaluate.
   * @return a boolean value indicating whether the {@link Object} is registered with this {@link Registry}.
   * @see #register(Object)
   */
  @NullSafe
  default boolean isRegistered(T obj) {

    return Optional.ofNullable(obj)
      .map(it -> {

        for (T registeredObject : this) {
          if (it.equals(registeredObject)) {
            return true;
          }
        }

        return false;

      })
      .orElse(false);
  }

  /**
   * Registers the {@link Object} with this {@link Registry}.
   *
   * @param <R> {@link Class concrete type} of this {@link Registry} implementation.
   * @param obj {@link Object} to register.
   * @return this {@link Registry}.
   * @see #unregister(Object)
   */
  <R extends Registry<T>> R register(T obj);

  /**
   * Unregisters the {@link Object} from this {@link Registry}.
   *
   * @param <R> {@link Class concrete type} of this {@link Registry} implementation.
   * @param obj {@link Object} to unregister.
   * @return this {@link Registry}.
   * @see #register(Object)
   */
  @NullSafe
  @SuppressWarnings("unchecked")
  default <R extends Registry<T>> R unregister(T obj) {

    Iterator<T> registeredObjects = this.iterator();

    while (registeredObjects.hasNext()) {
      if (registeredObjects.next().equals(obj)) {
        registeredObjects.remove();
      }
    }

    return (R) this;
  }
}
