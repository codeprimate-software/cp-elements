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

/**
 * The {@link Verifyable} interface defines a contract for implementing {@link Object objects} providing a method
 * to validate the soft constraints and invariants (e.g. business rules) of this implementing {@link Object object}
 * are upheld.
 *
 * @author John Blum
 * @param <T> {@link Class} type of the {@link Object} implementing this interface.
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Verifyable<T> {

  /**
   * Determines whether this {@link Verifyable} object is valid.
   *
   * @return a boolean value indicating whether this {@link Verifyable} object is valid.
   * @see #validate()
   */
  default boolean isValid() {
    try {
      return (validate() != null);
    }
    catch (RuntimeException ignore) {
      return false;
    }
  }

  /**
   * Validates the constraints and invariants of this implementing {@link Object}.
   *
   * @return this implementing {@link Verifyable} {@link Object}.
   */
  T validate();

}
