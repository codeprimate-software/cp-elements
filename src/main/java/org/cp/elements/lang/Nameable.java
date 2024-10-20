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

/**
 * Interface defining a contract for {@link Object objects} that have a name.
 *
 * @author John Blum
 * @param <T> {@link Class type} of the {@link Object} used to name this {@link Object}.
 * @see java.lang.FunctionalInterface
 * @since 1.0.0
 */
@FunctionalInterface
@SuppressWarnings("unused")
public interface Nameable<T> {

  /**
   * Factory method used to construct a new {@link Nameable} object initialized with
   * the given, required {@link T name}.
   *
   * @param <T> {@link Class type} of the name.
   * @param name {@link T name} of the {@link Nameable object}.
   * @return a new {@link Nameable object} initialized with the given, required {@link T name}.
   */
  static <T> Nameable<T> named(T name) {
    Assert.notNull(name, "Name is required");
    return () -> name;
  }

  /**
   * Returns the name of this object.
   *
   * @return the name of this object.
   */
  T getName();

}
