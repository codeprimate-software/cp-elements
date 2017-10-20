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

import java.util.Optional;

import org.cp.elements.lang.annotation.NullSafe;

/**
 * The {@link IdentifierSequence} interface defines a contract for implementing objects to generate
 * unique identifiers (IDs) to uniquely identify some object or entity.
 *
 * @author John J. Blum
 * @param <T> is a {@link Comparable} class type of the identifying value.
 * @see java.lang.Comparable
 * @see java.lang.FunctionalInterface
 * @see org.cp.elements.lang.Identifiable
 * @since 1.0.0
 */
@FunctionalInterface
@SuppressWarnings("unused")
public interface IdentifierSequence<T extends Comparable<T>> {

  /**
   * Generates the next unique ID in sequence.
   *
   * @return the next unique ID of class type T in the sequence.
   */
  T nextId();

  /**
   * Sets the identifier of the given {@link Identifiable} object is not set.
   *
   * @param <S> {@link Class} type of the {@link Identifiable} object.
   * @param identifiable {@link Identifiable} object to identify (set the ID).
   * @return the given {@link Identifiable} object.
   * @see org.cp.elements.lang.Identifiable
   */
  @NullSafe
  default <S extends Identifiable<T>> S identify(S identifiable) {

    return Optional.ofNullable(identifiable)
      .filter(Identifiable::isNew)
      .map(it -> it.<S>identifiedBy(nextId()))
      .orElse(identifiable);
  }
}
