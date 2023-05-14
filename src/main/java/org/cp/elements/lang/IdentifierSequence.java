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

import java.util.Optional;

import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Java {@link FunctionalInterface} defining a contract for implementing objects
 * used to generate unique identifiers (IDs) for uniquely identify some {@link Object} or {@literal entity}.
 *
 * @author John J. Blum
 * @param <T> {@link Comparable} {@link Class type} of the {@literal identifying value}.
 * @see java.lang.Comparable
 * @see java.lang.FunctionalInterface
 * @see org.cp.elements.lang.Identifiable
 * @since 1.0.0
 */
@FunctionalInterface
@SuppressWarnings("unused")
public interface IdentifierSequence<T extends Comparable<T>> {

  /**
   * Generates the next {@link T unique ID} in sequence.
   *
   * @return the next {@link T unique ID} of {@link Class type T} in the sequence.
   */
  T nextId();

  /**
   * Sets the {@link T identifier} for the given {@link Identifiable object} if not set.
   *
   * @param <S> {@link Class type} of {@link Identifiable object}.
   * @param identifiable {@link Identifiable object} to identify (set the ID).
   * @return the given {@link Identifiable} object.
   * @see org.cp.elements.lang.Identifiable
   * @see #nextId()
   */
  @NullSafe
  default @Nullable <S extends Identifiable<T>> S identify(@Nullable S identifiable) {

    return Optional.ofNullable(identifiable)
      .filter(Identifiable::isNew)
      .map(it -> it.<S>identifiedBy(nextId()))
      .orElse(identifiable);
  }
}
