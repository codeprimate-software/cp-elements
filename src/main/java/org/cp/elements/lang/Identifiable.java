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

import static org.cp.elements.lang.RuntimeExceptionsFactory.newUnsupportedOperationException;

/**
 * Java {@link FunctionalInterface} defining a contract for uniquely identifying {@link Object objects}
 * of a given {@link Class type}.
 *
 * @author John J. Blum
 * @param <ID> {@link Comparable} {@link Class type} of the {@link Object identifier}.
 * @see java.lang.Comparable
 * @see org.cp.elements.lang.ImmutableIdentifiable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
@FunctionalInterface
public interface Identifiable<ID extends Comparable<ID>> extends ImmutableIdentifiable<ID> {

  /**
   * Sets the {@link ID identifier} uniquely identifying {@literal this} {@link Object}.
   *
   * @param id {@link ID value} of {@link Class type ID} assigned as {@literal this} {@link Object Object's}
   * unique identifier.
   * @throws UnsupportedOperationException by default.
   * @see #getId()
   */
  default void setId(ID id) {
    throw newUnsupportedOperationException("Setting ID is not supported");
  }

  /**
   * Determines whether {@literal this} {@link Identifiable} object is {@literal new}.
   *
   * This {@link Object} is considered {@literal new} if the identifier is {@literal null}.
   *
   * @return a boolean value indicating whether {@literal this} {@link Identifiable} object is {@literal new}.
   * @see #isNotNew()
   */
  default boolean isNew() {
    return getId() == null;
  }

  /**
   * Determines whether {@literal this} {@link Identifiable} object is {@literal not new}.
   *
   * This {@link Object} is considered {@literal not new} if the identifier is not {@literal null}.
   *
   * @return a boolean value indicating whether {@literal this} {@link Identifiable} object is {@literal not new}.
   * @see #isNew()
   */
  default boolean isNotNew() {
    return !isNew();
  }

  /**
   * Builder method used to set this {@link Identifiable} object's identifier (ID).
   *
   * @param <IDX> {@link Class Subclass type} of {@literal this} {@link Identifiable} object.
   * @param id value of {@link Class type ID} assigned as this {@link Identifiable} object's unique identifier.
   * @return {@literal this} {@link Identifiable} object.
   * @see #setId(Comparable)
   */
  @SuppressWarnings("unchecked")
  default <IDX extends Identifiable<ID>> IDX identifiedBy(ID id) {
    setId(id);
    return (IDX) this;
  }
}
