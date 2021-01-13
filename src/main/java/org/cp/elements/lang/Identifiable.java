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
 * The {@link Identifiable} interface defines a contract for uniquely identifying objects of a given type.
 *
 * @author John J. Blum
 * @see java.lang.Comparable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Identifiable<T extends Comparable<T>> {

  /**
   * Returns the identifier uniquely identifying this object.
   *
   * @return the value uniquely identifying this object.
   */
  T getId();

  /**
   * Sets the identifier uniquely identifying this object.
   *
   * @param id value of type T assigned as this object's unique identifier.
   */
  void setId(T id);

  /**
   * Determines whether this {@link Identifiable} object is new indicated with a {@literal null} identifier.
   *
   * @return a boolean value indicating whether this {@link Identifiable} object is new.
   */
  default boolean isNew() {
    return getId() == null;
  }

  /**
   * Determines whether this {@link Identifiable} object is non-new indicated with a non-{@literal null} identifier.
   *
   * @return a boolean value indicating whether this {@link Identifiable} object is non-new.
   * @see #isNew()
   */
  default boolean isNotNew() {
    return !isNew();
  }

  /**
   * Builder method used to set this {@link Identifiable} object's id.
   *
   * @param <S> Subclass type of this {@link Identifiable} object.
   * @param id value of type T assigned as this object's unique identifier.
   * @return a reference to this {@link Identifiable}.
   * @see #setId(Comparable)
   */
  @SuppressWarnings("unchecked")
  default <S extends Identifiable<T>> S identifiedBy(T id) {
    setId(id);
    return (S) this;
  }
}
