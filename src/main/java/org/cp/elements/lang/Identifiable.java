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
 * The Identifiable interface defines a contract for uniquely identifying objects of the specified type.
 *
 * @author John J. Blum
 * @see java.lang.Comparable
 * @see org.cp.elements.lang.Auditable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Identifiable<T extends Comparable<T>> {

  /**
   * Gets the identifier uniquely identifying this object.
   *
   * @return the value of type T indicating this object's assigned unique identifier.
   */
  T getId();

  /**
   * Sets the identifier uniquely identifying this object.
   *
   * @param id a value of type T assigned as this object's unique identifier.
   */
  void setId(T id);

  /**
   * Determines whether this Identifiable object is new, which is signified by a null identifier.
   *
   * @return a boolean value indicating whether this Identifiable object is new.
   */
  boolean isNew();

}
