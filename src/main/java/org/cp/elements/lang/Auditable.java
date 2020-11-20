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

import java.time.LocalDateTime;

/**
 * The {@link Auditable} interface defines a contract for objects that need to be audited, enabling changes
 * to be tracked in fine-grained detail capturing who, when and what made changes to this object.
 *
 * @author John J. Blum
 * @param <USER> {@link Class} type for tracking the user.
 * @param <PROCESS> {@link Class} type for tracking the process.
 * @param <ID> {@link Class} type of the {@link Identifiable} object's identifier.
 * @see java.lang.Comparable
 * @see java.time.LocalDateTime
 * @see org.cp.elements.lang.Identifiable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Auditable<USER, PROCESS, ID extends Comparable<ID>> extends Identifiable<ID> {

  /**
   * Gets the user responsible for creating this object.
   *
   * @return an object representing the user who created this object.
   */
  USER getCreatedBy();

  /**
   * Sets the user responsible for creating this object.
   *
   * @param user object representing the user who created this object.
   */
  void setCreatedBy(USER user);

  /**
   * Gets the date and time when this object was created.
   *
   * @return a {@link LocalDateTime} capturing the date and time when this object was created.
   */
  LocalDateTime getCreatedOn();

  /**
   * Sets the date and time when this object was created.
   *
   * @param createdOn {@link LocalDateTime} capturing the date and time when this object was created.
   */
  void setCreatedOn(LocalDateTime createdOn);

  /**
   * Gets the process (application) used by the user to create this object.
   *
   * @return an object representing the process (application) used by the user to create this object.
   */
  PROCESS getCreatedWith();

  /**
   * Sets the process (application) used by the user to create this object.
   *
   * @param process object representing the process (application) used by the user to create this object.
   */
  void setCreatedWith(PROCESS process);

  /**
   * Gets the last user who modified this object.
   *
   * @return an object representing the last user responsible for modifying this object.
   */
  USER getLastModifiedBy();

  /**
   * Gets the date and time when this object was last modified.
   *
   * @return a {@link LocalDateTime} capturing the date and time when this object was last modified.
   */
  LocalDateTime getLastModifiedOn();

  /**
   * Gets the last process (application) used by the user to modify this object.
   *
   * @return an object representing the last process (application) used by the user to modify this object.
   */
  PROCESS getLastModifiedWith();

  /**
   * Determines whether this Auditable object has been modified.  One particular implementation suggests that
   * if the last modified date/time does not match the current modified date/time then the Auditable object has
   * been modified.  Of course, if any propery value of the object has changed, then the object has been modified.
   *
   * @return a boolean value indicating whether this Auditable object has been modified or not.
   */
  boolean isModified();

  /**
   * Determines whether the specified property of this Auditable object has been modified.  The property has been
   * changed if the old and new value are not equal in value.
   *
   * @param propertyName a String value specifying the name of the property to check for modification.
   * @return a boolean value indicating whether the specified property of this Auditable object, identified by name,
   * has been modified.
   */
  boolean isModified(String propertyName);

  /**
   * Gets the user responsible for modifying this object.
   *
   * @return an object representing the user responsible for modifying this object.
   */
  USER getModifiedBy();

  /**
   * Sets the user responsible for modifying this object.
   *
   * @param user object representing the user responsible for modifying this object.
   */
  void setModifiedBy(USER user);

  /**
   * Gets the date and time when this object was modified.
   *
   * @return a {@link LocalDateTime} capturing the date and time when this object was modified.
   */
  LocalDateTime getModifiedOn();

  /**
   * Sets the date and time when this object was modified.
   *
   * @param modifiedOn {@link LocalDateTime} capturing the date and time when this object was modified.
   */
  void setModifiedOn(LocalDateTime modifiedOn);

  /**
   * Gets the process (application) used by the user to modify this object.
   *
   * @return an object representing the process (application) used by the user to modify this object.
   */
  PROCESS getModifiedWith();

  /**
   * Sets the process (application) used by the user to modify this object.
   *
   * @param process object representing the process (application) used by the user to modify this object.
   */
  void setModifiedWith(PROCESS process);

  /**
   * Builder method to set the user responsible for creating this object.
   *
   * @param <S> Subclass type of this object implementing the {@link Auditable} interface.
   * @param user person responsible for creating this object.
   * @return this {@link Auditable} object.
   * @see #setCreatedBy(Object)
   */
  @SuppressWarnings("unchecked")
  default <S extends Auditable<USER, PROCESS, ID>> S createdBy(USER user) {
    setCreatedBy(user);
    return (S) this;
  }

  /**
   * Builder method to set the date/time when this object was created.
   *
   * @param <S> Subclass type of this object implementing the {@link Auditable} interface.
   * @param createdOn {@link LocalDateTime} capturing the creation date/time.
   * @return this {@link Auditable} object.
   * @see #setCreatedOn(LocalDateTime)
   */
  @SuppressWarnings("unchecked")
  default <S extends Auditable<USER, PROCESS, ID>> S createdOn(LocalDateTime createdOn) {
    setCreatedOn(createdOn);
    return (S) this;
  }

  /**
   * Builder method to set the process (application) used by the user to create this object.
   *
   * @param <S> Subclass type of this object implementing the {@link Auditable} interface.
   * @param process application used by the user to create this object.
   * @return this {@link Auditable} object.
   * @see #setCreatedWith(Object)
   */
  @SuppressWarnings("unchecked")
  default <S extends Auditable<USER, PROCESS, ID>> S createdWith(PROCESS process) {
    setCreatedWith(process);
    return (S) this;
  }

  /**
   * Builder method to set the user responsible for modifying this object.
   *
   * @param <S> Subclass type of this object implementing the {@link Auditable} interface.
   * @param user person responsible for modifying this object.
   * @return this {@link Auditable} object.
   * @see #setModifiedBy(Object)
   */
  @SuppressWarnings("unchecked")
  default <S extends Auditable<USER, PROCESS, ID>> S modifiedBy(USER user) {
    setModifiedBy(user);
    return (S) this;
  }

  /**
   * Builder method to set the date and time when this object was modified.
   *
   * @param <S> Subclass type of this object implementing the {@link Auditable} interface.
   * @param modifiedOn {@link LocalDateTime} capturing the modification date/time.
   * @return this {@link Auditable} object.
   * @see #setModifiedOn(LocalDateTime)
   */
  @SuppressWarnings("unchecked")
  default <S extends Auditable<USER, PROCESS, ID>> S modifiedOn(LocalDateTime modifiedOn) {
    setModifiedOn(modifiedOn);
    return (S) this;
  }

  /**
   * Builder method to set the process (application) used by the user to modify this object.
   *
   * @param <S> Subclass type of this object implementing the {@link Auditable} interface.
   * @param process application used by the user to modify this object.
   * @return this {@link Auditable} object.
   * @see #setModifiedWith(Object)
   */
  @SuppressWarnings("unchecked")
  default <S extends Auditable<USER, PROCESS, ID>> S modifiedWith(PROCESS process) {
    setModifiedWith(process);
    return (S) this;
  }
}
