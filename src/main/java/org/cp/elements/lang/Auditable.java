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
 * The Auditable interface defines a contract for objects that need to be audited, allowing all changes to be tracked
 * in fine-grained detail by specifying who, when and what made changes to this object.
 *
 * @author John J. Blum
 * @param <USER> Class type for tracking the user.
 * @param <PROCESS> Class type for tracking the process.
 * @param <ID> Class type of the {@link} Identifiable object's identifier.
 * @see java.lang.Comparable
 * @see java.time.LocalDateTime
 * @see org.cp.elements.lang.Identifiable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Auditable<USER, PROCESS, ID extends Comparable<ID>> extends Identifiable<ID> {

  /**
   * Gets the user who is responsible for creating this object.
   *
   * @return an object denoting the user who created this object.
   */
  USER getCreatedBy();

  /**
   * Sets the user who is responsible for creating this object.
   *
   * @param user object denoting the user who created this object.
   */
  void setCreatedBy(USER user);

  /**
   * Gets the date and time when this object was created.
   *
   * @return a {@link LocalDateTime} denoting the date and time when this object was created.
   */
  LocalDateTime getCreatedOn();

  /**
   * Sets the date and time when this object was created.
   *
   * @param createdOn {@link LocalDateTime} denoting the date and time when this object was created.
   */
  void setCreatedOn(LocalDateTime createdOn);

  /**
   * Gets the process (the what) that functionally created this object.
   *
   * @return an object denoting the process that created this object.
   */
  PROCESS getCreatingProcess();

  /**
   * Sets the process (the what) that functionally created this object.
   *
   * @param process object denoting the process that created this object.
   */
  void setCreatingProcess(PROCESS process);

  /**
   * Gets the user who was last responsible for modifying this object.
   *
   * @return an object denoting the last user responsible for modifying this object.
   */
  USER getLastModifiedBy();

  /**
   * Gets the date and time when this object was last modified.
   *
   * @return a {@link LocalDateTime} denoting the date and time when this object was last modified.
   */
  LocalDateTime getLastModifiedOn();

  /**
   * Gets the process (the what) that was last responsible for modifying this object.
   *
   * @return an object denoting the last process responsible for modifying this object.
   */
  PROCESS getLastModifyingProcess();

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
   * Gets the user who is responsible for modifying this object.
   *
   * @return an object denoting the user who modified this object.
   */
  USER getModifiedBy();

  /**
   * Sets the user who is responsible for modifying this object.
   *
   * @param user object denoting the user who modified this object.
   */
  void setModifiedBy(USER user);

  /**
   * Gets the date and time when this object was modified.
   *
   * @return a {@link LocalDateTime} denoting the date and time when this object was modified.
   */
  LocalDateTime getModifiedOn();

  /**
   * Sets the date and time when this object was modified.
   *
   * @param modifiedOn {@link LocalDateTime} denoting the date and time when this object was modified.
   */
  void setModifiedOn(LocalDateTime modifiedOn);

  /**
   * Gets the process (the what) that functionally modified this object.
   *
   * @return an object denoting the process that modified this object.
   */
  PROCESS getModifyingProcess();

  /**
   * Sets the process (the what) that functionally modified this object.
   *
   * @param process object denoting the process that modified this object.
   */
  void setModifyingProcess(PROCESS process);

  @SuppressWarnings("unchecked")
  default <S extends Auditable<USER, PROCESS, ID>> S createdBy(USER user) {
    setCreatedBy(user);
    return (S) this;
  }

  @SuppressWarnings("unchecked")
  default <S extends Auditable<USER, PROCESS, ID>> S createdOn(LocalDateTime createdOn) {
    setCreatedOn(createdOn);
    return (S) this;
  }

  @SuppressWarnings("unchecked")
  default <S extends Auditable<USER, PROCESS, ID>> S creatingProcess(PROCESS process) {
    setCreatingProcess(process);
    return (S) this;
  }

  @SuppressWarnings("unchecked")
  default <S extends Auditable<USER, PROCESS, ID>> S modifiedBy(USER user) {
    setModifiedBy(user);
    return (S) this;
  }

  @SuppressWarnings("unchecked")
  default <S extends Auditable<USER, PROCESS, ID>> S modifiedOn(LocalDateTime modifiedOn) {
    setModifiedOn(modifiedOn);
    return (S) this;
  }

  @SuppressWarnings("unchecked")
  default <S extends Auditable<USER, PROCESS, ID>> S modifyingProcess(PROCESS process) {
    setModifyingProcess(process);
    return (S) this;
  }
}
