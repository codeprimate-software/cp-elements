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

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;

import org.cp.elements.lang.annotation.NotNull;

/**
 * The {@link Auditable} interface defines a contract for {@link Object Objects} that need to be audited,
 * enabling changes to be tracked in fine-grained detail by capturing who, when and what made changes to
 * {@literal this} {@link Object}.
 *
 * @author John J. Blum
 * @param <USER> {@link Class type} used to track the user.
 * @param <PROCESS> {@link Class type} used track the process.
 * @param <ID> {@link Class type} used as the {@link Identifiable} object's identifier.
 * @see java.lang.Comparable
 * @see java.time.Instant
 * @see java.time.LocalDateTime
 * @see java.time.ZonedDateTime
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
   * @return a {@link Instant} capturing the date and time when this object was created.
   * @see java.time.Instant
   */
  Instant getCreatedOn();

  /**
   * Sets the date and time when this object was created.
   *
   * @param createdOn {@link Instant} capturing the date and time when this object was created.
   * @see java.time.Instant
   */
  void setCreatedOn(Instant createdOn);

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
   * @return a {@link Instant} capturing the date and time when this object was last modified.
   * @see java.time.Instant
   */
  Instant getLastModifiedOn();

  /**
   * Gets the last process (application) used by the user to modify this object.
   *
   * @return an object representing the last process (application) used by the user to modify this object.
   */
  PROCESS getLastModifiedWith();

  /**
   * Determines whether this object has been modified.
   *
   * One particular implementation suggests that if the last modified date and time does not match
   * the current modified date and time, then this {@link Auditable} object has been modified concurrently.
   * Of course, if any property value of this object has changed, then the object has been modified.
   *
   * @return a boolean value indicating whether this object has been modified or not.
   * @see #isModified(String)
   */
  boolean isModified();

  /**
   * Determines whether the specified property of this object has been modified.
   *
   * The property has been changed if the old value is not equal in valud to the new value.
   *
   * @param propertyName {@link String} specifying the name of the property to check for modification.
   * @return a boolean value indicating whether the specified property of {@literal this} {@link Auditable} object,
   * identified by name, has been modified.
   * @see #isModified()
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
   * @return a {@link Instant} capturing the date and time when this object was modified.
   * @see java.time.Instant
   */
  Instant getModifiedOn();

  /**
   * Sets the date and time when this object was modified.
   *
   * @param modifiedOn {@link Instant} capturing the date and time when this object was modified.
   * @see java.time.Instant
   */
  void setModifiedOn(Instant modifiedOn);

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
   * Builder method used to set the user responsible for creating this object.
   *
   * @param <S> {@link Class Subclass type} of {@literal this} {@link Object}
   * implementing the {@link Auditable} interface.
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
   * Builder method used to set the date and time when this object was created.
   *
   * @param <S> {@link Class Subclass type} of {@literal this} {@link Object}
   * implementing the {@link Auditable} interface.
   * @param createdOn {@link Instant} capturing the creation date/time.
   * @return this {@link Auditable} object.
   * @see #setCreatedOn(Instant)
   * @see java.time.Instant
   */
  @SuppressWarnings("unchecked")
  default <S extends Auditable<USER, PROCESS, ID>> S createdOn(Instant createdOn) {
    setCreatedOn(createdOn);
    return (S) this;
  }

  /**
   * Builder method used to set the date and time when this object was created.
   *
   * @param <S> {@link Class Subclass type} of {@literal this} {@link Object}
   * implementing the {@link Auditable} interface.
   * @param createdOn {@link LocalDateTime} capturing the creation date/time.
   * @return this {@link Auditable} object.
   * @see #createdOn(ZonedDateTime)
   * @see java.time.LocalDateTime
   */
  default <S extends Auditable<USER, PROCESS, ID>> S createdOn(@NotNull LocalDateTime createdOn) {
    Assert.notNull(createdOn, "createdOn LocalDateTime is required");
    return createdOn(ZonedDateTime.of(createdOn, ZoneId.systemDefault()));
  }

  /**
   * Builder method used to set the date and time when this object was created.
   *
   * @param <S> {@link Class Subclass type} of {@literal this} {@link Object}
   * implementing the {@link Auditable} interface.
   * @param createdOn {@link ZonedDateTime} capturing the creation date/time.
   * @return this {@link Auditable} object.
   * @see #createdOn(Instant)
   * @see java.time.ZonedDateTime
   */
  default <S extends Auditable<USER, PROCESS, ID>> S createdOn(@NotNull ZonedDateTime createdOn) {
    Assert.notNull(createdOn, "createdOn ZonedDateTime is required");
    return createdOn(createdOn.toInstant());
  }

  /**
   * Builder method used to set the process (application) used by the user to create this object.
   *
   * @param <S> {@link Class Subclass type} of {@literal this} {@link Object}
   * implementing the {@link Auditable} interface.
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
   * Builder method used to set the user responsible for modifying this object.
   *
   * @param <S> {@link Class Subclass type} of {@literal this} {@link Object}
   * implementing the {@link Auditable} interface.
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
   * Builder method used to set the date and time when this object was modified.
   *
   * @param <S> {@link Class Subclass type} of {@literal this} {@link Object}
   * implementing the {@link Auditable} interface.
   * @param modifiedOn {@link Instant} capturing the modification date and time.
   * @return this {@link Auditable} object.
   * @see #setModifiedOn(Instant)
   * @see java.time.Instant
   */
  @SuppressWarnings("unchecked")
  default <S extends Auditable<USER, PROCESS, ID>> S modifiedOn(Instant modifiedOn) {
    setModifiedOn(modifiedOn);
    return (S) this;
  }

  /**
   * Builder method used to set the date and time when this object was modified.
   *
   * @param <S> {@link Class Subclass type} of {@literal this} {@link Object}
   * implementing the {@link Auditable} interface.
   * @param modifiedOn {@link LocalDateTime} capturing the modification date and time.
   * @return this {@link Auditable} object.
   * @see #modifiedOn(ZonedDateTime)
   * @see java.time.LocalDateTime
   */
  default <S extends Auditable<USER, PROCESS, ID>> S modifiedOn(@NotNull LocalDateTime modifiedOn) {
    Assert.notNull(modifiedOn, "modifiedOn LocalDateTime is required");
    return modifiedOn(ZonedDateTime.of(modifiedOn, ZoneId.systemDefault()));
  }

  /**
   * Builder method used to set the date and time when this object was modified.
   *
   * @param <S> {@link Class Subclass type} of {@literal this} {@link Object}
   * implementing the {@link Auditable} interface.
   * @param modifiedOn {@link ZonedDateTime} capturing the modification date and time.
   * @return this {@link Auditable} object.
   * @see #modifiedOn(Instant)
   * @see java.time.ZonedDateTime
   */
  default <S extends Auditable<USER, PROCESS, ID>> S modifiedOn(@NotNull ZonedDateTime modifiedOn) {
    Assert.notNull(modifiedOn, "modifiedOn ZonedDateTime is required");
    return modifiedOn(modifiedOn.toInstant());
  }

  /**
   * Builder method used to set the process (application) used by the user to modify this object.
   *
   * @param <S> {@link Class Subclass type} of {@literal this} {@link Object}
   * implementing the {@link Auditable} interface.
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
