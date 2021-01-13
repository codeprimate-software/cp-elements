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

package org.cp.elements.lang.support;

import static org.cp.elements.lang.ObjectUtils.defaultIfNull;

import java.time.LocalDateTime;
import java.util.Optional;
import java.util.function.Supplier;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Auditable;

/**
 * {@link AuditableSupport} is an abstract {@link Class} supporting implementations of the {@link Auditable}
 * {@link Class interface}.
 *
 * @author John Blum
 * @see java.time.LocalDateTime
 * @see org.cp.elements.lang.Auditable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AuditableSupport<USER, PROCESS, ID extends Comparable<ID>> extends IdentifiableSupport<ID>
    implements Auditable<USER, PROCESS, ID>  {

  private static final String USER_NAME_SYSTEM_PROPERTY = "user.name";

  private LocalDateTime createdOn;
  private LocalDateTime lastModifiedOn;
  private LocalDateTime modifiedOn;

  private PROCESS createdWith;
  private PROCESS lastModifiedWith;
  private PROCESS modifiedWith;

  private USER createdBy;
  private USER lastModifiedBy;
  private USER modifiedBy;

  /**
   * Assert the given {@link Object value} is not {@literal null}.
   *
   * @param <T> {@link Class type} of the {@link Object value}.
   * @param value {@link Object} to evaluate.
   * @param message {@link Supplier} containing the message used in the {@link IllegalArgumentException}.
   * @return the given {@link Object value} if not {@literal null}.
   * @throws IllegalArgumentException with {@link Supplier message} if the {@link Object value} is {@literal null}.
   * @see java.util.function.Supplier
   */
  private <T> T assertNotNull(T value, Supplier<String> message) {
    Assert.notNull(value, message);
    return value;
  }

  /**
   * Gets the user responsible for creating this object.
   *
   * @return an object representing the user who created this object.
   */
  @Override
  public USER getCreatedBy() {
    return this.createdBy;
  }

  /**
   * Sets the user responsible for creating this object.
   *
   * @param createdBy object representing the user who created this object.
   */
  @Override
  public void setCreatedBy(USER createdBy) {
    this.createdBy = assertNotNull(createdBy, () -> "Created by is required");
  }

  /**
   * Gets the date and time when this object was created.
   *
   * @return a {@link LocalDateTime} capturing the date and time when this object was created.
   */
  @Override
  public LocalDateTime getCreatedOn() {
    return this.createdOn;
  }

  /**
   * Sets the date and time when this object was created.
   *
   * @param createdOn {@link LocalDateTime} capturing the date and time when this object was created.
   */
  @Override
  public void setCreatedOn(LocalDateTime createdOn) {
    this.createdOn = assertNotNull(createdOn, () -> "Created on is required");
  }

  /**
   * Gets the process (application) used by the user to create this object.
   *
   * @return an object representing the process (application) used by the user to create this object.
   */
  @Override
  public PROCESS getCreatedWith() {
    return this.createdWith;
  }

  /**
   * Sets the process (application) used by the user to create this object.
   *
   * @param createdWith object representing the process (application) used by the user to create this object.
   */
  @Override
  public void setCreatedWith(PROCESS createdWith) {
    this.createdWith = assertNotNull(createdWith, () -> "Created with is required");
  }

  /**
   * Gets the last user who modified this object.
   *
   * @return an object representing the last user responsible for modifying this object.
   */
  @Override
  public USER getLastModifiedBy() {
    return this.lastModifiedBy;
  }

  /**
   * Gets the date and time when this object was last modified.
   *
   * @return a {@link LocalDateTime} capturing the date and time when this object was last modified.
   */
  @Override
  public LocalDateTime getLastModifiedOn() {
    return this.lastModifiedOn;
  }

  /**
   * Gets the last process (application) used by the user to modify this object.
   *
   * @return an object representing the last process (application) used by the user to modify this object.
   */
  @Override
  public PROCESS getLastModifiedWith() {
    return this.lastModifiedWith;
  }

  /**
   * Gets the user responsible for modifying this object.
   *
   * @return an object representing the user responsible for modifying this object.
   */
  @Override
  public USER getModifiedBy() {
    return Optional.ofNullable(this.modifiedBy).orElseGet(this::getCreatedBy);
  }

  /**
   * Sets the user responsible for modifying this object.
   *
   * @param modifiedBy object representing the user responsible for modifying this object.
   */
  @Override
  @SuppressWarnings("unchecked")
  public void setModifiedBy(USER modifiedBy) {
    this.modifiedBy = assertNotNull(modifiedBy, () -> "Modified by is required");
    this.lastModifiedBy = defaultIfNull(this.lastModifiedBy, this.modifiedBy);
  }

  /**
   * Gets the date and time when this object was modified.
   *
   * @return a {@link LocalDateTime} capturing the date and time when this object was modified.
   */
  @Override
  public LocalDateTime getModifiedOn() {
    return Optional.ofNullable(this.modifiedOn).orElseGet(this::getCreatedOn);
  }

  /**
   * Sets the date and time when this object was modified.
   *
   * @param modifiedOn {@link LocalDateTime} capturing the date and time when this object was modified.
   */
  @Override
  public void setModifiedOn(LocalDateTime modifiedOn) {
    this.modifiedOn = assertNotNull(modifiedOn, () -> "Modified on is required");
    this.lastModifiedOn = defaultIfNull(this.lastModifiedOn, this.modifiedOn);
  }

  /**
   * Gets the process (application) used by the user to modify this object.
   *
   * @return an object representing the process (application) used by the user to modify this object.
   */
  @Override
  public PROCESS getModifiedWith() {
    return Optional.ofNullable(this.modifiedWith).orElseGet(this::getCreatedWith);
  }

  /**
   * Sets the process (application) used by the user to modify this object.
   *
   * @param modifiedWith object representing the process (application) used by the user to modify this object.
   */
  @Override
  @SuppressWarnings("unchecked")
  public void setModifiedWith(PROCESS modifiedWith) {
    this.modifiedWith = assertNotNull(modifiedWith, () -> "Modified with is required");
    this.lastModifiedWith = defaultIfNull(this.lastModifiedWith, this.modifiedWith);
  }

  /**
   * Returns the {@link String name} of the {@link USER} running the application.
   *
   * @return a {@link String} contain the name of the {@link USER} running the application
   * @see java.lang.System#getProperty(String)
   */
  protected String getUsername() {
    return System.getProperty(USER_NAME_SYSTEM_PROPERTY);
  }
}
