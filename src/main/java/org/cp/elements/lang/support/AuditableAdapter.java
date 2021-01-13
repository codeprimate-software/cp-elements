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

import java.time.LocalDateTime;

import org.cp.elements.lang.Auditable;
import org.cp.elements.lang.Constants;

/**
 * The AuditableAdapter class is abstract base class provided for convenience when implementing the Auditable interface.
 *
 * @author John J. Blum
 * @param <USER> an object type for tracking the user.
 * @param <PROCESS> an object type for tracking the process.
 * @see java.time.LocalDateTime
 * @see org.cp.elements.lang.Auditable
 * @see org.cp.elements.lang.support.IdentifiableAdapter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AuditableAdapter<USER, PROCESS, ID extends Comparable<ID>> extends IdentifiableAdapter<ID>
    implements Auditable<USER, PROCESS, ID> {

  /**
   * Gets the user responsible for creating this object.
   *
   * @return an object representing the user who created this object.
   */
  @Override
  public USER getCreatedBy() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Sets the user responsible for creating this object.
   *
   * @param user object representing the user who created this object.
   */
  @Override
  public void setCreatedBy(USER user) {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Gets the date and time when this object was created.
   *
   * @return a {@link LocalDateTime} capturing the date and time when this object was created.
   */
  @Override
  public LocalDateTime getCreatedOn() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Sets the date and time when this object was created.
   *
   * @param createdOn {@link LocalDateTime} capturing the date and time when this object was created.
   */
  @Override
  public void setCreatedOn(LocalDateTime createdOn) {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Gets the process (application) used by the user to create this object.
   *
   * @return an object representing the process (application) used by the user to create this object.
   */
  @Override
  public PROCESS getCreatedWith() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Sets the process (application) used by the user to create this object.
   *
   * @param process object representing the process (application) used by the user to create this object.
   */
  @Override
  public void setCreatedWith(PROCESS process) {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Gets the last user who modified this object.
   *
   * @return an object representing the last user responsible for modifying this object.
   */
  @Override
  public USER getLastModifiedBy() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Gets the date and time when this object was last modified.
   *
   * @return a {@link LocalDateTime} capturing the date and time when this object was last modified.
   */
  @Override
  public LocalDateTime getLastModifiedOn() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Gets the last process (application) used by the user to modify this object.
   *
   * @return an object representing the last process (application) used by the user to modify this object.
   */
  @Override
  public PROCESS getLastModifiedWith() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Determines whether this Auditable object has been modified.  One particular implementation suggests that
   * if the last modified date/time does not match the current modified date/time then the Auditable object has
   * been modified.  Of course, if any propery value of the object has changed, then the object has been modified.
   *
   * @return a boolean value indicating whether this Auditable object has been modified or not.
   */
  @Override
  public boolean isModified() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Determines whether the specified property of this Auditable object has been modified.  The property has been
   * changed if the old and new value are not equal in value.
   *
   * @param propertyName a String value specifying the name of the property to check for modification.
   * @return a boolean value indicating whether the specified property of this Auditable object, identified by name,
   * has been modified.
   */
  @Override
  public boolean isModified(String propertyName) {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Gets the user responsible for modifying this object.
   *
   * @return an object representing the user responsible for modifying this object.
   */
  @Override
  public USER getModifiedBy() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Sets the user responsible for modifying this object.
   *
   * @param user object representing the user responsible for modifying this object.
   */
  @Override
  public void setModifiedBy(USER user) {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Gets the date and time when this object was modified.
   *
   * @return a {@link LocalDateTime} capturing the date and time when this object was modified.
   */
  @Override
  public LocalDateTime getModifiedOn() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Sets the date and time when this object was modified.
   *
   * @param modifiedOn {@link LocalDateTime} capturing the date and time when this object was modified.
   */
  @Override
  public void setModifiedOn(LocalDateTime modifiedOn) {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Gets the process (application) used by the user to modify this object.
   *
   * @return an object representing the process (application) used by the user to modify this object.
   */
  @Override
  public PROCESS getModifiedWith() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Sets the process (application) used by the user to modify this object.
   *
   * @param process object representing the process (application) used by the user to modify this object.
   */
  @Override
  public void setModifiedWith(PROCESS process) {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }
}
