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

package org.cp.elements.lang.support;

import java.util.Calendar;

import org.cp.elements.lang.Auditable;
import org.cp.elements.lang.Constants;

/**
 * The AuditableAdapter class is abstract base class provided for convenience when implementing the Auditable interface.
 *
 * @author John J. Blum
 * @param <USER> an object type for tracking the user.
 * @param <PROCESS> an object type for tracking the process.
 * @see java.util.Calendar
 * @see org.cp.elements.lang.Auditable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AuditableAdapter<USER, PROCESS> implements Auditable<USER, PROCESS> {

  /**
   * Gets the user who is responsible for the creation of this object.
   *
   * @return an object denoting the user who created this object.
   */
  @Override
  public USER getCreatedBy() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Sets the user who is responsible for the creation of this object.
   *
   * @param user an object denoting the user who created this object.
   */
  @Override
  public void setCreatedBy(final USER user) {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Gets the date and time when this object was created.
   *
   * @return a Calendar object denoting the date and time when this object was created.
   */
  @Override
  public Calendar getCreatedDateTime() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Sets the date and time when this object was created.
   *
   * @param dateTime a Calendar object denoting the date and time when this object was created.
   */
  @Override
  public void setCreatedDateTime(final Calendar dateTime) {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Gets the process (the what) that functionally created this object.
   *
   * @return an object denoting the process that created this object.
   */
  @Override
  public PROCESS getCreatingProcess() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Sets the process (the what) that functionally created this object.
   *
   * @param process an object denoting the process that created this object.
   */
  @Override
  public void setCreatingProcess(final PROCESS process) {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Gets the user who was last responsible for modifying this object.
   *
   * @return an object denoting the last user responsible for modifying this object.
   */
  @Override
  public USER getLastModifiedBy() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Gets the last date and time when this object was modified.
   *
   * @return a Calendar object denoting the date and time when this object was last modified.
   */
  @Override
  public Calendar getLastModifiedDateTime() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Gets the process (the what) that was last responsible for modifying this object.
   *
   * @return an object denoting the last process responsible for modifying this object.
   */
  @Override
  public PROCESS getLastModifyingProcess() {
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
  public boolean isModified(final String propertyName) {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Gets the user who is responsible for modifying this object.
   *
   * @return an object denoting the user who modified this object.
   */
  @Override
  public USER getModifiedBy() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Sets the user who is responsible for modifying this object.
   *
   * @param user an object denoting the user who modified this object.
   */
  @Override
  public void setModifiedBy(final USER user) {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Gets the date and time when this object was modified.
   *
   * @return a Calendar object denoting the date and time when this object was modified.
   */
  @Override
  public Calendar getModifiedDateTime() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Sets the date and time when this object was modified.
   *
   * @param dateTime a Calendar object denoting the date and time when this object was modified.
   */
  @Override
  public void setModifiedDateTime(final Calendar dateTime) {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Gets the process (the what) that functionally modified this object.
   *
   * @return an object denoting the process that modified this object.
   */
  @Override
  public PROCESS getModifyingProcess() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Sets the process (the what) that functionally modified this object.
   *
   * @param process an object denoting the process that modified this object.
   */
  @Override
  public void setModifyingProcess(final PROCESS process) {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

}
