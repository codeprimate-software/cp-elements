/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.lang.support;

import java.util.Calendar;

import org.cp.elements.lang.Auditable;
import org.cp.elements.lang.Constants;

/**
 * The AuditableAdapter class is abstract base class provided for convenience when implementing the Auditable interface.
 * <p/>
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
   * <p/>
   * @return an object denoting the user who created this object.
   */
  @Override
  public USER getCreatedBy() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Sets the user who is responsible for the creation of this object.
   * <p/>
   * @param user an object denoting the user who created this object.
   */
  @Override
  public void setCreatedBy(final USER user) {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Gets the date and time when this object was created.
   * <p/>
   * @return a Calendar object denoting the date and time when this object was created.
   */
  @Override
  public Calendar getCreatedDateTime() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Sets the date and time when this object was created.
   * <p/>
   * @param dateTime a Calendar object denoting the date and time when this object was created.
   */
  @Override
  public void setCreatedDateTime(final Calendar dateTime) {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Gets the process (the what) that functionally created this object.
   * <p/>
   * @return an object denoting the process that created this object.
   */
  @Override
  public PROCESS getCreatingProcess() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Sets the process (the what) that functionally created this object.
   * <p/>
   * @param process an object denoting the process that created this object.
   */
  @Override
  public void setCreatingProcess(final PROCESS process) {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Gets the user who was last responsible for modifying this object.
   * <p/>
   * @return an object denoting the last user responsible for modifying this object.
   */
  @Override
  public USER getLastModifiedBy() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Gets the last date and time when this object was modified.
   * <p/>
   * @return a Calendar object denoting the date and time when this object was last modified.
   */
  @Override
  public Calendar getLastModifiedDateTime() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Gets the process (the what) that was last responsible for modifying this object.
   * <p/>
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
   * <p/>
   * @return a boolean value indicating whether this Auditable object has been modified or not.
   */
  @Override
  public boolean isModified() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Determines whether the specified property of this Auditable object has been modified.  The property has been
   * changed if the old and new value are not equal in value.
   * <p/>
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
   * <p/>
   * @return an object denoting the user who modified this object.
   */
  @Override
  public USER getModifiedBy() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Sets the user who is responsible for modifying this object.
   * <p/>
   * @param user an object denoting the user who modified this object.
   */
  @Override
  public void setModifiedBy(final USER user) {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Gets the date and time when this object was modified.
   * <p/>
   * @return a Calendar object denoting the date and time when this object was modified.
   */
  @Override
  public Calendar getModifiedDateTime() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Sets the date and time when this object was modified.
   * <p/>
   * @param dateTime a Calendar object denoting the date and time when this object was modified.
   */
  @Override
  public void setModifiedDateTime(final Calendar dateTime) {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Gets the process (the what) that functionally modified this object.
   * <p/>
   * @return an object denoting the process that modified this object.
   */
  @Override
  public PROCESS getModifyingProcess() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Sets the process (the what) that functionally modified this object.
   * <p/>
   * @param process an object denoting the process that modified this object.
   */
  @Override
  public void setModifyingProcess(final PROCESS process) {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

}
