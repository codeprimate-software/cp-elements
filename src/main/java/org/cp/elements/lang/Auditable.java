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

package org.cp.elements.lang;

import java.util.Calendar;

/**
 * The Auditable interface defines a contract for objects that need to be audited, allowing all changes to be tracked
 * in fine-grained detail by specifying who, when and what made changes to this object.
 * <p/>
 * @author John J. Blum
 * @param <USER> an object type for tracking the user.
 * @param <PROCESS> an object type for tracking the process.
 * @see java.util.Calendar
 * @see org.cp.elements.lang.Identifiable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Auditable<USER, PROCESS> {

  /**
   * Gets the user who is responsible for the creation of this object.
   * <p/>
   * @return an object denoting the user who created this object.
   */
  USER getCreatedBy();

  /**
   * Sets the user who is responsible for the creation of this object.
   * <p/>
   * @param user an object denoting the user who created this object.
   */
  void setCreatedBy(USER user);

  /**
   * Gets the date and time when this object was created.
   * <p/>
   * @return a Calendar object denoting the date and time when this object was created.
   */
  Calendar getCreatedDateTime();

  /**
   * Sets the date and time when this object was created.
   * <p/>
   * @param dateTime a Calendar object denoting the date and time when this object was created.
   */
  void setCreatedDateTime(Calendar dateTime);

  /**
   * Gets the process (the what) that functionally created this object.
   * <p/>
   * @return an object denoting the process that created this object.
   */
  PROCESS getCreatingProcess();

  /**
   * Sets the process (the what) that functionally created this object.
   * <p/>
   * @param process an object denoting the process that created this object.
   */
  void setCreatingProcess(PROCESS process);

  /**
   * Gets the user who was last responsible for modifying this object.
   * <p/>
   * @return an object denoting the last user responsible for modifying this object.
   */
  USER getLastModifiedBy();

  /**
   * Gets the last date and time when this object was modified.
   * <p/>
   * @return a Calendar object denoting the date and time when this object was last modified.
   */
  Calendar getLastModifiedDateTime();

  /**
   * Gets the process (the what) that was last responsible for modifying this object.
   * <p/>
   * @return an object denoting the last process responsible for modifying this object.
   */
  PROCESS getLastModifyingProcess();

  /**
   * Determines whether this Auditable object has been modified.  One particular implementation suggests that
   * if the last modified date/time does not match the current modified date/time then the Auditable object has
   * been modified.  Of course, if any propery value of the object has changed, then the object has been modified.
   * <p/>
   * @return a boolean value indicating whether this Auditable object has been modified or not.
   */
  boolean isModified();

  /**
   * Determines whether the specified property of this Auditable object has been modified.  The property has been
   * changed if the old and new value are not equal in value.
   * <p/>
   * @param propertyName a String value specifying the name of the property to check for modification.
   * @return a boolean value indicating whether the specified property of this Auditable object, identified by name,
   * has been modified.
   */
  boolean isModified(String propertyName);

  /**
   * Gets the user who is responsible for modifying this object.
   * <p/>
   * @return an object denoting the user who modified this object.
   */
  USER getModifiedBy();

  /**
   * Sets the user who is responsible for modifying this object.
   * <p/>
   * @param user an object denoting the user who modified this object.
   */
  void setModifiedBy(USER user);

  /**
   * Gets the date and time when this object was modified.
   * <p/>
   * @return a Calendar object denoting the date and time when this object was modified.
   */
  Calendar getModifiedDateTime();

  /**
   * Sets the date and time when this object was modified.
   * <p/>
   * @param dateTime a Calendar object denoting the date and time when this object was modified.
   */
  void setModifiedDateTime(Calendar dateTime);

  /**
   * Gets the process (the what) that functionally modified this object.
   * <p/>
   * @return an object denoting the process that modified this object.
   */
  PROCESS getModifyingProcess();

  /**
   * Sets the process (the what) that functionally modified this object.
   * <p/>
   * @param process an object denoting the process that modified this object.
   */
  void setModifyingProcess(PROCESS process);

}
