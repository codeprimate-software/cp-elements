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

package org.cp.elements.beans.event;

import java.util.Calendar;
import java.util.EventObject;

import org.cp.elements.lang.DateTimeUtils;

/**
 * The ChangeEvent class is an event signifying a state change in the source object referenced by this EventObject class.
 * <p/>
 * @author John J. Blum
 * @see java.util.EventObject
 * @since 1.0.0
 */
public class ChangeEvent extends EventObject {

  private final Calendar changeDateTime;

  /**
   * Creates an instance of the ChangeEvent class initialized with the specified object as the source for change events.
   * <p/>
   * @param source an Object reference as the source of the change events.
   */
  public ChangeEvent(final Object source) {
    super(source);
    changeDateTime = Calendar.getInstance();
  }

  /**
   * Gets the date and time that the change event on the source object occurred.
   * <p/>
   * @return a Calendar indicating the date and time of the change event.
   * @see java.util.Calendar
   */
  public Calendar getChangeDateTime() {
    return DateTimeUtils.clone(changeDateTime);
  }

}
