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

package org.cp.elements.beans.event;

import java.util.Calendar;
import java.util.EventObject;

import org.cp.elements.lang.DateTimeUtils;

/**
 * The ChangeEvent class is an event signifying a state change in the source object referenced by this EventObject class.
 *
 * @author John J. Blum
 * @see java.util.EventObject
 * @since 1.0.0
 */
public class ChangeEvent extends EventObject {

  private final Calendar changeDateTime;

  /**
   * Creates an instance of the ChangeEvent class initialized with the specified object as the source for change events.
   *
   * @param source an Object reference as the source of the change events.
   */
  public ChangeEvent(final Object source) {
    super(source);
    changeDateTime = Calendar.getInstance();
  }

  /**
   * Gets the date and time that the change event on the source object occurred.
   *
   * @return a Calendar indicating the date and time of the change event.
   * @see java.util.Calendar
   */
  public Calendar getChangeDateTime() {
    return DateTimeUtils.clone(changeDateTime);
  }

}
