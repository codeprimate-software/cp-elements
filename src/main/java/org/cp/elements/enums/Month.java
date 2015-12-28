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

package org.cp.elements.enums;

import java.util.Calendar;

/**
 * The Month enum defines constants (enumerated values) for Months in the year.
 *
 * @author John J. Blum
 * @see java.lang.Enum
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public enum Month {
  JANUARY("Jan", "January", 1, Calendar.JANUARY),
  FEBRUARY("Feb", "February", 2, Calendar.FEBRUARY),
  MARCH("Mar", "March", 3, Calendar.MARCH),
  APRIL("Apr", "April", 4, Calendar.APRIL),
  MAY("May", "May", 5, Calendar.MAY),
  JUNE("Jun", "June", 6, Calendar.JUNE),
  JULY("Jul", "July", 7, Calendar.JULY),
  AUGUST("Aug", "August", 8, Calendar.AUGUST),
  SEPTEMBER("Sep", "September", 9, Calendar.SEPTEMBER),
  OCTOBER("Oct", "October", 10, Calendar.OCTOBER),
  NOVEMBER("Nov", "November", 11, Calendar.NOVEMBER),
  DECEMBER("Dec", "December", 12, Calendar.DECEMBER);

  private final int calendarConstant;
  private final int position;

  private final String abbreviation;
  private final String name;

  Month(final String abbreviation, final String name, final int position, final int calendarConstant) {
    this.abbreviation = abbreviation;
    this.name = name;
    this.position = position;
    this.calendarConstant = calendarConstant;
  }

  public static Month valueOfAbbreviation(final String abbreviation) {
    for (Month month : values()) {
      if (month.getAbbreviation().equalsIgnoreCase(abbreviation)) {
        return month;
      }
    }

    return null;
  }

  public static Month valueOfCalendarConstant(final int calendarConstant) {
    for (Month month : values()) {
      if (month.getCalendarConstant() == calendarConstant) {
        return month;
      }
    }

    return null;
  }

  public static Month valueOfName(final String name) {
    for (Month month : values()) {
      if (month.getName().equalsIgnoreCase(name)) {
        return month;
      }
    }

    return null;
  }

  public static Month valueOfPosition(final int position) {
    for (Month month : values()) {
      if (month.getPosition() == position) {
        return month;
      }
    }

    return null;
  }

  public String getAbbreviation() {
    return abbreviation;
  }

  public int getCalendarConstant() {
    return calendarConstant;
  }

  public String getName() {
    return name;
  }

  public int getPosition() {
    return position;
  }

  @Override
  public String toString() {
    return this.name;
  }

}
