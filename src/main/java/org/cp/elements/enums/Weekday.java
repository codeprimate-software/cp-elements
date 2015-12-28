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

import static org.cp.elements.util.CollectionExtensions.from;

import java.util.Calendar;

/**
 * The Weekday enum defines constants (enumerated values) for all the days of a week.
 *
 * @author John J. Blum
 * @see java.lang.Enum
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public enum Weekday {
  SUNDAY("Sun", "Sunday", 1, Calendar.SUNDAY),
  MONDAY("Mon", "Monday", 2, Calendar.MONDAY),
  TUESDAY("Tue", "Tuesday", 3, Calendar.TUESDAY),
  WEDNESDAY("Wed", "Wednesday", 4, Calendar.WEDNESDAY),
  THURSDAY("Thu", "Thursday", 5, Calendar.THURSDAY),
  FRIDAY("Fri", "Friday", 6, Calendar.FRIDAY),
  SATURDAY("Sat", "Saturday", 7, Calendar.SATURDAY);

  private final int calendarConstant;
  private final int position;

  private final String abbreviation;
  private final String name;

  Weekday(final String abbreviation, final String name, final int position, final int calendarConstant) {
    this.abbreviation = abbreviation;
    this.name = name;
    this.position = position;
    this.calendarConstant = calendarConstant;
  }

  public static Weekday valueOfAbbreviation(final String abbreviation) {
    for (Weekday day : values()) {
      if (day.getAbbreviation().equalsIgnoreCase(abbreviation)) {
        return day;
      }
    }

    return null;
  }

  public static Weekday valueOfCalendarConstant(final int calendarConstant) {
    for (Weekday day : values()) {
      if (day.getCalendarConstant() == calendarConstant) {
        return day;
      }
    }

    return null;
  }

  public static Weekday valueOfName(final String name) {
    for (Weekday day : values()) {
      if (day.getName().equalsIgnoreCase(name)) {
        return day;
      }
    }

    return null;
  }

  public static Weekday valueOfPosition(final int position) {
    for (Weekday day : values()) {
      if (day.getPosition() == position) {
        return day;
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

  public boolean isWeekday() {
    return !isWeekend();
  }

  public boolean isWeekend() {
    return from(SUNDAY, SATURDAY).toSet().contains(this);
  }

  @Override
  public String toString() {
    return this.name;
  }

}
