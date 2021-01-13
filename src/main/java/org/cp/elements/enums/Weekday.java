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

package org.cp.elements.enums;

import static org.cp.elements.util.CollectionExtensions.from;

import java.util.Arrays;
import java.util.Calendar;

/**
 * The {@link Weekday} enum is an {@link Enum enumeration} for all the days in a week.
 *
 * @author John J. Blum
 * @see java.lang.Enum
 * @see java.util.Calendar
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

  public static Weekday valueOfAbbreviation(String abbreviation) {

    return Arrays.stream(values())
      .filter(day -> day.getAbbreviation().equalsIgnoreCase(abbreviation))
      .findFirst()
      .orElse(null);
  }

  public static Weekday valueOfCalendarConstant(int calendarConstant) {

    return Arrays.stream(values())
      .filter(day -> day.getCalendarConstant() == calendarConstant)
      .findFirst()
      .orElse(null);
  }

  public static Weekday valueOfName(String name) {

    return Arrays.stream(values())
      .filter(day -> day.getName().equalsIgnoreCase(name))
      .findFirst()
      .orElse(null);
  }

  public static Weekday valueOfPosition(int position) {

    return Arrays.stream(values())
      .filter(day -> day.getPosition() == position)
      .findFirst()
      .orElse(null);
  }

  private final int calendarConstant;
  private final int position;

  private final String abbreviation;
  private final String name;

  Weekday(String abbreviation, String name, int position, int calendarConstant) {

    this.abbreviation = abbreviation;
    this.name = name;
    this.position = position;
    this.calendarConstant = calendarConstant;
  }

  public String getAbbreviation() {
    return this.abbreviation;
  }

  public int getCalendarConstant() {
    return this.calendarConstant;
  }

  public String getName() {
    return this.name;
  }

  public int getPosition() {
    return this.position;
  }

  public boolean isWeekday() {
    return !isWeekend();
  }

  public boolean isWeekend() {
    return from(SUNDAY, SATURDAY).toSet().contains(this);
  }

  @Override
  public String toString() {
    return getName();
  }
}
