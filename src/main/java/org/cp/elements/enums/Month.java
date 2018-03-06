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

import java.util.Arrays;
import java.util.Calendar;

/**
 * The {@link Month} enum is an {@link Enum enumeration} of the months in a year.
 *
 * @author John J. Blum
 * @see java.lang.Enum
 * @see java.time.Month
 * @see java.util.Calendar
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public enum Month {

  JANUARY("Jan", "January", 1, Calendar.JANUARY, java.time.Month.JANUARY),
  FEBRUARY("Feb", "February", 2, Calendar.FEBRUARY, java.time.Month.FEBRUARY),
  MARCH("Mar", "March", 3, Calendar.MARCH, java.time.Month.MARCH),
  APRIL("Apr", "April", 4, Calendar.APRIL, java.time.Month.APRIL),
  MAY("May", "May", 5, Calendar.MAY, java.time.Month.MAY),
  JUNE("Jun", "June", 6, Calendar.JUNE, java.time.Month.JUNE),
  JULY("Jul", "July", 7, Calendar.JULY, java.time.Month.JULY),
  AUGUST("Aug", "August", 8, Calendar.AUGUST, java.time.Month.AUGUST),
  SEPTEMBER("Sep", "September", 9, Calendar.SEPTEMBER, java.time.Month.SEPTEMBER),
  OCTOBER("Oct", "October", 10, Calendar.OCTOBER, java.time.Month.OCTOBER),
  NOVEMBER("Nov", "November", 11, Calendar.NOVEMBER, java.time.Month.NOVEMBER),
  DECEMBER("Dec", "December", 12, Calendar.DECEMBER, java.time.Month.DECEMBER);

  public static Month valueOfAbbreviation(String abbreviation) {

    return Arrays.stream(values())
      .filter(month -> month.getAbbreviation().equalsIgnoreCase(abbreviation))
      .findFirst()
      .orElse(null);
  }

  public static Month valueOfCalendarMonth(int calendarMonth) {

    return Arrays.stream(values())
      .filter(month -> month.getCalendarMonth() == calendarMonth)
      .findFirst()
      .orElse(null);
  }

  public static Month valueOfJavaTimeMonth(java.time.Month javaTimeMonth) {

    return Arrays.stream(values())
      .filter(month -> month.getJavaTimeMonth().equals(javaTimeMonth))
      .findFirst()
      .orElse(null);
  }

  public static Month valueOfName(String name) {

    return Arrays.stream(values())
      .filter(month -> month.getName().equalsIgnoreCase(name))
      .findFirst()
      .orElse(null);
  }

  public static Month valueOfPosition(int position) {

    return Arrays.stream(values())
      .filter(month -> month.getPosition() == position)
      .findFirst()
      .orElse(null);
  }

  private final int calendarMonth;
  private final int position;

  private final java.time.Month month;

  private final String abbreviation;
  private final String name;

  /**
   * Constructs an instance of the {@link Month} enum initialized with the given month meta-data and descriptors.
   *
   * @param abbreviation abbreviation for the month (e.g. Jan).
   * @param name name of the month (e.g January).
   * @param position integer value indicating the position of the month within th calendar year (e.g. January == 1).
   * @param calendarMonth {@link Calendar} month (e.g. {@link Calendar#JANUARY}.
   * @param month {@link java.time.Month} value of this {@link Month} enum (e.g. {@link java.time.Month#JANUARY}.
   */
  Month(String abbreviation, String name, int position, int calendarMonth, java.time.Month month) {

    this.abbreviation = abbreviation;
    this.name = name;
    this.position = position;
    this.calendarMonth = calendarMonth;
    this.month = month;
  }

  public String getAbbreviation() {
    return this.abbreviation;
  }

  public int getCalendarMonth() {
    return this.calendarMonth;
  }

  public java.time.Month getJavaTimeMonth() {
    return this.month;
  }

  public String getName() {
    return this.name;
  }

  public int getPosition() {
    return this.position;
  }

  @Override
  public String toString() {
    return getName();
  }
}
