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

package org.cp.elements.enums;

import static org.cp.elements.lang.OperatorUtils.from;

import java.util.Calendar;

/**
 * The Weekday enum defines constants (enumerated values) for all the days of a week.
 * <p/>
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
    for (final Weekday day : values()) {
      if (day.getName().equalsIgnoreCase(name)) {
        return day;
      }
    }

    return null;
  }

  public static Weekday valueOfPosition(final int position) {
    for (final Weekday day : values()) {
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
