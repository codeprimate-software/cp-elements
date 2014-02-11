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

import java.util.Calendar;

/**
 * The Month enum defines constants (enumerated values) for Months in the year.
 * <p/>
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
