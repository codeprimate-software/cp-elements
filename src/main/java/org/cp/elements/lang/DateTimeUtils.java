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
 * The DateTimeUtils class contains common functionality for working with dates and time.
 * <p/>
 * @author John J. Blum
 * @see java.util.Calendar
 * @see java.util.Date
 * @see java.sql.Time
 * @see java.sql.Timestamp
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class DateTimeUtils {

  public static final int NUMBER_OF_MILLISECONDS_IN_SECOND = 1000;
  public static final int NUMBER_OF_SECONDS_IN_MINUTE = 60;
  public static final int NUMBER_OF_MINUTES_IN_HOUR = 60;
  public static final int NUBMER_OF_HOURS_IN_DAY = 24;
  public static final int NUMBER_OF_DAYS_IN_FEBRUARY_OF_LEAP_YEAR = 29;
  public static final int NUMBER_OF_DAYS_IN_WEEK = 7;
  public static final int NUMBER_OF_DAYS_IN_YEAR = 365;
  public static final int NUMBER_OF_DAYS_IN_LEAP_YEAR = 366;
  public static final int NUMBER_OF_MONTHS_IN_YEAR = 12;
  public static final int NUMBER_OF_YEARS_IN_DECADE = 10;
  public static final int NUMBER_OF_YEARS_IN_SCORE = 20;
  public static final int NUMBER_OF_YEARS_IN_CENTURY = 100;
  public static final int NUMBER_OF_YEARS_IN_MILLENNIUM = 1000;

  public static final int[] NUMBER_OF_DAYS_IN_MONTH = {
    31, // January
    28, // February
    31, // March
    30, // April
    31, // May
    30, // June
    31, // July
    31, // August
    30, // September
    31, // October
    30, // November
    31  // December
  };

  /**
   * Clones the specified Calendar object if not null, otherwise if the date and time value is null
   * then this method returns null.
   * <p/>
   * @param dateTime the Calendar object to clone.
   * @return a clone of the specified Calendar object if the Calendar is not null, otherwise return null.
   * @see java.util.Calendar#clone()
   */
  public static Calendar clone(final Calendar dateTime) {
    return (dateTime == null ? null : (Calendar) dateTime.clone());
  }

  /**
   * Creates a Calendar instance with a date and time set to the time in milliseconds.
   * <p/>
   * @param timeInMilliseconds a long value indicating the number of milliseconds since the EPOCH.
   * @return a Calendar instance initialized with the specified date/time in milliseconds.
   * @see java.util.Calendar
   */
  public static Calendar create(final long timeInMilliseconds) {
    final Calendar dateTime = Calendar.getInstance();
    dateTime.clear();
    dateTime.setTimeInMillis(timeInMilliseconds);
    return dateTime;
  }

  /**
   * Truncates the time portion of the Calendar's date/time components.  If the Calendar object is null, then null
   * is returned.  The fields of the Calendar to be truncated include the hour of day, minute, second and milliseconds.
   * <p/>
   * @param dateTime the Calendar object to truncate.
   * @return the Calendar object with the time portion truncated (the corresponding time fields of this Calendar
   * are set to zero).
   * @see java.util.Calendar
   */
  public static Calendar truncate(final Calendar dateTime) {
    if (dateTime != null) {
      dateTime.clear(Calendar.HOUR_OF_DAY);
      dateTime.clear(Calendar.MINUTE);
      dateTime.clear(Calendar.SECOND);
      dateTime.clear(Calendar.MILLISECOND);
    }

    return dateTime;
  }

}
