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

package org.cp.elements.lang;

import java.util.Calendar;
import java.util.Optional;

import org.cp.elements.lang.annotation.NullSafe;

/**
 * The DateTimeUtils class contains common functionality for working with dates and time.
 *
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
  public static final int NUMBER_OF_HOURS_IN_DAY = 24;
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
   *
   * @param dateTime the Calendar object to clone.
   * @return a clone of the specified Calendar object if the Calendar is not null, otherwise return null.
   * @see java.util.Calendar#clone()
   */
  @NullSafe
  public static Calendar clone(Calendar dateTime) {
    return Optional.ofNullable(dateTime).map(localDateTime -> (Calendar) localDateTime.clone()).orElse(null);
  }

  /**
   * Creates a Calendar instance with a date and time set to the time in milliseconds.
   *
   * @param timeInMilliseconds a long value indicating the number of milliseconds since the EPOCH.
   * @return a Calendar instance initialized with the specified date/time in milliseconds.
   * @see java.util.Calendar
   */
  public static Calendar create(long timeInMilliseconds) {
    Calendar dateTime = Calendar.getInstance();
    dateTime.clear();
    dateTime.setTimeInMillis(timeInMilliseconds);
    return dateTime;
  }

  /**
   * Truncates the time portion of the Calendar's date/time components.  If the Calendar object is null, then null
   * is returned.  The fields of the Calendar to be truncated include the hour of day, minute, second and milliseconds.
   *
   * @param dateTime the Calendar object to truncate.
   * @return the Calendar object with the time portion truncated (the corresponding time fields of this Calendar
   * are set to zero).
   * @see java.util.Calendar
   */
  @NullSafe
  public static Calendar truncate(Calendar dateTime) {
    Optional.ofNullable(dateTime).ifPresent(localDateTime -> {
      localDateTime.clear(Calendar.HOUR_OF_DAY);
      localDateTime.clear(Calendar.MINUTE);
      localDateTime.clear(Calendar.SECOND);
      localDateTime.clear(Calendar.MILLISECOND);
    });

    return dateTime;
  }
}
