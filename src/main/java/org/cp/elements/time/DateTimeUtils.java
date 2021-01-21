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
package org.cp.elements.time;

import java.util.Calendar;
import java.util.Optional;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;

/**
 * The {@link DateTimeUtils} class is an abstract utility class containing common functionality for conveniently
 * working with dates and times.
 *
 * This class predates the new {@code java.time} package in the Java API and it is highly recommended that users use
 * those date/time types instead.
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
   * Clones the specified {@link Calendar} object.
   *
   * @param dateTime {@link Calendar} object to clone.
   * @return a clone of the specified {@link Calendar} object if not {@literal null}, otherwise return {@literal null}.
   * @see java.util.Calendar#clone()
   */
  @NullSafe
  public static @Nullable Calendar clone(@NotNull Calendar dateTime) {

    return Optional.ofNullable(dateTime)
      .map(Calendar::clone)
      .map(Calendar.class::cast)
      .orElse(null);
  }

  /**
   * Creates a {@link Calendar} initialized with the specified date and time in milliseconds.
   *
   * @param timeInMilliseconds long value indicating the number of milliseconds since the EPOCH.
   * @return a {@link Calendar} initialized with the specified date/time in milliseconds.
   * @see java.util.Calendar
   */
  public static @NotNull Calendar create(long timeInMilliseconds) {

    Calendar dateTime = Calendar.getInstance();

    dateTime.clear();
    dateTime.setTimeInMillis(timeInMilliseconds);

    return dateTime;
  }

  /**
   * Truncates the time portion of the {@link Calendar Calendar's} date & time components.
   *
   * If the {@link Calendar} object is {@literal null}, then {@literal null} is returned. The fields of
   * the {@link Calendar} to be truncated include the hour of day, minute, second and milliseconds.
   *
   * @param dateTime {@link Calendar} object to truncate.
   * @return the {@link Calendar} object with the time portion truncated. All corresponding time fields
   * of the {@link Calendar} object are set to zero.
   * @see java.util.Calendar
   */
  @NullSafe
  public static @Nullable Calendar truncate(@NotNull Calendar dateTime) {

    if (dateTime != null) {
      dateTime.clear(Calendar.HOUR_OF_DAY);
      dateTime.clear(Calendar.MINUTE);
      dateTime.clear(Calendar.SECOND);
      dateTime.clear(Calendar.MILLISECOND);
    }

    return dateTime;
  }
}
