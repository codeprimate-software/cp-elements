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

import java.time.DayOfWeek;
import java.util.Arrays;
import java.util.Calendar;
import java.util.function.Function;
import java.util.function.Predicate;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.util.CollectionExtensions;

/**
 * {@link Enum Enumeration} for all the days in a week.
 *
 * @author John J. Blum
 * @see java.lang.Enum
 * @see java.util.Calendar
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public enum Weekday {

  SUNDAY("Sun", "Sunday", Calendar.SUNDAY, DayOfWeek.SUNDAY, 1),
  MONDAY("Mon", "Monday", Calendar.MONDAY, DayOfWeek.MONDAY, 2),
  TUESDAY("Tue", "Tuesday", Calendar.TUESDAY, DayOfWeek.TUESDAY, 3),
  WEDNESDAY("Wed", "Wednesday", Calendar.WEDNESDAY, DayOfWeek.WEDNESDAY, 4),
  THURSDAY("Thu", "Thursday", Calendar.THURSDAY, DayOfWeek.THURSDAY, 5),
  FRIDAY("Fri", "Friday", Calendar.FRIDAY, DayOfWeek.FRIDAY, 6),
  SATURDAY("Sat", "Saturday", Calendar.SATURDAY, DayOfWeek.SATURDAY, 7);

  /**
   * Factory method used to find a {@link Weekday} for the given {@link String abbreviation}.
   *
   * @param abbreviation {@link String} containing the {@literal abbreviation} used to find and match
   * the desired {@link Weekday}.
   * @return a {@link Weekday} matching the given {@link String abbreviation} or {@literal null}
   * if no {@link Weekday} has the given {@link String abbreviation}.
   * @see #valueOfPredicate(Predicate)
   * @see #getAbbreviation()
   */
  public static @Nullable Weekday valueOfAbbreviation(@Nullable String abbreviation) {
    return valueOfPredicate(day -> day.getAbbreviation().equalsIgnoreCase(abbreviation));
  }

  /**
   * Factory method used to find a {@link Weekday} for the given {@link Calendar#DAY_OF_WEEK}.
   *
   * @param calendarDay {@link Calendar#DAY_OF_WEEK} used to find and match the desired {@link Weekday}.
   * @return a {@link Weekday} matching the given {@link Calendar#DAY_OF_WEEK} or {@literal null}
   * if no {@link Weekday} has the given {@link Calendar#DAY_OF_WEEK}.
   * @see java.util.Calendar#DAY_OF_WEEK
   * @see #valueOfPredicate(Predicate)
   * @see #getCalendarDay()
   */
  public static @Nullable Weekday valueOfCalendarDay(int calendarDay) {
    return valueOfPredicate(day -> day.getCalendarDay() == calendarDay);
  }

  /**
   * Factory method used to find a {@link Weekday} for the given {@link DayOfWeek}.
   *
   * @param dayOfWeek {@link DayOfWeek} used to find and match the desired {@link Weekday}.
   * @return a {@link Weekday} matching the given {@link DayOfWeek} or {@literal null}
   * if no {@link Weekday} has the given {@link DayOfWeek}.
   * @see #valueOfPredicate(Predicate)
   * @see java.time.DayOfWeek
   * @see #getDayOfWeek()
   */
  public static @Nullable Weekday valueOfDayOfWeek(@Nullable DayOfWeek dayOfWeek) {
    return valueOfPredicate(day -> day.getDayOfWeek().equals(dayOfWeek));
  }

  /**
   * Factory method used to find a {@link Weekday} for the given {@link String name}.
   *
   * @param name {@link String} containing the {@literal name} used to find and match the desired {@link Weekday}.
   * @return a {@link Weekday} matching the given {@link String name} or {@literal null}
   * if no {@link Weekday} has the given {@link String name}.
   * @see #valueOfPredicate(Predicate)
   * @see #getName()
   */
  public static @Nullable Weekday valueOfName(@Nullable String name) {
    return valueOfPredicate(day -> day.getName().equalsIgnoreCase(name));
  }

  /**
   * Factory method used to find a {@link Weekday} for the given {@link Integer position}.
   *
   * @param position {@link Integer} containing the {@literal position} used to find and match
   * the desired {@link Weekday}.
   * @return a {@link Weekday} matching the given {@link Integer position} or {@literal null}
   * if no {@link Weekday} has the given {@link Integer position}.
   * @see #valueOfPredicate(Predicate)
   * @see #getPosition()
   */
  public static @Nullable Weekday valueOfPosition(int position) {
    return valueOfPredicate(day -> day.getPosition() == position);
  }

  /**
   * Finds the first {@link Weekday} enumerated value that matches the given, required {@link Predicate}.
   *
   * @param predicate {@link Predicate} used to find and match the desired {@link Weekday}.
   * @return a {@link Weekday} matching the {@link Predicate} or {@literal null} if no {@link Weekday}
   * is found matching the {@link Predicate}.
   * @see java.util.function.Predicate
   */
  private static @Nullable Weekday valueOfPredicate(@NotNull Predicate<Weekday> predicate) {

    return Arrays.stream(values())
      .filter(predicate)
      .findFirst()
      .orElse(null);
  }

  private final int calendarDay;
  private final int position;

  private final java.time.DayOfWeek dayOfWeek;

  private final String abbreviation;
  private final String name;

  /**
   * Constructs a new {@link Weekday} initialized with weekday (day of week) metadata and descriptors.
   *
   * @param abbreviation {@link String} specifying the {@literal abbreviation} for {@literal this} {@link Weekday}.
   * @param name {@link String} containing the {@literal name} of {@literal this} {@link Weekday}.
   * @param calendarDay {@link Calendar#DAY_OF_WEEK} value.
   * @param dayOfWeek {@link DayOfWeek} value.
   * @param position {@link Integer#TYPE} specifying the position of {@literal this} {@link Weekday} in a calendar week.
   */
  Weekday(@NotNull String abbreviation, @NotNull String name, int calendarDay, @NotNull DayOfWeek dayOfWeek,
      int position) {

    this.abbreviation = abbreviation;
    this.name = name;
    this.calendarDay = calendarDay;
    this.dayOfWeek = dayOfWeek;
    this.position = position;
  }

  /**
   * Gets the {@link String abbreviation} for {@literal this} {@link Weekday}.
   *
   * @return the {@link String abbreviation} for {@literal this} {@link Weekday}.
   * @see #getName()
   */
  public @NotNull String getAbbreviation() {
    return this.abbreviation;
  }

  /**
   * Gets the {@link Calendar#DAY_OF_WEEK} for {@literal this} {@link Weekday}.
   *
   * @return the {@link Calendar#DAY_OF_WEEK} for {@literal this} {@link Weekday}.
   * @see java.util.Calendar#DAY_OF_WEEK
   */
  public int getCalendarDay() {
    return this.calendarDay;
  }

  /**
   * Gets the {@link DayOfWeek} for {@literal this} {@link Weekday}.
   *
   * @return the {@link DayOfWeek} for {@literal this} {@link Weekday}.
   * @see java.time.DayOfWeek
   */
  public @NotNull DayOfWeek getDayOfWeek() {
    return this.dayOfWeek;
  }

  /**
   * Gets the {@link String name} for {@literal this} {@link Weekday}.
   *
   * @return the {@link String name} for {@literal this} {@link Weekday}.
   * @see #getAbbreviation()
   */
  public @NotNull String getName() {
    return this.name;
  }

  /**
   * Gets the {@link Integer position} for {@literal this} {@link Weekday}.
   *
   * @return the {@link Integer position} for {@literal this} {@link Weekday}.
   */
  public int getPosition() {
    return this.position;
  }

  /**
   * Gets an adjacent {@link Weekday} relative to {@literal this} {@link Weekday}.
   * <p>
   * The {@literal adjacent} {@link Weekday} is either the day before {@literal this} {@link Weekday}, as in yesterday,
   * or the day after {@literal this} {@link Weekday}, as in tomorrow.
   *
   * @param positionFunction {@link Function} used to adjust the {@link #getPosition()} of {@literal this}
   * {@link Weekday} to an {@literal adjacent} day.
   * @return an adjacent {@link Weekday} relative to {@literal this} {@link Weekday}.
   * @see java.util.function.Function
   */
  private Weekday getAdjacentWeekday(@NotNull Function<Integer, Integer> positionFunction) {
    return valueOfPosition(positionFunction.apply(getPosition()));
  }

  /**
   * Gets the {@link Weekday} after {@literal this} {@link Weekday}.
   * <p>
   * If {@literal this} {@link Weekday} is {@link Weekday#SATURDAY},
   * then this method will return {@link Weekday#SUNDAY} and the week resets.
   *
   * @return the {@link Weekday} after {@literal this} {@link Weekday}.
   * @see #getAdjacentWeekday(Function)
   */
  public Weekday getTomorrow() {
    return getAdjacentWeekday(position -> position >= 7 ? 1 : position + 1);
  }

  /**
   * Gets the {@link Weekday} prior to {@literal this} {@link Weekday}.
   * <p>
   * If {@literal this} {@link Weekday} is {@link Weekday#SUNDAY},
   * then this method will return {@link Weekday#SATURDAY} and the week returns to the previous week.
   *
   * @return the {@link Weekday} prior to {@literal this} {@link Weekday}.
   * @see #getAdjacentWeekday(Function)
   */
  public Weekday getYesterday() {
    return getAdjacentWeekday(position -> position <= 1 ? 7 : position - 1);
  }

  /**
   * Determines whether this {@link Weekday} is the given {@link Weekday day in the week}.
   *
   * @param day {@link Weekday} to evaluate; must not be {@literal null}.
   * @return a boolean value determining if this {@link Weekday} is the given {@link Weekday day in the week}.
   */
  private boolean isDay(@NotNull Weekday day) {
    return this.equals(day);
  }

  /**
   * Determines whether this {@link Weekday} is a {@link Weekday#SUNDAY}.
   *
   * @return a boolean value indicating whether this {@link Weekday} is a {@link Weekday#SUNDAY}.
   * @see #isDay(Weekday)
   * @see Weekday#SUNDAY
   */
  public boolean isSunday() {
    return isDay(Weekday.SUNDAY);
  }

  /**
   * Determines whether this {@link Weekday} is a {@link Weekday#MONDAY}.
   *
   * @return a boolean value indicating whether this {@link Weekday} is a {@link Weekday#MONDAY}.
   * @see #isDay(Weekday)
   * @see Weekday#MONDAY
   */
  public boolean isMonday() {
    return Weekday.MONDAY.equals(this);
  }

  /**
   * Determines whether this {@link Weekday} is a {@link Weekday#TUESDAY}.
   *
   * @return a boolean value indicating whether this {@link Weekday} is a {@link Weekday#TUESDAY}.
   * @see #isDay(Weekday)
   * @see Weekday#TUESDAY
   */
  public boolean isTuesday() {
    return Weekday.TUESDAY.equals(this);
  }

  /**
   * Determines whether this {@link Weekday} is a {@link Weekday#WEDNESDAY}.
   *
   * @return a boolean value indicating whether this {@link Weekday} is a {@link Weekday#WEDNESDAY}.
   * @see #isDay(Weekday)
   * @see Weekday#WEDNESDAY
   */
  public boolean isWednesday() {
    return Weekday.WEDNESDAY.equals(this);
  }

  /**
   * Determines whether this {@link Weekday} is a {@link Weekday#THURSDAY}.
   *
   * @return a boolean value indicating whether this {@link Weekday} is a {@link Weekday#THURSDAY}.
   * @see #isDay(Weekday)
   * @see Weekday#THURSDAY
   */
  public boolean isThursday() {
    return Weekday.THURSDAY.equals(this);
  }

  /**
   * Determines whether this {@link Weekday} is a {@link Weekday#FRIDAY}.
   *
   * @return a boolean value indicating whether this {@link Weekday} is a {@link Weekday#FRIDAY}.
   * @see #isDay(Weekday)
   * @see Weekday#FRIDAY
   */
  public boolean isFriday() {
    return Weekday.FRIDAY.equals(this);
  }

  /**
   * Determines whether this {@link Weekday} is a {@link Weekday#SATURDAY}.
   *
   * @return a boolean value indicating whether this {@link Weekday} is a {@link Weekday#SATURDAY}.
   * @see #isDay(Weekday)
   * @see Weekday#SATURDAY
   */
  public boolean isSaturday() {
    return Weekday.SATURDAY.equals(this);
  }

  /**
   * Determines whether {@literal this} {@link Weekday} enumerated value is a day in the week not on the weekend.
   * <p>
   * Will return {@literal true} if the {@link Weekday} is in the set of: [{@literal Monday}, {@literal Tuesday},
   * {@literal Wednesday}, {@literal Thursday}, {@literal Friday}].
   * <p>
   * Will return {@literal false} if the {@link Weekday} is in the set of: [{@literal Saturday}, {@literal Sunday}].
   *
   * @return a boolean value if {@literal this} {@link Weekday} enumerated value is a day in the week
   * not on the weekend.
   * @see #isWeekend()
   */
  public boolean isWeekday() {
    return !isWeekend();
  }

  /**
   * Determines whether {@literal this} {@link Weekday} enumerated value is a day of the weekend.
   * <p>
   * Will return {@literal true} if the {Weekday} is in the set of: [{@literal Sunday}, {@literal Saturday}].
   * <p>
   * Will return {@literal false} if the {@link Weekday} is in the set of: [{@literal Monday}, {@literal Tuesday},
   * {@literal Wednesday}, {@literal Thursday}, {@literal Friday}].
   *
   * @return a boolean value if {@literal this} {@link Weekday} enumerated value is a day of the weekend.
   * @see #isWeekday()
   */
  public boolean isWeekend() {
    return CollectionExtensions.from(SUNDAY, SATURDAY).toSet().contains(this);
  }

  /**
   * Returns a {@link String} representation of {@literal this} {@link Weekday}.
   *
   * @return a {@link String} describing {@literal this} {@link Weekday}.
   * @see java.lang.String
   * @see #getName()
   */
  @Override
  public @NotNull String toString() {
    return getName();
  }
}
