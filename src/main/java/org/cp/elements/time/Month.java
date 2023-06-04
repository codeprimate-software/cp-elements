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

import java.util.Arrays;
import java.util.Calendar;
import java.util.function.Function;
import java.util.function.Predicate;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * An {@link Enum enumeration} of the months in a calendar year.
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

  /**
   * Factory method used to find a {@link Month} for the given {@link String abbreviation}.
   *
   * @param abbreviation {@link String} containing the abbreviation for the {@link Month} to find.
   * @return a {@link Month} in the enumeration with the given {@link String abbreviation},
   * or {@literal null} if no {@link Month} with the given {@link String abbreviation} exists.
   * @see #valueOfPredicate(Predicate)
   * @see #getAbbreviation()
   */
  public static @Nullable Month valueOfAbbreviation(@Nullable String abbreviation) {
    return valueOfPredicate(month -> month.getAbbreviation().equalsIgnoreCase(abbreviation));
  }

  /**
   * Factory method used to find a {@link Month} for the given {@link java.util.Calendar#MONTH}.
   *
   * @param calendarMonth {@link Calendar#MONTH} used to find a {@link Month} in the enumeration.
   * @return a {@link Month} in the enumeration with the given {@link java.util.Calendar#MONTH},
   * or {@literal null} if no {@link Month} with the given {@link java.util.Calendar#MONTH} exists.
   * @see #valueOfPredicate(Predicate)
   * @see java.util.Calendar#MONTH
   */
  public static @Nullable Month valueOfCalendarMonth(int calendarMonth) {
    return valueOfPredicate(month -> month.getCalendarMonth() == calendarMonth);
  }

  /**
   * Factory method used to find a {@link Month} for the given {@link java.time.Month}.
   *
   * @param javaTimeMonth {@link java.time.Month} used to find a {@link Month} in the enumeration.
   * @return a {@link Month} in the enumeration with the given {@link java.time.Month},
   * or {@literal null} if no {@link Month} with the given {@link java.time.Month} exists.
   * @see #valueOfPredicate(Predicate)
   * @see java.time.Month
   */
  public static @Nullable Month valueOfJavaTimeMonth(@Nullable java.time.Month javaTimeMonth) {
    return valueOfPredicate(month -> month.getJavaTimeMonth().equals(javaTimeMonth));
  }

  /**
   * Factory method used to find a {@link Month} for the given {@link String name}.
   *
   * @param name {@link String} containing the name of the {@link Month} to find.
   * @return a {@link Month} in the enumeration with the given {@link String name},
   * or {@literal null} if no {@link Month} with the given {@link String name} exists.
   * @see #valueOfPredicate(Predicate)
   * @see #getName()
   */
  public static @Nullable Month valueOfName(@Nullable String name) {
    return valueOfPredicate(month -> month.getName().equalsIgnoreCase(name));
  }

  /**
   * Factory method used to find a {@link Month} at the given {@link Integer#TYPE position} in the calendar year.
   *
   * @param position {@link Integer#TYPE} indicating the position of the {@link Month} in the calendar year to find.
   * @return the {@link Month} in the enumeration at the {@link Integer#TYPE position} in the calendar year,
   * or {@literal null} if the given {@link Integer#TYPE position} is not within the calendar year
   * {@literal  (1 <= position <= 12)}.
   * @see #valueOfPredicate(Predicate)
   * @see #getPosition()
   */
  public static @Nullable Month valueOfPosition(int position) {
    return valueOfPredicate(month -> month.getPosition() == position);
  }

  /**
   * Finds the first {@link Month} enumerated value that matches the given, required {@link Predicate}.
   *
   * @param predicate {@link Predicate} used to find and match the desired {@link Month}.
   * @return a {@link Month} matching the {@link Predicate} or {@literal null} if no {@link Month}
   * is found matching the {@link Predicate}.
   * @see java.util.function.Predicate
   * @see #values()
   */
  private static @Nullable Month valueOfPredicate(@NotNull Predicate<Month> predicate) {

    return Arrays.stream(values())
      .filter(predicate)
      .findFirst()
      .orElse(null);
  }

  private final int calendarMonth;
  private final int position;

  private final java.time.Month month;

  private final String abbreviation;
  private final String name;

  /**
   * Constructs a new the {@link Month} enum initialized with the given month metadata and descriptors.
   *
   * @param abbreviation {@link String} specifying an abbreviation for the month (for example: {@literal Jan}).
   * @param name {@link String} containing the name for the month (for example: {@literal January}).
   * @param position {@link Integer#TYPE} value specifying the order of the month in a calendar year
   * (for example: {@literal January} is the first month in the calendar year).
   * @param calendarMonth {@link Calendar} month (for example: {@link Calendar#JANUARY}.
   * @param month {@link java.time.Month} for the month (for example: {@link java.time.Month#JANUARY}).
   * @see java.util.Calendar
   * @see java.time.Month
   */
  Month(@NotNull String abbreviation, @NotNull String name, int position, int calendarMonth,
      @NotNull java.time.Month month) {

    this.abbreviation = abbreviation;
    this.name = name;
    this.position = position;
    this.calendarMonth = calendarMonth;
    this.month = month;
  }

  /**
   * Gets the {@link String abbreviation} for {@literal this} {@link Month}.
   *
   * @return the {@link String abbreviation} for {@literal this} {@link Month}.
   */
  public @NotNull String getAbbreviation() {
    return this.abbreviation;
  }

  /**
   * Gets the equivalent {@link java.util.Calendar} month for {@literal this} {@link Month}.
   *
   * @return the equivalent {@link java.util.Calendar} month for {@literal this} {@link Month}.
   * @see java.util.Calendar#MONTH
   */
  public int getCalendarMonth() {
    return this.calendarMonth;
  }

  /**
   * Gets the equivalent {@link java.time.Month} for {@literal this} {@link Month}.
   *
   * @return the equivalent {@link java.time.Month} for {@literal this} {@link Month}.
   * @see java.time.Month
   */
  public @NotNull java.time.Month getJavaTimeMonth() {
    return this.month;
  }

  /**
   * Gets the {@link String name} of {@literal this} {@link Month}.
   *
   * @return the {@link String name} of {@literal this} {@link Month}.
   */
  public @NotNull String getName() {
    return this.name;
  }

  /**
   * Gets the {@literal Integer#TYPE position} of {@literal this} {@link Month} within a calendar year.
   *
   * @return the {@literal Integer#TYPE position} of {@literal this} {@link Month} within a calendar year.
   */
  public int getPosition() {
    return this.position;
  }

  /**
   * Returns the adjacent {@link Month} either prior or following {@literal this} {@link Month}.
   *
   * @param positionFunction {@link Function} used to adjust {@literal this} {@link Month Month's}
   * {@link #getPosition()} to determine the adjacent {@link Month}.
   * @return the adjacent {@link Month} either prior or following {@literal this} {@link Month}.
   * @see java.util.function.Function
   */
  private Month getAdjacentMonth(@NotNull Function<Integer, Integer> positionFunction) {
    return valueOfPosition(positionFunction.apply(getPosition()));
  }

  /**
   * Gets the {@link Month} following {@literal this} {@link Month}.
   *
   * If {@literal this} {@link Month} is {@link Month#DECEMBER},
   * then the {@link Month} resets to {@link Month#JANUARY}.
   *
   * @return the {@link Month} following {@literal this} {@link Month}.
   * @see #getAdjacentMonth(Function)
   */
  public @NotNull Month getNextMonth() {
    return getAdjacentMonth(position -> position >= 12 ? 1 : position + 1);
  }

  /**
   * Gets the {@link Month} prior to {@literal this} {@link Month}.
   *
   * If {@literal this} {@link Month} is {@link Month#JANUARY},
   * then the {@link Month} rolls back to {@link Month#DECEMBER}.
   *
   * @return the {@link Month} prior to {@literal this} {@link Month}.
   * @see #getAdjacentMonth(Function)
   */
  public Month getPreviousMonth() {
    return getAdjacentMonth(position -> position <= 1 ? 12 : position - 1);
  }

  /**
   * Determines whether {@literal this} {@link Month} is the given, required {@link Month}.
   *
   * @param month {@link Month} used to evaluate {@literal this} {@link Month}; must not be {@literal null}.
   * @return a boolean value indicating whether {@literal this} {@link Month} is the given, required {@link Month}.
   */
  private boolean isMonth(@NotNull Month month) {
    return this.equals(month);
  }

  /**
   * Determines whether {@literal this} {@link Month} is {@link Month#JANUARY}.
   *
   * @return a boolean value indicating whether {@literal this} {@link Month} is {@link Month#JANUARY}.
   * @see #isMonth(Month)
   * @see Month#JANUARY
   */
  public boolean isJanuary() {
    return isMonth(Month.JANUARY);
  }

  /**
   * Determines whether {@literal this} {@link Month} is {@link Month#FEBRUARY}.
   *
   * @return a boolean value indicating whether {@literal this} {@link Month} is {@link Month#FEBRUARY}.
   * @see #isMonth(Month)
   * @see Month#FEBRUARY
   */
  public boolean isFebruary() {
    return isMonth(Month.FEBRUARY);
  }

  /**
   * Determines whether {@literal this} {@link Month} is {@link Month#MARCH}.
   *
   * @return a boolean value indicating whether {@literal this} {@link Month} is {@link Month#MARCH}.
   * @see #isMonth(Month)
   * @see Month#MARCH
   */
  public boolean isMarch() {
    return isMonth(Month.MARCH);
  }

  /**
   * Determines whether {@literal this} {@link Month} is {@link Month#APRIL}.
   *
   * @return a boolean value indicating whether {@literal this} {@link Month} is {@link Month#APRIL}.
   * @see #isMonth(Month)
   * @see Month#APRIL
   */
  public boolean isApril() {
    return isMonth(Month.APRIL);
  }

  /**
   * Determines whether {@literal this} {@link Month} is {@link Month#MAY}.
   *
   * @return a boolean value indicating whether {@literal this} {@link Month} is {@link Month#MAY}.
   * @see #isMonth(Month)
   * @see Month#MAY
   */
  public boolean isMay() {
    return isMonth(Month.MAY);
  }

  /**
   * Determines whether {@literal this} {@link Month} is {@link Month#JUNE}.
   *
   * @return a boolean value indicating whether {@literal this} {@link Month} is {@link Month#JUNE}.
   * @see #isMonth(Month)
   * @see Month#JUNE
   */
  public boolean isJune() {
    return isMonth(Month.JUNE);
  }

  /**
   * Determines whether {@literal this} {@link Month} is {@link Month#JULY}.
   *
   * @return a boolean value indicating whether {@literal this} {@link Month} is {@link Month#JULY}.
   * @see #isMonth(Month)
   * @see Month#JULY
   */
  public boolean isJuly() {
    return isMonth(Month.JULY);
  }

  /**
   * Determines whether {@literal this} {@link Month} is {@link Month#AUGUST}.
   *
   * @return a boolean value indicating whether {@literal this} {@link Month} is {@link Month#AUGUST}.
   * @see #isMonth(Month)
   * @see Month#AUGUST
   */
  public boolean isAugust() {
    return isMonth(Month.AUGUST);
  }

  /**
   * Determines whether {@literal this} {@link Month} is {@link Month#SEPTEMBER}.
   *
   * @return a boolean value indicating whether {@literal this} {@link Month} is {@link Month#SEPTEMBER}.
   * @see #isMonth(Month)
   * @see Month#SEPTEMBER
   */
  public boolean isSeptember() {
    return isMonth(Month.SEPTEMBER);
  }

  /**
   * Determines whether {@literal this} {@link Month} is {@link Month#OCTOBER}.
   *
   * @return a boolean value indicating whether {@literal this} {@link Month} is {@link Month#OCTOBER}.
   * @see #isMonth(Month)
   * @see Month#OCTOBER
   */
  public boolean isOctober() {
    return isMonth(Month.OCTOBER);
  }

  /**
   * Determines whether {@literal this} {@link Month} is {@link Month#NOVEMBER}.
   *
   * @return a boolean value indicating whether {@literal this} {@link Month} is {@link Month#NOVEMBER}.
   * @see #isMonth(Month)
   * @see Month#NOVEMBER
   */
  public boolean isNovember() {
    return isMonth(Month.NOVEMBER);
  }

  /**
   * Determines whether {@literal this} {@link Month} is {@link Month#DECEMBER}.
   *
   * @return a boolean value indicating whether {@literal this} {@link Month} is {@link Month#DECEMBER}.
   * @see #isMonth(Month)
   * @see Month#DECEMBER
   */
  public boolean isDecember() {
    return isMonth(Month.DECEMBER);
  }

  /**
   * Return a {@link String} representation of {@literal this} {@link Month}.
   *
   * @return a {@link String} describing {@literal this} {@link Month}.
   * @see #getName()
   */
  @Override
  public @NotNull String toString() {
    return getName();
  }
}
