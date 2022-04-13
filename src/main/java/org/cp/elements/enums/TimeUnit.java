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

import java.util.Arrays;
import java.util.function.Predicate;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * An {@link Enum enumeration} for units of time.
 *
 * @author John J. Blum
 * @see java.lang.Enum
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public enum TimeUnit {

  NANOSECOND("ns", "Nanosecond", "1 billionth of a second"),
  MICROSECOND("us", "Microsecond", "1 millionth of a second"),
  MILLISECOND("ms", "Millisecond", "1 thousandth of a second"),
  SECOND("s", "Second", "1 second"),
  MINUTE("mi", "Minute", "60 seconds"),
  HOUR("hr", "Hour", "60 minutes"),
  DAY("day", "Day", "24 hours"),
  WEEK("wk", "Week", "7 days"),
  MONTH("mon", "Month", "28-31 days"),
  YEAR("yr", "Year", "12 months, 365 days"),
  DECADE("dec", "Decade", "10 years"),
  SCORE("score", "Score", "20 years"),
  CENTURY("cent", "Century", "100 years"),
  MILLENNIA("millennia", "Millennia", "1000 years");

  /**
   * Factory method used to find a {@link TimeUnit} for the given {@link String abbreviation}.
   *
   * @param abbreviation {@link String} containing the {@literal abbreviation} used to find
   * and match the desired {@link TimeUnit}.
   * @return a {@link TimeUnit} matching the given {@link String abbreviation} or {@literal null}
   * if no {@link TimeUnit} is a match for the given {@link String abbreviation}.
   * @see #valueOf(Predicate)
   * @see #getAbbreviation()
   */
  public static @Nullable TimeUnit valueOfAbbreviation(@Nullable String abbreviation) {
    return valueOf(timeUnit -> timeUnit.getAbbreviation().equalsIgnoreCase(abbreviation));
  }

  /**
   * Factory method used to find a {@link TimeUnit} for the given {@link String name}.
   *
   * @param name {@link String} containing the {@literal name} used to find and match the desired {@link TimeUnit}.
   * @return a {@link TimeUnit} matching the given {@link String name} or {@literal null}
   * if no {@link TimeUnit} is a match for the given {@link String name}.
   * @see #valueOf(Predicate)
   * @see #getName()
   */
  public static @Nullable TimeUnit valueOfName(@Nullable String name) {
    return valueOf(timeUnit -> timeUnit.getName().equalsIgnoreCase(name));
  }

  /**
   * Factory method used to find a {@link TimeUnit} matching the given, required {@link Predicate}.
   *
   * @param predicate {@link Predicate} used to find and match the desired {@link TimeUnit};
   * must not be {@literal null}.
   * @return a {@link TimeUnit} matching the given, required {@link Predicate} or {@literal null}
   * if no {@link TimeUnit} is match for the given, required {@link Predicate}
   * @see java.util.function.Predicate
   * @see #values()
   */
  private static @Nullable TimeUnit valueOf(@NotNull Predicate<TimeUnit> predicate) {

    return Arrays.stream(values())
      .filter(predicate)
      .findFirst()
      .orElse(null);
  }

  private final String abbreviation;
  private final String description;
  private final String name;

  /**
   * Constructs a new instance of {@link TimeUnit} initialized with the given, required {@link String abbreviation},
   * {@link String name} and {@link String description} to describe the unit of time.
   *
   * @param abbreviation {@link String} specifying the {@literal abbreviation} for {@literal this} {@link TimeUnit}.
   * @param name {@link String} containing the {@literal name} of {@literal this} {@link TimeUnit}.
   * @param description {@link String} describing {@literal this} {@link TimeUnit}.
   */
  TimeUnit(String abbreviation, String name, String description) {
    this.abbreviation = abbreviation;
    this.name = name;
    this.description = description;
  }

  /**
   * Gets the {@link String abbreviation} for {@literal this} {@link TimeUnit}.
   *
   * @return the {@link String abbreviation} for {@literal this} {@link TimeUnit}.
   * @see #getName()
   */
  public String getAbbreviation() {
    return this.abbreviation;
  }

  /**
   * Gets a {@link String} describing {@literal this} {@link TimeUnit}.
   *
   * @return a {@link String} describing {@literal this} {@link TimeUnit}.
   */
  public String getDescription() {
    return this.description;
  }

  /**
   * Gets the {@link String name} of {@literal this} {@link TimeUnit}.
   *
   * @return the {@link String name} of {@literal this} {@link TimeUnit}.
   * @see #getAbbreviation()
   */
  public String getName() {
    return this.name;
  }

  /**
   * Return a {@link String} representation of {@literal this} {@link TimeUnit}.
   *
   * @return a {@link String} describing {@literal this} {@link TimeUnit}.
   * @see java.lang.Object#toString()
   * @see #getDescription()
   */
  @Override
  public String toString() {
    return getDescription();
  }
}
