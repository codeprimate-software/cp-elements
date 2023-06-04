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

import org.cp.elements.function.FunctionUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * An {@link Enum Enumeration} for different races as defined by the OMB standards.
 *
 * @author John J. Blum
 * @see java.lang.Enum
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public enum Race {

  AFRICAN_AMERICAN("Black", "African American"),
  ALASKA_NATIVE("Alaskan", "Alaska Native"),
  AMERICAN_INDIAN("Indian", "American Indian/Native American"),
  ASIAN("Asian", "Asian"),
  NATIVE_HAWAIIAN("Hawaiian", "Native Hawaiian/Other Pacific Islander"),
  WHITE("White", "White");

  /**
   * Factory method used to find and match a {@link Race} by {@link String abbreviation}.
   *
   * @param abbreviation {@link String} containing the {@literal abbreviation} used to
   * find and match a {@link Race} enum.
   * @return a {@link Race} matching the given {@link String abbreviation} or {@literal null}
   * if no {@link Race} matches the given {@link String abbreviation}.
   * @see #valueOf(Predicate)
   * @see #getAbbreviation()
   */
  public static @Nullable Race valueOfAbbreviation(@Nullable String abbreviation) {
    return valueOf(race -> race.getAbbreviation().equalsIgnoreCase(abbreviation));
  }

  /**
   * Factory method used to find and match a {@link Race} by {@link String name}.
   *
   * @param name {@link String} containing the {@literal name} used to find and match a {@link Race} enum.
   * @return a {@link Race} matching the given {@link String name} or {@literal null}
   * if no {@link Race} matches the given {@link String name}.
   * @see #valueOf(Predicate)
   * @see #getName()
   */
  public static @Nullable Race valueOfName(@Nullable String name) {
    return valueOf(race -> race.getName().equalsIgnoreCase(name));
  }

  /**
   * Factory method used to find and match a {@link Race} by the given, required {@link Predicate}.
   *
   * @param predicate {@link Predicate} used to find and match a {@link Race} enumerated value.
   * @return a {@link Race} matching the given, required {@link Predicate} or {@literal null}
   * if no {@link Race} is a match for the given, required {@link Predicate}.
   * @see java.util.function.Predicate
   * @see #values()
   */
  private static @Nullable Race valueOf(@NotNull Predicate<Race> predicate) {

    return Arrays.stream(values())
      .filter(FunctionUtils.nullSafePredicateMatchNone(predicate))
      .findFirst()
      .orElse(null);
  }

  private final String abbreviation;
  private final String name;

  /**
   * Constructs a new {@link Race} initialized with the given {@link String abbreviation}
   * and {@link String name} for the {@link Race}.
   *
   * @param abbreviation {@link String} specifying the {@literal abbreviation} for {@literal this} {@link Race}.
   * @param name {@link String} containing the {@literal name} of {@literal this} {@link Race}.
   */
  Race(@NotNull String abbreviation, @NotNull String name) {
    this.abbreviation = abbreviation;
    this.name = name;
  }

  /**
   * Get the {@link String abbreviation} for {@literal this} {@link Race}.
   *
   * @return the {@link String abbreviation} for {@literal this} {@link Race}.
   * @see #getName()
   */
  public String getAbbreviation() {
    return this.abbreviation;
  }

  /**
   * Get the {@link String name} of {@literal this} {@link Race}.
   *
   * @return the {@link String name} of {@literal this} {@link Race}.
   * @see #getAbbreviation()
   */
  public String getName() {
    return this.name;
  }

  /**
   * Returns a {@link String} representation for {@literal this} {@link Race}.
   *
   * @return a {@link String} describing {@literal this} {@link Race}.
   * @see java.lang.Object#toString()
   * @see #getName()
   */
  @Override
  public String toString() {
    return getName();
  }
}
