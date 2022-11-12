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
import java.util.Locale;
import java.util.Optional;
import java.util.function.Predicate;

import org.cp.elements.function.FunctionUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * An {@link Enum Enumeration} of weight measurements.
 *
 * @author John Blum
 * @see java.lang.Enum
 * @since 1.0.0
 */
public enum WeightUnit {

  MILLIGRAM("mg"),
  GRAM("g"),
  KILOGRAM("kg"),
  OUNCE("ou"),
  POUND("lb"),
  TON("t");

  /**
   * Factory method used to get the default unit of weight based in the current, default {@link Locale}.
   *
   * Returns {@link WeightUnit#OUNCE} if this is the {@literal United States of America (USA)},
   * otherwise returns {@link WeightUnit#GRAM}.
   *
   * @return the default {@link WeightUnit} based in the current, default {@link Locale}.
   * @see java.util.Locale#getCountry()
   * @see java.util.Locale#getDefault()
   */
  public static @NotNull WeightUnit getDefault() {

    return Optional.of(Locale.getDefault().getCountry())
      .filter(Locale.US.getCountry()::equals)
      .map(it -> WeightUnit.OUNCE)
      .orElse(WeightUnit.GRAM);
  }

  /**
   * Factory method used to find and match a {@link WeightUnit} by {@link String abbreviation}.
   *
   * @param abbreviation {@link String} containing the {@literal abbreviation} of the {@link WeightUnit} to find.
   * @return a {@link WeightUnit} with the given {@link String abbreviation}, or {@literal null}
   * if no {@link WeightUnit} with the given {@link String abbreviation} exists.
   * @see #valueOf(Predicate)
   * @see #getAbbreviation()
   */
  public static @Nullable WeightUnit valueOfAbbreviation(@Nullable String abbreviation) {
    return valueOf(weight -> weight.getAbbreviation().equalsIgnoreCase(String.valueOf(abbreviation).trim()));
  }

  /**
   * Factory method used to find and match a {@link WeightUnit} by {@link String name}.
   *
   * @param name {@link String} containing the {@literal name} of the {@link WeightUnit} to find.
   * @return a {@link WeightUnit} with the given {@link String name}, or {@literal null}
   * if no {@link WeightUnit} with the given {@link String name} exists.
   * @see #valueOf(Predicate)
   * @see #name()
   */
  public static @Nullable WeightUnit valueOfName(@Nullable String name) {
    return valueOf(weight -> weight.name().equalsIgnoreCase(String.valueOf(name).trim()));
  }

  /**
   * Factory method used to find and match a {@link WeightUnit} matching the given, required {@link Predicate}.
   *
   * @param predicate {@link Predicate} used to find and match a {@link WeightUnit}; must not be {@literal null}.
   * @return a {@link WeightUnit} matching the given, required {@link Predicate} or {@literal null}
   * if not {@link WeightUnit} is a match for the given, required {@link Predicate}.
   * @see java.util.function.Predicate
   * @see #values()
   */
  private static @Nullable WeightUnit valueOf(@NotNull Predicate<WeightUnit> predicate) {

    return Arrays.stream(values())
      .filter(FunctionUtils.nullSafePredicateMatchNone(predicate))
      .findFirst()
      .orElse(null);
  }

  private final String abbreviation;

  /**
   * Constructs a new instance of {@link WeightUnit} initialized with the given, required {@link String abbreviation}.
   *
   * @param abbreviation {@link String} containing the abbreviation for {@literal this} {@link WeightUnit}.
   */
  WeightUnit(@NotNull String abbreviation) {
    this.abbreviation = abbreviation;
  }

  /**
   * Gets the {@link String abbreviation} for {@literal this} {@link WeightUnit}.
   *
   * @return a {@link String} containing the abbreviation for {@literal this} {@link WeightUnit}.
   */
  public @NotNull String getAbbreviation() {
    return this.abbreviation;
  }

  /**
   * Gets the pluralized {@link #name()} for {@literal this} {@link WeightUnit}.
   *
   * @return the pluralized {@link #name()} for {@literal this} {@link WeightUnit}.
   * @see #name()
   */
  public @NotNull String getPluralName() {
    return name().concat("S");
  }

  /**
   * Returns a {@link String} representation for {@literal this} {@link WeightUnit}.
   *
   * @return a {@link String} describing {@literal this} {@link WeightUnit}.
   * @see java.lang.Object#toString()
   * @see #name()
   */
  @Override
  public String toString() {
    return StringUtils.capitalize(name().toLowerCase());
  }
}
