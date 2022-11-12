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
 * An {@link Enum Enumeration} of length, height or distance measurements.
 *
 * @author John Blum
 * @see java.lang.Enum
 * @see <a href="https://en.wikipedia.org/wiki/Unit_of_length">Unit of length</a>
 * @since 1.0.0
 */
public enum LengthUnit {

  YOCTOMETER("ym"),
  ZEPTOMETER("zm"),
  ATTOMETER("am"),
  FEMTOMETER("fm"),
  PICOMETER("pm"),
  NANOMETER("nm"),
  MICROMETER("um"),
  MILLIMETER("mm"),
  CENTIMETER("cm"),
  DECIMETER("dm"),
  METER("m"),
  DECAMETER("da"),
  HECTOMETER("hm"),
  KILOMETER("km"),
  MEGAMETER("MM"),
  GIGAMETER("GM"),
  TERAMETER("TM"),
  PETAMETER("PM"),
  EXAMETER("EM"),
  ZETAMETER("ZM"),
  YOTTAMETER("YM"),
  INCH("in"),
  FOOT("ft"),
  YARD("yd"),
  MILE("mi"),
  LIGHT_YEAR("ly");

  /**
   * Factory method used to get the default unit of length based in the current, default {@link Locale}.
   *
   * Returns {@link LengthUnit#FOOT} if this is the {@literal United States of America (USA)},
   * otherwise returns {@link LengthUnit#METER} for all other countries.
   *
   * @return the default {@link LengthUnit} based in the current, default {@link Locale}.
   * @see java.util.Locale#getCountry()
   * @see java.util.Locale#getDefault()
   */
  public static @NotNull LengthUnit getDefault() {

    return Optional.of(Locale.getDefault().getCountry())
      .filter(Locale.US.getCountry()::equals)
      .map(country -> LengthUnit.FOOT)
      .orElse(LengthUnit.METER);
  }

  /**
   * Factory method used to find and match a {@link LengthUnit} by {@link String abbreviation}.
   *
   * @param abbreviation {@link String} containing the {@literal abbreviation} of the {@link LengthUnit} to find.
   * @return the {@link LengthUnit} for the given {@link String abbreviation}, or {@literal null}
   * if no {@link LengthUnit} with the given {@link String abbreviation} exists.
   * @see #valueOf(Predicate)
   * @see #getAbbreviation()
   */
  public static @Nullable LengthUnit valueOfAbbreviation(@Nullable String abbreviation) {
    return valueOf(length -> length.getAbbreviation().equals(abbreviation));
  }

  /**
   * Factory method used to find and match a {@link LengthUnit} by {@link String name}.
   *
   * This operation is case-insensitive.
   *
   * @param name {@link String} containing the {@literal name} of the {@link LengthUnit} to find.
   * @return the {@link LengthUnit} with the given {@link String name}, or {@literal null}
   * if no {@link LengthUnit} with the given {@link String name} exists.
   * @see #valueOf(Predicate)
   * @see #name()
   */
  public static @Nullable LengthUnit valueOfName(@Nullable String name) {
    return valueOf(length -> length.name().equalsIgnoreCase(name));
  }

  /**
   * Factory method used to find and match a {@link LengthUnit} by the given, required {@link Predicate}.
   *
   * @param predicate {@link Predicate} used to find and match the {@link LengthUnit}; must not be {@literal null}.
   * @return a {@link LengthUnit} matching the given, required {@link Predicate} or {@literal null}
   * if no {@link LengthUnit} is a match for the the given, required {@link Predicate}.
   * @see java.util.function.Predicate
   * @see #values()
   */
  private static @Nullable LengthUnit valueOf(@NotNull Predicate<LengthUnit> predicate) {

    return Arrays.stream(values())
      .filter(FunctionUtils.nullSafePredicateMatchNone(predicate))
      .findFirst()
      .orElse(null);
  }

  private final String abbreviation;

  /**
   * Construct a new instance of {@link LengthUnit} initialized with the given, required {@link String abbreviation}.
   *
   * @param abbreviation {@link String} containing the abbreviation for {@literal this} {@link LengthUnit}.
   */
  LengthUnit(@NotNull String abbreviation) {
    this.abbreviation = abbreviation;
  }

  /**
   * Gets the {@link String abbreviation} for {@literal this} {@link LengthUnit}.
   *
   * @return the {@link String abbreviation} for {@literal this} {@link LengthUnit}.
   * @see java.lang.String
   */
  public @NotNull String getAbbreviation() {
    return this.abbreviation;
  }

  /**
   * Returns the pluralized {@link #name()} of this {@link LengthUnit} enumerated value.
   *
   * @return the pluralized {@link #name()} of this {@link LengthUnit} enumerated value.
   * @see #name()
   */
  public @NotNull String getPluralName() {
    return this == FOOT ? "FEET" : this == INCH ? "INCHES" : name().concat("S");
  }

  /**
   * Returns a {@link String} representation for {@literal this} {@link LengthUnit}.
   *
   * @return a {@link String} describing {@literal this} {@link LengthUnit}.
   * @see java.lang.Object#toString()
   * @see #name()
   */
  @Override
  public String toString() {
    return StringUtils.capitalize(name().toLowerCase());
  }
}
