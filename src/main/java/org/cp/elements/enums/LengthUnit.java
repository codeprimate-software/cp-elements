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

  YOCTOMETER("ym", Math.pow(10, -24)),
  ZEPTOMETER("zm", Math.pow(10, -21)),
  ATTOMETER("am", Math.pow(10, -18)),
  FEMTOMETER("fm", Math.pow(10, -15)),
  PICOMETER("pm", Math.pow(10, -12)),
  NANOMETER("nm", Math.pow(10, -9)),
  MICROMETER("um", Math.pow(10, -6)),
  MILLIMETER("mm", 0.001d),
  CENTIMETER("cm", 0.01d),
  DECIMETER("dm", 0.10d),
  METER("m", 1.0d),
  DECAMETER("da", 10.0d),
  HECTOMETER("hm", 100.0d),
  KILOMETER("km", 1_000.0d),
  MEGAMETER("MM", 1_000_000d),
  GIGAMETER("GM", 1_000_000_000d),
  TERAMETER("TM", Math.pow(10, 12)),
  PETAMETER("PM", Math.pow(10, 15)),
  EXAMETER("EM", Math.pow(10, 18)),
  ZETAMETER("ZM", Math.pow(10, 21)),
  YOTTAMETER("YM", Math.pow(10, 24)),
  INCH("in", 0.0254d),
  FOOT("ft", 0.3048d),
  YARD("yd", 0.9144),
  MILE("mi", 1609.34),
  LIGHT_YEAR("ly", 9.460528405d * Math.pow(10, 15));

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

  private final double meterConversionFactor;

  private final String abbreviation;

  /**
   * Construct a new instance of {@link LengthUnit} initialized with the given, required {@link String abbreviation}.
   *
   * @param abbreviation {@link String} containing the abbreviation for {@literal this} {@link LengthUnit}.
   */
  LengthUnit(@NotNull String abbreviation, double meterConversionFactor) {

    this.abbreviation = abbreviation;
    this.meterConversionFactor = meterConversionFactor;
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
   * Gets the {@link Double} conversion factor used to convert measurements in this {@link LengthUnit}
   * to {@link LengthUnit#METER meters}.
   *
   * @return the {@link Double} conversion factor used to convert measurements in this {@link LengthUnit}
   * to {@link LengthUnit#METER meters}.
   */
  public double getMeterConversionFactor() {
    return this.meterConversionFactor;
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
