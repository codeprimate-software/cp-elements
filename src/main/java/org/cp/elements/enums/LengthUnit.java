/*
 * Copyright 2016 Author or Authors.
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

import org.cp.elements.lang.StringUtils;

/**
 * The {@link LengthUnit} enum is an {@link Enum enumeration} of distances or length.
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
   * Factory method used to get the default {@link LengthUnit} based in the current, default {@link Locale}.
   *
   * @return the default {@link LengthUnit} based in current, default {@link Locale}.
   * @see java.util.Locale#getCountry()
   */
  public static LengthUnit getDefault() {

    return Optional.of(Locale.getDefault().getCountry())
      .filter(Locale.US.getCountry()::equals)
      .map(country -> LengthUnit.FOOT)
      .orElse(LengthUnit.METER);
  }

  /**
   * Factory method used to find or lookup a {@link LengthUnit} by {@link String abbreviation}.
   *
   * @param abbreviation {@link String} containing the abbreviation of the {@link LengthUnit}.
   * @return the {@link LengthUnit} for the given {@link String abbreviation} or {@literal null}
   * if no {@link LengthUnit} with the given {@link String abbreviation} exists.
   * @see #getAbbreviation()
   * @see #values()
   */
  public static LengthUnit valueOfAbbreviation(String abbreviation) {

    return Arrays.stream(values())
      .filter(distance -> distance.getAbbreviation().equals(abbreviation))
      .findFirst()
      .orElse(null);
  }

  /**
   * Factory method used to find or lookup a {@link LengthUnit} by {@link String name}.
   *
   * This operation is case-insensitive.
   *
   * @param name {@link String} containing the name of the {@link LengthUnit} to find.
   * @return the {@link LengthUnit} for the given {@link String name} or {@literal null}
   * if no {@link LengthUnit} with the given {@link String name} exists.
   * @see #values()
   * @see #name()
   */
  public static LengthUnit valueOfName(String name) {

    return Arrays.stream(values())
      .filter(distance -> distance.name().equalsIgnoreCase(name))
      .findFirst()
      .orElse(null);
  }

  private final String abbreviation;

  /**
   * Construct a new instance of {@link LengthUnit} initialized with the given {@link String abbreviation}.
   *
   * @param abbreviation {@link String} containing the abbreviation for this {@link LengthUnit}.
   */
  LengthUnit(String abbreviation) {
    this.abbreviation = abbreviation;
  }

  /**
   * Returns the {@link String abbreviation} for this {@link LengthUnit}.
   *
   * @return the {@link String abbreviation} for this {@link LengthUnit}.
   * @see java.lang.String
   */
  public String getAbbreviation() {
    return this.abbreviation;
  }

  /**
   * Returns the pluralized {@link #name()} of this {@link LengthUnit} enumerated value.
   *
   * @return the pluralized {@link #name()} of this {@link LengthUnit} enumerated value.
   * @see #name()
   */
  public String getPluralName() {
    return this == FOOT ? "FEET" : this == INCH ? "INCHES" : name().concat("S");
  }

  /**
   * Returns a {@link String} representation of this {@link LengthUnit}.
   *
   * @return a {@link String} describing this {@link LengthUnit}.
   * @see java.lang.Object#toString()
   * @see #name()
   */
  @Override
  public String toString() {
    return StringUtils.capitalize(name().toLowerCase());
  }
}
