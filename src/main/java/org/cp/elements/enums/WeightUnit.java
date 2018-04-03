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

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.StringUtils;

/**
 * The {@link WeightUnit} enum is an {@link Enum enumeration} of weights.
 *
 * @author John Blum
 * @see java.lang.Enum
 * @since 1.0.0
 */
@SuppressWarnings("unused")
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
   * Returns {@link WeightUnit#OUNCE} if this is the {@literal United States},
   * otherwise returns {@link WeightUnit#GRAM}.
   *
   * @return the default {@link WeightUnit} based in the current, default {@link Locale}.
   * @see java.util.Locale#getCountry()
   */
  public static WeightUnit getDefault() {

    return Optional.of(Locale.getDefault().getCountry())
      .filter(Locale.US.getCountry()::equals)
      .map(it -> WeightUnit.OUNCE)
      .orElse(WeightUnit.GRAM);
  }

  /**
   * Factory method used to find or lookup a unit of weight by {@link String abbreviation}.
   *
   * @param abbreviation {@link String} containing the abbreviation of the {@link WeightUnit}.
   * @return a {@link WeightUnit} with the given {@link String abbreviation} or {@literal null}
   * if no {@link LengthUnit} with the given {@link String abbreviation} exists.
   * @see #getAbbreviation()
   * @see #values()
   */
  public static WeightUnit valueOfAbbreviation(String abbreviation) {

    return Arrays.stream(values())
      .filter(it -> it.getAbbreviation().equalsIgnoreCase(String.valueOf(abbreviation).trim()))
      .findFirst()
      .orElse(null);
  }

  /**
   * Factory method used to find or lookup a unit of weight by {@link String name}.
   *
   * @param name {@link String} containing the name of the {@link WeightUnit}.
   * @return a {@link WeightUnit} with the given {@link String name} or {@literal null}
   * if no {@link LengthUnit} with the given {@link String name} exists.
   * @see #values()
   * @see #name()
   */
  public static WeightUnit valueOfName(String name) {

    return Arrays.stream(values())
      .filter(it -> it.name().equalsIgnoreCase(String.valueOf(name).trim()))
      .findFirst()
      .orElse(null);
  }

  private final String abbreviation;

  /**
   * Constructs a new instance of {@link WeightUnit} initialized with the given {@link String abbreviation}.
   *
   * @param abbreviation {@link String} containing the abbreviation for this {@link WeightUnit}.
   * @throws IllegalArgumentException if {@link String abbreviation} is {@literal null} or empty.
   */
  WeightUnit(String abbreviation) {

    Assert.hasText(abbreviation, "Abbreviation is required");

    this.abbreviation = abbreviation;
  }

  /**
   * Returns the {@link String abbreviation} for this {@link WeightUnit}.
   *
   * @return a {@link String} containing the abbreviation for this {@link WeightUnit}.
   */
  public String getAbbreviation() {
    return this.abbreviation;
  }

  /**
   * Returns the pluralized {@link #name()} of this {@link WeightUnit} enumerated value.
   *
   * @return the pluralized {@link #name()} of this {@link WeightUnit} enumerated value.
   * @see #name()
   */
  public String getPluralName() {
    return name().concat("S");
  }

  /**
   * Returns a {@link String} representation of this {@link WeightUnit}.
   *
   * @return a {@link String} describing this {@link WeightUnit}.
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    return StringUtils.capitalize(name().toLowerCase());
  }
}
