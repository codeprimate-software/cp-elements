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

import org.cp.elements.lang.StringUtils;

/**
 * The {@link Distance} enum is an {@link Enum enumeration} of distances or length.
 *
 * @author John Blum
 * @see java.lang.Enum
 * @since 1.0.0
 */
public enum Distance {

  NANOMETER("nm"),
  MICROMETER("um"),
  MILLIMETER("mm"),
  CENTIMETER("cm"),
  METER("m"),
  KILOMETER("km"),
  INCH("in"),
  FOOT("ft"),
  YARD("yd");

  /**
   * Factory method used to find or lookup a {@link Distance} by {@link String abbreviation}.
   *
   * @param abbreviation {@link String} containing the abbreviation of the {@link Distance}.
   * @return the {@link Distance} for the given {@link String abbreviation} or {@literal null}
   * if no {@link Distance} with the given {@link String abbreviation} exists.
   * @see #getAbbreviation()
   * @see #values()
   */
  public static Distance valueOfAbbreviation(String abbreviation) {

    return Arrays.stream(values())
      .filter(distance -> distance.getAbbreviation().equalsIgnoreCase(abbreviation))
      .findFirst()
      .orElse(null);
  }

  /**
   * Factory method used to find or lookup a {@link Distance} by {@link String name}.
   *
   * This operation is case-insensitive.
   *
   * @param name {@link String} containing the name of the {@link Distance} to find.
   * @return the {@link Distance} for the given {@link String name} or {@literal null}
   * if no {@link Distance} with the given {@link String name} exists.
   * @see #values()
   * @see #name()
   */
  public static Distance valueOfName(String name) {

    return Arrays.stream(values())
      .filter(distance -> distance.name().equalsIgnoreCase(name))
      .findFirst()
      .orElse(null);
  }

  private final String abbreviation;

  /**
   * Construct a new instance of {@link Distance} initialized with the given {@link String abbreviation}.
   *
   * @param abbreviation {@link String} containing the abbreviation for this {@link Distance}.
   */
  Distance(String abbreviation) {
    this.abbreviation = abbreviation;
  }

  /**
   * Returns the {@link String abbreviation} for this {@link Distance}.
   *
   * @return the {@link String abbreviation} for this {@link Distance}.
   * @see java.lang.String
   */
  public String getAbbreviation() {
    return this.abbreviation;
  }

  /**
   * Returns a {@link String} representation of this {@link Distance}.
   *
   * @return a {@link String} describing this {@link Distance}.
   * @see java.lang.Object#toString()
   * @see #name()
   */
  @Override
  public String toString() {
    return StringUtils.capitalize(name().toLowerCase());
  }
}
