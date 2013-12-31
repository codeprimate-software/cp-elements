/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.enums;

/**
 * The Country enum define constants for all the countries of the world.
 * <p/>
 * @author John J. Blum
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public enum Country {
  UNITED_STATES_OF_AMERICA("US", "United States of America");

  private final String abbreviation;
  private final String name;

  Country(final String abbreviation, final String name) {
    this.abbreviation = abbreviation;
    this.name = name;
  }

  public static Country valueOfAbbreviation(final String abbreviation) {
    for (final Country country : values()) {
      if (country.getAbbreviation().equalsIgnoreCase(abbreviation)) {
        return country;
      }
    }

    return null;
  }

  public static Country valueOfName(final String name) {
    for (final Country country : values()) {
      if (country.getName().equalsIgnoreCase(name)) {
        return country;
      }
    }

    return null;
  }

  public String getAbbreviation() {
    return abbreviation;
  }

  public String getName() {
    return name;
  }

  @Override
  public String toString() {
    return this.name;
  }

}
