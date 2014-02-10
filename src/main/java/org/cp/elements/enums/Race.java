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
 * The Race enum defines constants (enumerated values) for different ethnicity and nationality.
 * <p/>
 * @author John J. Blum
 * @see java.lang.Enum
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public enum Race {
  AFRICAN_AMERICAN("Black", "African American"),
  ALASKAN_NATIVE("Eskimo", "Alaskan Native"),
  ASIAN("Asian", "Asian"),
  EUROPEAN("European", "European"),
  HISPANIC("Hispanic", "Hispanic"),
  INDIAN("Indi", "Indian"),
  NATIVE_AMERICAN("Indian", "Native American"),
  WHITE("White", "White");

  private final String abbreviation;
  private final String name;

  Race(final String abbreviation, final String name) {
    this.abbreviation = abbreviation;
    this.name = name;
  }

  public static Race valueOfAbbreviation(final String abbreviation) {
    for (Race race : values()) {
      if (race.getAbbreviation().equalsIgnoreCase(abbreviation)) {
        return race;
      }
    }

    return null;
  }

  public static Race valueOfName(final String name) {
    for (Race race : values()) {
      if (race.getName().equalsIgnoreCase(name)) {
        return race;
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
