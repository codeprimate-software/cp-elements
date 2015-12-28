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

/**
 * The Race enum defines constants (enumerated values) for different ethnicity and nationality.
 *
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
