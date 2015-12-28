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
 * The Gender enum defines constants (enumerated values) for the two sexes (male and female).
 *
 * @author John J. Blum
 * @see java.lang.Enum
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public enum Gender {
  FEMALE("F", "Female"),
  MALE("M", "Male");

  private final String abbreviation;
  private final String name;

  Gender(final String abbreviation, final String name) {
    this.abbreviation = abbreviation;
    this.name = name;
  }

  public static Gender valueOfAbbreviation(final String abbreviation) {
    for (Gender gender : values()) {
      if (gender.getAbbreviation().equalsIgnoreCase(abbreviation)) {
        return gender;
      }
    }

    return null;
  }

  public static Gender valueOfName(final String name) {
    for (Gender gender : values()) {
      if (gender.getName().equalsIgnoreCase(name)) {
        return gender;
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
