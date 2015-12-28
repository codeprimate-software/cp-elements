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
 * The State enum defines constants (enumerated values) for all 50 of the United States of America.
 *
 * @author John J. Blum
 * @see java.lang.Enum
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public enum State {
  ALABAMA("AL", "Alabama"),
  ALASKA("AK", "Alaska"),
  ARIZONA("AZ", "Arizona"),
  ARKANSAS("AR", "Arkansas"),
  CALIFORNIA("CA", "California"),
  COLORADO("CO", "Colarado"),
  CONNECTICUT("CT", "Connecticut"),
  DELAWARE("DE", "Delaware"),
  FLORIDA("FL", "Florida"),
  GEORGIA("GA", "Georgia"),
  HAWAII("HI", "Hawaii"),
  IDAHO("ID", "Idaho"),
  ILLINOIS("IL", "Illinois"),
  INDIANA("IN", "Indiana"),
  IOWA("IA", "Iowa"),
  KANSAS("KA", "Kansas"),
  KENTUCKY("KY", "Kentucky"),
  LOUISIANA("LA", "Louisiana"),
  MAINE("ME", "Maine"),
  MARYLAND("MD", "Maryland"),
  MASSACHUSETTS("MA", "Massachusetts"),
  MICHIGAN("MI", "Michigan"),
  MINNESOTA("MN", "Minnesota"),
  MISSISSIPPI("MS", "Mississippi"),
  MISSOURI("MO", "Missouri"),
  MONTANA("MT", "Montana"),
  NEBRASKA("NE", "Nebraska"),
  NEVADA("NV", "Nevada"),
  NEW_HAMPSHIRE("NH", "New Hampshire"),
  NEW_JERSEY("NJ", "New Jersey"),
  NEW_MEXICO("NM", "New Mexico"),
  NEW_YORK("NY", "New York"),
  NORTH_CAROLINA("NC", "North Carolina"),
  NORTH_DAKOTA("ND", "North Dakota"),
  OHIO("OH", "Ohio"),
  OKLAHOMA("OK", "Oklahoma"),
  OREGON("OR", "Oregon"),
  PENNSYLVANIA("PA", "Pennsylvannia"),
  RHODE_ISLAND("RI", "Rhode Island"),
  SOUTH_CAROLINA("SC", "South Carolina"),
  SOUTH_DAKOTA("SD", "South Dakota"),
  TENNESSEE("TN", "Tennessee"),
  TEXAS("TX", "Texas"),
  UTAH("UT", "Utah"),
  VERMONT("VT", "Vermont"),
  VIRGINIA("VA", "Virginia"),
  WASHINGTON("WA", "Washington"),
  WEST_VIRGINIA("WV", "West Virginia"),
  WISCONSIN("WI", "Wisconsin"),
  WYOMING("WY", "Wyoming");

  private final String abbreviation;
  private final String name;

  State(final String abbreviation, final String name) {
    this.abbreviation = abbreviation;
    this.name = name;
  }

  public static State valueOfAbbreviation(final String abbreviation) {
    for (State state : values()) {
      if (state.getAbbreviation().equalsIgnoreCase(abbreviation)) {
        return state;
      }
    }

    return null;
  }

  public static State valueOfName(final String name) {
    for (State state : values()) {
      if (state.getName().equalsIgnoreCase(name)) {
        return state;
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
