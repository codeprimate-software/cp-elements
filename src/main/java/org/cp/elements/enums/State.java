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
 * The State enum defines constants (enumerated values) for all 50 of the United States of America.
 * <p/>
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
