/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * 
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * 
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * 
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * 
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.util.search;

/**
 * The SearchType enum is an enumeration of different type of searching algorithms.
 * 
 * @author John J. Blum
 * @see java.lang.Enum
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public enum SearchType {
  BINARY_SEARCH("BINARY", "Binary Search"),
  INDEX_SEARCH("INDEX", "Index Search"),
  LINEAR_SEARCH("LINEAR", "Linear Search"),
  UNKNOWN_SEARCH("UNKNOWN", "Unknown Search");

  private final String abbreviation;
  private final String name;

  /**
   * Constructs an instance of the SearchType enum initialized with a corresponding abbreviation and name.
   *
   * @param abbreviation a String specifying a search type abbreviation.
   * @param name a String specifying the search type name.
   */
  SearchType(final String abbreviation, final String name) {
    this.abbreviation = abbreviation;
    this.name = name;
  }

  /**
   * Returns a SearchType enumerated value for the given abbreviation or null if no match was found.
   *
   * @param abbreviation a String indicating the abbreviation to match the SearchType.
   * @return a SearchType enumerated value for the given abbreviation or null if no match was found.
   * @see #getAbbreviation()
   */
  public static SearchType valueOfAbbreviation(final String abbreviation) {
    for (SearchType value : values()) {
      if (value.getAbbreviation().equalsIgnoreCase(abbreviation)) {
        return value;
      }
    }

    return null;
  }

  /**
   * Returns a SearchType enumerated value for the given name or null if no match was found.
   *
   * @param name a String indicating the name to match the SearchType.
   * @return a SearchType enumerated value for the given name or null if no match was found.
   * @see #getName()
   */
  public static SearchType valueOfName(final String name) {
    for (SearchType value : values()) {
      if (value.getName().equalsIgnoreCase(name)) {
        return value;
      }
    }

    return null;
  }

  /**
   * Gets the abbreviation for this SearchType enumerated value.
   *
   * @return a String specifying the abbreviation of this SearchType.
   */
  public String getAbbreviation() {
    return abbreviation;
  }

  /**
   * Gets the name for this SearchType enumerated value.
   *
   * @return a String specifying the name of this SearchType.
   */
  public String getName() {
    return name;
  }

  /**
   * Gets a String description of this SearchType enumerated value.
   *
   * @return a String describing this SearchType.
   */
  @Override
  public String toString() {
    return this.name;
  }

}
