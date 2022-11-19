/*
 * Copyright 2011-Present Author or Authors.
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
package org.cp.elements.util.search;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * An {@link Enum Enumeration} of different types of search algorithms.
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
  SearchType(@NotNull String abbreviation, @NotNull String name) {

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
  public static @Nullable SearchType valueOfAbbreviation(@Nullable String abbreviation) {

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
  public static @Nullable SearchType valueOfName(@Nullable String name) {

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
  public @NotNull String getAbbreviation() {
    return this.abbreviation;
  }

  /**
   * Gets the name for this SearchType enumerated value.
   *
   * @return a String specifying the name of this SearchType.
   */
  public @NotNull String getName() {
    return this.name;
  }

  /**
   * Gets a String description of this SearchType enumerated value.
   *
   * @return a String describing this SearchType.
   */
  @Override
  public @NotNull String toString() {
    return getName();
  }
}
