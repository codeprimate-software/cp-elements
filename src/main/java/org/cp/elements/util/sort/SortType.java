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

package org.cp.elements.util.sort;

/**
 * The SortType enum is an enumeration of different type of sorting algorithms.
 *
 * @author John J. Blum
 * @see java.lang.Enum
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public enum SortType {
  BUBBLE_SORT("BUBBLE", "Bubble Sort"),
  COMB_SORT("COMB", "Comb Sort"),
  HEAP_SORT("HEAP", "Heap Sort"),
  INSERTION_SORT("INSERTION", "Insertion Sort"),
  MERGE_SORT("MERGE", "Merge Sort"),
  QUICK_SORT("QUICK", "Quick Sort"),
  SELECTION_SORT("SELECTION", "Selection Sort"),
  SHELL_SORT("SHELL", "Shell Sort"),
  UNKONWN("UNKNOWN", "Unknown Sort");

  private final String abbreviation;
  private final String name;

  /**
   * Constructs an instance of the SortType enum initialized with a corresponding abbreviation and name.
   *
   * @param abbreviation a String specifying a sort type abbreviation.
   * @param name a String specifying the sort type name.
   */
  SortType(final String abbreviation, final String name) {
    this.abbreviation = abbreviation;
    this.name = name;
  }

  /**
   * Returns a SortType enumerated value for the given abbreviation or null if no match was found.
   *
   * @param abbreviation a String indicating the abbreviation to match the SortType.
   * @return a SortType enumerated value for the given abbreviation or null if no match was found.
   * @see #getAbbreviation()
   */
  public static SortType valueOfAbbreviation(final String abbreviation) {
    for (SortType value : values()) {
      if (value.getAbbreviation().equalsIgnoreCase(abbreviation)) {
        return value;
      }
    }

    return null;
  }

  /**
   * Returns a SortType enumerated value for the given name or null if no match was found.
   *
   * @param name a String indicating the name to match the SortType.
   * @return a SortType enumerated value for the given name or null if no match was found.
   * @see #getName()
   */
  public static SortType valueOfName(final String name) {
    for (SortType value : values()) {
      if (value.getName().equalsIgnoreCase(name)) {
        return value;
      }
    }

    return null;
  }

  /**
   * Gets the abbreviation for this SortType enumerated value.
   *
   * @return a String specifying the abbreviation of this SortType.
   */
  public String getAbbreviation() {
    return abbreviation;
  }

  /**
   * Gets the name for this SortType enumerated value.
   *
   * @return a String specifying the name of this SortType.
   */
  public String getName() {
    return name;
  }

  /**
   * Gets a String description of this SortType enumerated value.
   *
   * @return a String describing this SortType.
   */
  @Override
  public String toString() {
    return this.name;
  }

}
