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

/**
 * The {@link Order} enum is an {@link Enum enumeration} for sort order, or order by.
 *
 * @author John J. Blum
 * @see java.lang.Enum
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public enum Order {

  ASCENDING("ASC", "Ascending"),
  DESCENDING("DESC", "Descending");

  public static Order valueOfAbbreviation(String abbreviation) {

    return Arrays.stream(values())
      .filter(order -> order.getAbbreviation().equalsIgnoreCase(abbreviation))
      .findFirst()
      .orElse(null);
  }

  public static Order valueOfName(String name) {

    return Arrays.stream(values())
      .filter(order -> order.getName().equalsIgnoreCase(name))
      .findFirst()
      .orElse(null);
  }

  private final String abbreviation;
  private final String name;

  Order(String abbreviation, String name) {
    this.abbreviation = abbreviation;
    this.name = name;
  }

  public String getAbbreviation() {
    return this.abbreviation;
  }

  public String getName() {
    return this.name;
  }

  @Override
  public String toString() {
    return getName();
  }
}
