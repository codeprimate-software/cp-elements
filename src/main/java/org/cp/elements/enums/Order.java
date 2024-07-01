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
package org.cp.elements.enums;

import java.util.Arrays;
import java.util.function.Predicate;

import org.cp.elements.function.FunctionUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * An {@link Enum Enumeration} of {@literal sort orders} used in {@literal ORDER BY} clauses of query statements.
 *
 * @author John J. Blum
 * @see java.lang.Enum
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public enum Order {

  ASCENDING("ASC", "Ascending"),
  DESCENDING("DESC", "Descending");

  /**
   * Factory method used to find and match an {@link Order} by {@link String abbreviation}.
   *
   * @param abbreviation {@link String} containing the {@literal abbreviation} of the {@link Order} to find.
   * @return an {@link Order} matching the given {@link String abbreviation} or {@literal null}
   * if no {@link Order} matches the given {@link String abbreviation}.
   * @see #valueOfPredicate(Predicate)
   * @see #getAbbreviation()
   */
  public static @Nullable Order valueOfAbbreviation(@Nullable String abbreviation) {
    return valueOfPredicate(order -> order.getAbbreviation().equalsIgnoreCase(abbreviation));
  }

  /**
   * Factory method used to find and match an {@link Order} by {@link String name}.
   *
   * @param name {@link String} containing the {@literal name} of the {@link Order} to find.
   * @return an {@link Order} matching the given {@link String name} or {@literal null}
   * if no {@link Order} matches by the given {@link String name}.
   * @see #valueOfPredicate(Predicate)
   * @see #getName()
   */
  public static @Nullable Order valueOfName(@Nullable String name) {
    return valueOfPredicate(order -> order.getName().equalsIgnoreCase(name));
  }

  /**
   * Factory method used to find and match an {@link Order} by the given, required {@link Predicate}.
   *
   * @param predicate {@link Predicate} used to find and match an {@link Order}.
   * @return an {@link Order} matching the given {@link Predicate} or {@literal null}
   * if no {@link Order} matches the given {@link Predicate}.
   * @see java.util.function.Predicate
   * @see #values()
   */
  private static @Nullable Order valueOfPredicate(@NotNull Predicate<Order> predicate) {

    return Arrays.stream(values())
      .filter(FunctionUtils.nullSafePredicateMatchingNone(predicate))
      .findFirst()
      .orElse(null);
  }

  private final String abbreviation;
  private final String name;

  /**
   * Constructs a new {@link Order} initialized with the given metadata and descriptors.
   *
   * @param abbreviation {@link String} specifying the {@literal abbreviation} of {@literal this} {@link Order}.
   * @param name {@link String} containing the {@literal name} of {@literal this} {@link Order}.
   */
  Order(@NotNull String abbreviation, @NotNull String name) {

    this.abbreviation = abbreviation;
    this.name = name;
  }

  /**
   * Get the {@link String abbreviation} for {@literal this} {@link Order}.
   *
   * @return the {@link String abbreviation} for {@literal this} {@link Order}.
   * @see #getName()
   */
  public String getAbbreviation() {
    return this.abbreviation;
  }

  /**
   * Get the {@link String name} of {@literal this} {@link Order}.
   *
   * @return the {@link String name} of {@literal this} {@link Order}.
   * @see #getAbbreviation()
   */
  public String getName() {
    return this.name;
  }

  /**
   * Return a {@link String} representation of {@literal this} {@link Order}.
   *
   * @return a {@link String} describing {@literal this} {@link Order}.
   * @see java.lang.Object#toString()
   * @see #getName()
   */
  @Override
  public String toString() {
    return getName();
  }
}
