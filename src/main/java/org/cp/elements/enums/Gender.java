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

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * An {@link Enum enumeration} of the genders [{@literal male}, {@literal female}] for people.
 *
 * @author John J. Blum
 * @see java.lang.Enum
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public enum Gender {

  FEMALE("F", "Female"),
  MALE("M", "Male");

  /**
   * Factory method used to find a {@link Gender} by {@link String abbreviation}.
   *
   * @param abbreviation {@link String} containing the {@literal abbreviation} of the {@link Gender} to find.
   * @return the {@link Gender} with the given {@link String abbreviation}, or {@literal null}
   * if no {@link Gender} with the given {@link String abbreviation} exists.
   * @see #valueOf(Predicate)
   * @see #getAbbreviation()
   */
  public static @Nullable Gender valueOfAbbreviation(@Nullable String abbreviation) {
    return valueOf(gender -> gender.getAbbreviation().equalsIgnoreCase(abbreviation));
  }

  /**
   * Factory method used to find a {@link Gender} by {@link String name}.
   *
   * @param name {@link String} containing the {@literal name} of the {@link Gender} to find.
   * @return the {@link Gender} with the given {@link String name}, or {@literal null}
   * if no {@link Gender} with the given {@link String name} exists.
   * @see #valueOf(Predicate)
   * @see #getName()
   */
  public static @Nullable Gender valueOfName(@Nullable String name) {
    return valueOf(gender -> gender.getName().equalsIgnoreCase(name));
  }

  /**
   * Factory method used to find a {@link Gender} by the given, required {@link Predicate}.
   *
   * @param predicate {@link Predicate} used to fina and match a {@link Gender}; must not be {@literal null}.
   * @return a {@link Gender} matching the given, required {@link Predicate} or {@literal null} if no {@link Gender}
   * is a match for the given, required {@link Predicate}.
   * @see java.util.function.Predicate
   * @see #values()
   */
  private static @Nullable Gender valueOf(Predicate<Gender> predicate) {

    return Arrays.stream(values())
      .filter(predicate)
      .findFirst()
      .orElse(null);
  }

  private final String abbreviation;
  private final String name;

  /**
   * Constructs a new instance of {@link Gender} initialized with the given, required {@link String abbreviation}
   * and {@link String name}.
   *
   * @param abbreviation {@link String} specifying the abbreviation for {@literal this} {@link Gender}.
   * @param name {@link String} containing the name of {@literal this} {@link Gender}.
   */
  Gender(@NotNull String abbreviation, @NotNull String name) {
    this.abbreviation = abbreviation;
    this.name = name;
  }

  /**
   * Get the {@link String abbreviation} for {@literal this} {@link Gender}.
   *
   * @return the {@link String abbreviation} of {@literal this} {@link Gender}.
   */
  public @NotNull String getAbbreviation() {
    return this.abbreviation;
  }

  /**
   * Get the {@link String name} for {@literal this} {@link Gender}.
   *
   * @return the {@link String name} of {@literal this} {@link Gender}.
   */
  public @NotNull String getName() {
    return this.name;
  }

  /**
   * @inheritDoc
   */
  @Override
  public String toString() {
    return getName();
  }
}
