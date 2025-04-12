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
package org.cp.elements.time;

import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalArgumentException;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalStateException;

import java.time.Duration;
import java.time.temporal.ChronoUnit;
import java.util.Arrays;
import java.util.function.Predicate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Nameable;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;

/**
 * {@link Enum Enumeration} of different formatters for {@link Duration}.
 *
 * @author John Blum
 * @see java.lang.Enum
 * @see java.time.Duration
 * @since 2.0.0
 */
public enum DurationFormatter {

  TIME_UNIT("\\d+[a-zA-Z]{1,2}");

  private final Pattern pattern;

  DurationFormatter(String durationPattern) {
    this.pattern = Pattern.compile(durationPattern);
  }

  /**
   * Gets the configured Regular Expression (REGEX) {@link Pattern} used to parse and match a {@link Duration}
   * formatted as a {@link String}.
   *
   * @return the configured Regular Expression (REGEX) {@link Pattern} used to parse and match a {@link Duration}
   * formatted as a {@link String}.
   * @see java.util.regex.Pattern
   */
  @NotNull Pattern getPattern() {
    return this.pattern;
  }

  /**
   * Parses the given, required {@link String text} as a {@link Duration}.
   *
   * @param text {@link String} containing the {@link String} formatted time as a representation of {@link Duration}.
   * @return the computed {@link Duration} from the {@link String text}.
   * @see java.time.Duration
   */
  public @NotNull Duration parse(String text) {

    Assert.hasText(text, "Text [%s] to parse as a Duration is required", text);

    Matcher matcher = getPattern().matcher(text);

    Duration total = Duration.ZERO;

    while (matcher.find()) {
      String value = matcher.group();
      total = total.plus(Unit.parseUnit(value).toDuration(value));
    }

    return total;
  }

  /**
   * {@link Enum Enumeration} of units of time.
   *
   * @see java.lang.Enum
   * @see org.cp.elements.lang.Nameable
   */
  public enum Unit implements Nameable<String> {

    NANOSECONDS("ns", ChronoUnit.NANOS),
    MICROSECONDS("us", ChronoUnit.MICROS),
    MILLISECONDS("ms", ChronoUnit.MILLIS),
    SECONDS("s", ChronoUnit.SECONDS),
    MINUTES("m", ChronoUnit.MINUTES),
    HOURS("h", ChronoUnit.HOURS),
    DAYS("d", ChronoUnit.DAYS);

    /**
     * Factory method used to find a {@link DurationFormatter.Unit} for the given, required {@link ChronoUnit}.
     *
     * @param chronoUnit {@link ChronoUnit} used to match a {@link DurationFormatter.Unit}.
     * @return a {@link DurationFormatter.Unit} for the given, required {@link ChronoUnit}.
     * @throws IllegalArgumentException if no {@link Unit} matches the given {@link ChronoUnit}.
     * @see java.time.temporal.ChronoUnit
     */
    public static @NotNull Unit valueOfChronoUnit(@NotNull ChronoUnit chronoUnit) {

      return Arrays.stream(values())
        .filter(unit -> unit.getUnit().equals(chronoUnit))
        .findFirst()
        .orElseThrow(() -> newIllegalArgumentException("No Unit %s matches the given ChronoUnit [%s]",
          Arrays.toString(values()), chronoUnit));
    }

    /**
     * Factory method used to find a {@link DurationFormatter.Unit} by the given, required {@link String symbol}.
     *
     * @param symbol {@link String} containing the symbol used to match the given {@link Unit}.
     * @return a {@link DurationFormatter.Unit} by the given, required {@link String symbol}.
     * @throws IllegalArgumentException if no {@link Unit} matches the given {@link String symbol}.
     */
    public static @NotNull Unit valueOfSymbol(@NotNull String symbol) {

      return Arrays.stream(values())
        .filter(unit -> unit.getSymbol().equalsIgnoreCase(StringUtils.trim(symbol)))
        .findFirst()
        .orElseThrow(() -> newIllegalArgumentException("No Unit %s matches the given Symbol [%s]",
          Arrays.toString(values()), symbol));
    }

    private final ChronoUnit unit;

    private final String symbol;

    Unit(String symbol, ChronoUnit unit) {
      this.symbol = symbol;
      this.unit = unit;
    }

    static @NotNull Unit parseUnit(@NotNull String value) {

      String symbol = parseSymbol(value);

      return Arrays.stream(values())
        .filter(unit -> unit.getSymbol().equalsIgnoreCase(symbol))
        .findFirst()
        .orElseThrow(() -> newIllegalStateException("Value [%s] does not contain a valid time unit", value));
    }

    private static String parse(String value, Predicate<Character> predicate) {

      Assert.hasText(value, "Value [%s] is required", value);

      StringBuilder builder = new StringBuilder();

      for (char character : value.toCharArray()) {
        if (predicate.test(character)) {
          builder.append(character);
        }
      }

      return builder.toString();
    }

    private static String parseSymbol(String value) {
      return parse(value, Character::isLetter);
    }

    private static Long parseTime(String value) {
      return Long.parseLong(parse(value, Character::isDigit));
    }

    /**
     * Gets the {@link String name} of this {@link Unit}.
     *
     * @return the {@link String name} of this {@link Unit}.
     */
    public @NotNull String getName() {
      return StringUtils.capitalize(name().toLowerCase());
    }

    /**
     * Gets the {@link String symbol} representing of this {@link Unit}.
     *
     * @return the {@link String symbol} representing of this {@link Unit}.
     */
    public @NotNull String getSymbol() {
      return this.symbol;
    }

    /**
     * Gets the Java {@link ChronoUnit} matching this {@link Unit}.
     *
     * @return the Java {@link ChronoUnit} matching this {@link Unit}.
     * @see java.time.temporal.ChronoUnit
     */
    public @NotNull ChronoUnit getUnit() {
      return this.unit;
    }

    /**
     * Computes a {@link Duration} from the given {@link String} representation.
     *
     * @param value {@link String} containing the value to parse as a {@link Duration}.
     * @return a {@link Duration} from the given {@link String} representation.
     * @see java.time.Duration
     * @see #parseTime(String)
     * @see #getUnit()
     */
    public @NotNull Duration toDuration(@NotNull String value) {
      return Duration.of(parseTime(value), getUnit());
    }
  }
}
