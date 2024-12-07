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
package org.cp.elements.lang;

import static org.cp.elements.lang.LangExtensions.is;

import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Abstract base class for processing {@link Integer Integers}.
 *
 * @author John Blum
 * @see java.lang.Integer
 * @since 1.0.0
 */
@SuppressWarnings({ "AbstractClassName", "unused" })
public abstract class Integers {

  public static final Integer ZERO = 0;
  public static final Integer MINUS_ONE = -1;
  public static final Integer ONE = 1;
  public static final Integer TWO = 2;
  public static final Integer FOUR = 4;
  public static final Integer EIGHT = 8;
  public static final Integer SIXTEEN = 16;
  public static final Integer THIRTY_TWO = 32;
  public static final Integer SIXTY_FOUR = 64;
  public static final Integer ONE_HUNDRED = 100;
  public static final Integer ONE_HUNDRED_TWENTY_EIGHT = 128;
  public static final Integer TWO_HUNDRED_FIFTY_SIX = 256;
  public static final Integer FIVE_HUNDRED = 500;
  public static final Integer FIVE_HUNDRED_TWELVE = 512;
  public static final Integer ONE_THOUSAND = 1000;
  public static final Integer ONE_THOUSAND_TWENTY_FOUR = 1024;
  public static final Integer TWO_THOUSAND_FOURTY_EIGHT = 2048;
  public static final Integer FOUR_THOUSAND_NINETY_SIX = 4096;
  public static final Integer EIGHT_THOUSAND_ONE_HUNDRED_NINETY_TWO = 8192;

  /**
   * Returns the given {@link Number} as an {@link Integer}.
   *
   * @param number {@link Number} to convert to an {@link Integer}.
   * @return the given {@link Number} as an {@link Integer}.
   * Returns {@literal null} if the given {@link Number} is {@literal null}.
   * @see java.lang.Integer
   * @see java.lang.Number
   */
  @NullSafe
  public static @Nullable Integer asInteger(@Nullable Number number) {
    return number != null ? number.intValue() : null;
  }

  /**
   * Null-safe method used to determine whether the given {@link Integer value} is greater than {@literal 0}.
   *
   * @param value {@link Integer value} to evaluate.
   * @return a boolean value indicating whether the given {@link Integer value} is greater than {@literal 0}.
   * @see #isLessThanZero(Integer)
   * @see java.lang.Integer
   */
  @NullSafe
  public static boolean isGreaterThanZero(@Nullable Integer value) {
    return value != null && is(value).greaterThan(ZERO);
  }

  /**
   * Null-safe method used to determine whether the given {@link Integer value} is less than {@literal 0}.
   *
   * @param value {@link Integer value} to evaluate.
   * @return a boolean value indicating whether the given {@link Integer value} is less than {@literal 0}.
   * @see #isGreaterThanZero(Integer)
   * @see java.lang.Integer
   */
  @NullSafe
  public static boolean isLessThanZero(@Nullable Integer value) {
    return value != null && is(value).lessThan(ZERO);
  }

  /**
   * Null-safe method used to determine whether the given {@link Integer value} is equal to {@literal -1}.
   *
   * @param value {@link Integer value} to evaluate.
   * @return a boolean valued indicating whether the given {@link Integer value} is equal to {@literal -1}.
   * @see java.lang.Integer
   */
  @NullSafe
  public static boolean isMinusOne(@Nullable Integer value) {
    return MINUS_ONE.equals(value);
  }

  /**
   * Null-safe method used to determine whether the given {@link Integer value} is equal to {@literal 1}.
   *
   * @param value {@link Integer value} to evaluate.
   * @return a boolean valued indicating whether the given {@link Integer value} is equal to {@literal 1}.
   * @see java.lang.Integer
   */
  @NullSafe
  public static boolean isOne(@Nullable Integer value) {
    return ONE.equals(value);
  }

  /**
   * Null-safe method used to determine whether the given {@link Integer value} is equal to {@literal 0}.
   *
   * @param value {@link Integer value} to evaluate.
   * @return a boolean valued indicating whether the given {@link Integer value} is equal to {@literal 0}.
   * @see java.lang.Integer
   */
  @NullSafe
  public static boolean isZero(@Nullable Integer value) {
    return ZERO.equals(value);
  }

  /**
   * Converts a {@link Integer positive number} into a {@link Integer negative number}
   * and converts a {@link Integer negative number} into a {@link Integer positive number}.
   * <p>
   * If the given {@link Integer number} is {@literal 0}, then {@literal 0} is returned.
   *
   * @param number {@link Integer} to invert.
   * @return the inverted {@link Integer number}.
   * @see java.lang.Integer
   */
  public static int negate(int number) {
    return number * -1;
  }
}
