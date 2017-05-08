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

package org.cp.elements.lang.support;

import static org.cp.elements.lang.LangExtensions.assertThat;
import static org.cp.elements.lang.reflect.ReflectionUtils.getValue;
import static org.cp.elements.lang.reflect.ReflectionUtils.withFields;

import java.util.Optional;
import java.util.logging.Logger;

import org.cp.elements.lang.Builder;
import org.cp.elements.lang.NullSafe;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.reflect.ModifierUtils;
import org.cp.elements.text.FormatUtils;

/**
 * The {@link HashCodeBuilder} class is a Builder used to compute the hash code of an object.
 *
 * @author John J. Blum
 * @see java.lang.Object#hashCode()
 * @see org.cp.elements.lang.Builder
 * @see org.cp.elements.lang.ObjectUtils
 * @see org.cp.elements.lang.reflect.ReflectionUtils
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class HashCodeBuilder implements Builder<Integer> {

  protected static final int DEFAULT_BASE_VALUE = 17;
  protected static final int DEFAULT_MULTIPLIER = 37;

  static Logger logger = Logger.getLogger(HashCodeBuilder.class.getName());

  private final int multiplier;

  private int hashValue;

  /**
   * Factory method to construct a new instance of {@link HashCodeBuilder} initialized with default values
   * for base value and multiplier.
   *
   * @return an instance of {@link HashCodeBuilder}.
   * @see #HashCodeBuilder()
   */
  public static HashCodeBuilder create() {
    return new HashCodeBuilder();
  }

  /**
   * Factory method to construct a new instance of {@link HashCodeBuilder} initialized with the given base value
   * and multiplier.
   *
   * @param baseValue integer indicating the starting value of the hash code.
   * @param multiplier integer indicating the multiplier used in the individual factors.
   * @return an instance of the {@link HashCodeBuilder} initialized with the given base value and multiplier.
   * @throws IllegalArgumentException if {@code baseValue} or {@code multiplier} are less than equal to 0.
   * @see #HashCodeBuilder(int, int)
   */
  public static HashCodeBuilder create(int baseValue, int multiplier) {
    return new HashCodeBuilder(baseValue, multiplier);
  }

  /**
   * Returns the {@link Logger} for the {@link HashCodeBuilder} class handling logging
   * for all instances of {@link HashCodeBuilder}.
   *
   * @return the {@link HashCodeBuilder} class {@link Logger}.
   * @see java.util.logging.Logger
   */
  protected static Logger getLogger() {
    return logger;
  }

  /**
   * Factory method to construct a new instance of {@link HashCodeBuilder} used to compute the hash code
   * of the given {@link Object}.
   *
   * This factory method can used to implement a {@code hashCode()} method for any object by simply
   * overriding the {@link Object#hashCode()} method on your class type and implementing as follows...
   *
   * <code>
   *   &#64;Override
   *   public int hashCode() {
   *     return HashCodeBuilder.hashCodeFor(this).build();
   *   }
   * </code>
   *
   * @param obj {@link Object} on which to compute the hash code.
   * @return an instance of {@link HashCodeBuilder} used to compute the hash code of the given {@link Object}.
   * @see java.lang.Object#hashCode()
   */
  @NullSafe
  @SuppressWarnings("all")
  public static HashCodeBuilder hashCodeFor(Object obj) {
    HashCodeBuilder builder = create();

    Optional.ofNullable(obj).ifPresent(object -> {
      withFields().on(object).matching(field -> !ModifierUtils.isTransient(field)).call(field -> {
        getLogger().fine(() -> FormatUtils.format("Hashing field [%1$s] on object [%2$s]",
          field.getName(), object.getClass().getName()));

        builder.with(getValue(object, field, field.getType()));
      });
    });

    return builder;
  }

  /**
   * Constructs an instance of the {@link HashCodeBuilder} initialized with the default base value and multiplier
   * used to compute the hash code of an object.
   *
   * @see #HashCodeBuilder(int, int)
   */
  public HashCodeBuilder() {
    this(DEFAULT_BASE_VALUE, DEFAULT_MULTIPLIER);
  }

  /**
   * Constructs an instance of the {@link HashCodeBuilder} initialized with the given base value and multiplier
   * used to compute the hash code of an object.
   *
   * @param baseValue integer indicating the starting value of the hash code.
   * @param multiplier integer indicating the multiplier used in the individual factors.
   * @throws IllegalArgumentException if {@code baseValue} or {@code multiplier} are less than equal to 0.
   */
  public HashCodeBuilder(int baseValue, int multiplier) {
    assertThat(baseValue).stating("baseValue [%d] must be greater than 0", baseValue).isGreaterThan(0);
    assertThat(multiplier).stating("multiplier [%d] must be greater than 0", multiplier).isGreaterThan(0);

    this.hashValue = baseValue;
    this.multiplier = multiplier;
  }

  /**
   * Combines the given integer value into the calculation of the hash code.
   *
   * @param value integer value to combine into the current calculation of the hash code.
   * @return this {@link HashCodeBuilder}.
   * @see #multiplier()
   * @see #hashValue()
   */
  protected HashCodeBuilder combine(int value) {
    setHashValue(multiplier() * hashValue() + value);
    return this;
  }

  /**
   * Returns the current value of the computed hash code.
   *
   * @return an integer for the current value of the computed hash code.
   * @see #setHashValue(int)
   */
  protected int hashValue() {
    return this.hashValue;
  }

  /**
   * Set the current value for the computed hash code.
   *
   * @param hashValue integer used as the current value for the computed hash code.
   * @return this {@link HashCodeBuilder}.
   * @see #hashValue()
   */
  protected HashCodeBuilder setHashValue(int hashValue) {
    this.hashValue = hashValue;
    return this;
  }

  /**
   * Returns the multiplier used in computing the hash code.
   *
   * @return an integer value for the multiplier used in computing the hash code.
   */
  protected int multiplier() {
    return this.multiplier;
  }

  /**
   * Combines the {@literal boolean} value into the hash code calculation.
   *
   * @param value {@literal boolean} value to combine into the hash code calculation.
   * @return this {@link HashCodeBuilder}.
   * @see #with(int)
   */
  public HashCodeBuilder with(boolean value) {
    return with(value ? 0 : 1);
  }

  /**
   * Combines the {@literal byte} value into the hash code calculation.
   *
   * @param value {@literal byte} value to combine into the hash code calculation.
   * @return this {@link HashCodeBuilder}.
   * @see #with(int)
   */
  public HashCodeBuilder with(byte value) {
    return with((int) value);
  }

  /**
   * Combines the {@literal char} value into the hash code calculation.
   *
   * @param value {@literal char} value to combine into the hash code calculation.
   * @return this {@link HashCodeBuilder}.
   * @see #with(int)
   */
  public HashCodeBuilder with(char value) {
    return with((int) value);
  }

  /**
   * Combines the {@literal short} value into the hash code calculation.
   *
   * @param value {@literal short} value to combine into the hash code calculation.
   * @return this {@link HashCodeBuilder}.
   * @see #with(int)
   */
  public HashCodeBuilder with(short value) {
    return with((int) value);
  }

  /**
   * Combines the {@literal int} value into the hash code calculation.
   *
   * @param value {@literal int} value to combine into the hash code calculation.
   * @return this {@link HashCodeBuilder}.
   * @see #combine(int)
   */
  public HashCodeBuilder with(int value) {
    return combine(value);
  }

  /**
   * Combines the {@literal long} value into the hash code calculation.
   *
   * @param value {@literal long} value to combine into the hash code calculation.
   * @return this {@link HashCodeBuilder}.
   * @see #with(int)
   */
  public HashCodeBuilder with(long value) {
    return with((int) (value ^ (value >>> 32)));
  }

  /**
   * Combines the {@literal float} value into the hash code calculation.
   *
   * @param value {@literal float} value to combine into the hash code calculation.
   * @return this {@link HashCodeBuilder}.
   * @see #with(int)
   */
  public HashCodeBuilder with(float value) {
    return with(Float.floatToIntBits(value));
  }

  /**
   * Combines the {@literal double} value into the hash code calculation.
   *
   * @param value {@literal double} value to combine into the hash code calculation.
   * @return this {@link HashCodeBuilder}.
   * @see #with(long)
   */
  public HashCodeBuilder with(double value) {
    return with(Double.doubleToLongBits(value));
  }

  /**
   * Combines the {@link Object} value into the hash code calculation.
   *
   * @param obj {@link Object} value to combine into the hash code calculation.
   * @return this {@link HashCodeBuilder}.
   * @see org.cp.elements.lang.ObjectUtils#hashCode(Object)
   * @see java.lang.Object#hashCode()
   * @see #combine(int)
   */
  public HashCodeBuilder with(Object obj) {
    return combine(ObjectUtils.hashCode(obj));
  }

  /**
   * Returns the result of computing the hash code.
   *
   * @return an integer value with the result of computing the hash code.
   * @see #hashValue()
   */
  public Integer build() {
    return hashValue();
  }
}
