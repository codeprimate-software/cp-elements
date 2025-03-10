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
package org.cp.elements.data.conversion.converters;

import static org.cp.elements.lang.ElementsExceptionsFactory.newConversionException;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Function;

import org.cp.elements.data.conversion.AbstractConverter;
import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.data.conversion.Converter;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.lang.annotation.Order;
import org.cp.elements.util.MapBuilder;

/**
 * {@link NumberConverter} converts an {@link Object} to a {@link Number} of a {@link Class qualified numerical type}.
 *
 * @author John J. Blum
 * @see java.lang.Number
 * @see java.lang.Object
 * @see java.lang.String
 * @see java.math.BigDecimal
 * @see java.math.BigInteger
 * @see java.util.concurrent.atomic.AtomicInteger
 * @see java.util.concurrent.atomic.AtomicLong
 * @see org.cp.elements.data.conversion.AbstractConverter
 * @see org.cp.elements.lang.annotation.Order
 * @since 1.0.0
 */
@SuppressWarnings("unused")
@Order(-100_000)
public class NumberConverter extends AbstractConverter<Object, Number> {

  protected static final String CONVERSION_EXCEPTION_MESSAGE =
    "[%1$s] is not a valid number of the qualifying type [%2$s]";

  /**
   * Parses the given {@link String} representing a number and returns the value as a {@link QT qualified number type}.
   *
   * @param <QT> {@link Class qualified subtype} of {@link Number}.
   * @param number {@link String} containing the number to convert into a {@link Number}.
   * @param numberType {@link Class subtype} of {@link Number}.
   * @return a {@link Number} parsed from the {@link String} containing a number.
   * @throws ConversionException if the {@link String} cannot be converted into a {@link Number}.
   */
  protected @NotNull <QT extends Number> QT parseNumber(@NotNull String number, @NotNull Class<QT> numberType) {

    Map<Class<? extends Number>, Function<String, QT>> numberConversions =
      MapBuilder.<Class<? extends Number>, Function<String, QT>>newHashMap()
        .put(AtomicInteger.class, it -> numberType.cast(new AtomicInteger(Integer.parseInt(it))))
        .put(AtomicLong.class, it -> numberType.cast(new AtomicLong(Long.parseLong(it))))
        .put(BigDecimal.class, it -> numberType.cast(new BigDecimal(it)))
        .put(BigInteger.class, it -> numberType.cast(new BigInteger(it)))
        .put(Byte.class, it -> numberType.cast(Byte.parseByte(it)))
        .put(Short.class, it -> numberType.cast(Short.parseShort(it)))
        .put(Integer.class, it -> numberType.cast(Integer.parseInt(it)))
        .put(Long.class, it -> numberType.cast(Long.parseLong(it)))
        .put(Float.class, it -> numberType.cast(Float.parseFloat(it)))
        .put(Double.class, it -> numberType.cast(Double.parseDouble(it)))
        .build();

    return numberConversions.entrySet().stream()
      .filter(entry -> entry.getKey().isAssignableFrom(numberType))
      .findFirst()
      .map(entry -> entry.getValue().apply(number))
      .orElseThrow(() -> newConversionException("[%s] is not a valid Number type", numberType.getName()));
  }

  /**
   * Returns the given {@link Number} as a {@link Class qualified subtype} of {@link Number}.
   *
   * @param <QT> {@link Class qualified subtype} of {@link Number}.
   * @param number {@link Number} to qualify.
   * @param numberType {@link Class qualified subtype} of {@link Number}.
   * @return the given {@link Number} as a {@link QT qualified subtype} of {@link Number}.
   * @throws ConversionException if {@link Number} cannot be qualified
   * as the desired {@link Class subtype} of {@link Number}.
   */
  protected @NotNull <QT extends Number> QT toQualifyingNumber(@NotNull Number number, @NotNull Class<QT> numberType) {

    Map<Class<? extends Number>, Function<Number, QT>> numberConversions =
      MapBuilder.<Class<? extends Number>, Function<Number, QT>>newHashMap()
        .put(AtomicInteger.class, it -> numberType.cast(new AtomicInteger(it.intValue())))
        .put(AtomicLong.class, it -> numberType.cast(new AtomicLong(it.longValue())))
        .put(BigDecimal.class, it -> numberType.cast(new BigDecimal(it.toString())))
        .put(BigInteger.class, it -> numberType.cast(new BigInteger(it.toString())))
        .put(Byte.class, it -> numberType.cast(it.byteValue()))
        .put(Short.class, it -> numberType.cast(it.shortValue()))
        .put(Integer.class, it -> numberType.cast(it.intValue()))
        .put(Long.class, it -> numberType.cast(it.longValue()))
        .put(Float.class, it -> numberType.cast(it.floatValue()))
        .put(Double.class, it -> numberType.cast(it.doubleValue()))
        .build();

    return numberConversions.entrySet().stream()
      .filter(entry -> entry.getKey().isAssignableFrom(numberType))
      .findFirst()
      .map(entry -> entry.getValue().apply(number))
      .orElseThrow(() -> newConversionException("[%s] is not a valid Number type", numberType.getName()));
  }

  /**
   * Determines whether this {@link Converter} can convert {@link Object Objects}
   * {@link Class from type} {@link Class to type}.
   *
   * @param fromType {@link Class type} to convert from.
   * @param toType {@link Class type} to convert to.
   * @return a boolean indicating whether this {@link Converter} can convert {@link Object Objects}
   * {@link Class from type} {@link Class to type}.
   * @see org.cp.elements.data.conversion.ConversionService#canConvert(Class, Class)
   * @see #canConvert(Object, Class)
   */
  @NullSafe
  @Override
  public boolean canConvert(@Nullable Class<?> fromType, @Nullable Class<?> toType) {
    return fromType != null && isAssignableTo(fromType, Number.class, String.class)
      && toType != null && Number.class.isAssignableFrom(toType);
  }

  /**
   * Converts an {@link Object} of {@link Class type S} into an {@link Object} of {@link Class qualifying type QT}.
   *
   * @param <QT> {@link Class qualifying type} extending {@link Class type T}.
   * @param value {@link Object} of {@link Class type S} to convert.
   * @param qualifyingType the {@link Class qualifying type} of the {@link Object} resolved in the conversion.
   * @return the converted {@link Object} of {@link Class qualifying type QT}.
   * @throws ConversionException if the {@link Object} cannot be converted.
   * @throws IllegalArgumentException if {@link Class qualifying type} is {@literal null}.
   * @see org.cp.elements.data.conversion.ConversionService#convert(Object, Class)
   * @see #convert(Object)
   */
  @Override
  public @NotNull <QT extends Number> QT convert(@Nullable Object value, @NotNull Class<QT> qualifyingType) {

    Assert.notNull(qualifyingType, "Qualifying type is required");

    try {
      if (qualifyingType.isInstance(value)) {
        return qualifyingType.cast(value);
      }
      else if (value instanceof Number) {
        return toQualifyingNumber((Number) value, qualifyingType);
      }
      else if (isStringWithDigits(value)) {
        return parseNumber(value.toString().trim(), qualifyingType);
      }
      else {
        throw newConversionException(CONVERSION_EXCEPTION_MESSAGE, value, qualifyingType.getName());
      }
    }
    catch (Exception cause) {

      if (cause instanceof ConversionException) {
        throw (ConversionException) cause;
      }

      throw newConversionException(cause, CONVERSION_EXCEPTION_MESSAGE, value, qualifyingType.getName());
    }
  }

  @NullSafe
  private boolean isStringWithDigits(@Nullable Object value) {
    return value instanceof String && StringUtils.containsDigits(value.toString());
  }
}
