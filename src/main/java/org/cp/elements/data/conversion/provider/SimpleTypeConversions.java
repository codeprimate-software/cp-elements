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
package org.cp.elements.data.conversion.provider;

import static org.cp.elements.lang.ElementsExceptionsFactory.newConversionException;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalArgumentException;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.function.Function;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ClassUtils;
import org.cp.elements.lang.NumberUtils;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;

/**
 * {@link Enum} (Enumeration) of simple (primitive) {@link Class type} conversions.
 *
 * @author John Blum
 * @see java.lang.Enum
 * @see java.math.BigDecimal
 * @see java.math.BigInteger
 * @see java.util.function.Function
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public enum SimpleTypeConversions {

  BIG_DECIMAL_CONVERTER(BigDecimal.class) {

    @Override
    public @NotNull Function<Object, BigDecimal> conversionFunction() {
      return argument -> SimpleTypeConversions.computeNumber(argument, BigDecimal::new);
    }
  },

  BIG_INTEGER_CONVERTER(BigInteger.class) {

    @Override
    public @NotNull Function<Object, BigInteger> conversionFunction() {
      return argument -> SimpleTypeConversions.computeNumber(argument, BigInteger::new);
    }
  },

  BOOLEAN_CONVERTER(Boolean.class) {

    @Override
    public @NotNull Function<Object, Boolean> conversionFunction() {
      return argument -> Boolean.parseBoolean(String.valueOf(argument).trim());
    }
  },

  BYTE_CONVERTER(Byte.class) {

    @Override
    public @NotNull Function<Object, Byte> conversionFunction() {
      return argument -> {
        try {
          return SimpleTypeConversions.computeNumber(argument, Byte::parseByte);
        }
        catch (NumberFormatException cause) {
          throw newConversionException(cause, "Cannot convert [%s] into a Byte", argument);
        }
      };
    }
  },

  SHORT_CONVERTER(Short.class) {

    @Override
    public @NotNull Function<Object, Short> conversionFunction() {
      return argument -> {
        try {
          return SimpleTypeConversions.computeNumber(argument, Short::parseShort);
        }
        catch (NumberFormatException cause) {
          throw newConversionException(cause, "Cannot convert [%s] into a Short", argument);
        }
      };
    }
  },

  INTEGER_CONVERTER(Integer.class) {

    @Override
    public @NotNull Function<Object, Integer> conversionFunction() {
      return argument -> {
        try {
          return SimpleTypeConversions.computeNumber(argument, Integer::parseInt);
        }
        catch (NumberFormatException cause) {
          throw newConversionException(cause, "Cannot convert [%s] into an Integer", argument);
        }
      };
    }
  },

  LONG_CONVERTER(Long.class) {

    @Override
    public @NotNull Function<Object, Long> conversionFunction() {
      return argument -> {
        try {
          return SimpleTypeConversions.computeNumber(argument, Long::parseLong);
        }
        catch (NumberFormatException cause) {
          throw newConversionException(cause, "Cannot convert [%s] into a Long", argument);
        }
      };
    }
  },

  FLOAT_CONVERTER(Float.class) {

    @Override
    public @NotNull Function<Object, Float> conversionFunction() {
      return argument -> {
        try {
          return Float.parseFloat(String.valueOf(argument).trim());
        }
        catch (NumberFormatException cause) {
          throw newConversionException(cause, "Cannot convert [%s] into a Float", argument);
        }
      };
    }
  },

  DOUBLE_CONVERTER(Double.class) {

    @Override
    public @NotNull Function<Object, Double> conversionFunction() {
      return argument -> {
        try {
          return Double.parseDouble(String.valueOf(argument).trim());
        }
        catch (NumberFormatException cause) {
          throw newConversionException(cause, "Cannot convert [%s] into a Double", argument);
        }
      };
    }
  },

  STRING_CONVERTER(String.class) {

    @Override
    public @NotNull Function<Object, String> conversionFunction() {
      return String::valueOf;
    }
  };

  /**
   * Computes a {@link Number} from the given {@link Object value} constructed with the given,
   * required {@link Function}.
   *
   * @param <T> {@link Class type} of {@link Number} to be computed.
   * @param value {@link Object} to compute as a {@link Number}.
   * @param numberConstructorFunction {@link Function} used to construct a specific {@link Class type}
   * of {@link Number}; must not be {@literal null}.
   * @return a {@link Number} computed from the given {@link Object value}.
   * @throws IllegalArgumentException if the {@link Function} is {@literal null}.
   * @see java.util.function.Function
   * @see java.lang.Number
   */
  private static @NotNull <T extends Number> T computeNumber(@NotNull Object value,
    @NotNull Function<String, T> numberConstructorFunction) {

    Assert.notNull(numberConstructorFunction, "The Function used to construct the Number is required");

    String valueString = String.valueOf(value).trim();

    String number = isStrictBinaryString(valueString) ? NumberUtils.fromBinaryString(valueString).toString()
      : NumberUtils.isHexadecimalString(valueString) ? NumberUtils.fromHexadecimalString(valueString).toString()
      : StringUtils.replaceAll(valueString, StringUtils.UNDERSCORE, StringUtils.EMPTY_STRING);

    return numberConstructorFunction.apply(number);
  }

  private static boolean isStrictBinaryString(@NotNull String value) {
    return value.startsWith(NumberUtils.BINARY_PREFIX_NOTATION) && NumberUtils.isBinaryString(value);
  }

  /**
   * Factory method used to find a {@link SimpleTypeConversions} instance by the given {@link Class target type}.
   *
   * @param targetType {@link Class} declaring the {@link Class target type} of the conversion.
   * @return a {@link SimpleTypeConversions} instance for the given {@link Class target type}.
   * @throws IllegalArgumentException if a {@link SimpleTypeConversions} instance is not found
   * for the given {@link Class target type}.
   */
  public static @NotNull SimpleTypeConversions findBy(@NotNull Class<?> targetType) {

    for (SimpleTypeConversions conversion : values()) {
      if (conversion.getTargetType().equals(targetType)) {
        return conversion;
      }
    }

    throw newIllegalArgumentException("No SimpleTypeConversion exists for target type [%s]",
      ClassUtils.getName(targetType));
  }

  private final Class<?> targetType;

  /**
   * Constructs a new the {@link SimpleTypeConversions} {@link Enum enumeration} initialized with
   * the given, required {@link Class target type} of the {@link Object} resulting from the conversion.
   *
   * @param targetType {@link Class} declaring the {@link Class target type} of the conversion.
   * @throws IllegalArgumentException if the {@link Class target type} is {@literal null}.
   * @see java.lang.Class
   */
  SimpleTypeConversions(@NotNull Class<?> targetType) {
    this.targetType = ObjectUtils.requireObject(targetType, "Target type of the conversion is required");
  }

  /**
   * Gets the {@link Class target type} of the {@link Object} resulting from the conversion.
   *
   * @return the {@link Class target type} of the {@link Object} resulting from the conversion; never {@literal null}.
   * @see java.lang.Class
   */
  public @NotNull Class<?> getTargetType() {
    return this.targetType;
  }

  /**
   * Gets the {@link Function} used to perform the conversion.
   *
   * @param <T> {@link Class target type} of the {@link Object} resulting from the conversion.
   * @return a {@link Function} encapsulating the logic used to perform the conversion.
   * @see java.util.function.Function
   */
  public abstract @NotNull <T> Function<Object, ?> conversionFunction();

  /**
   * Converts the given {@link Object} into a value of the target type {@link Class T}.
   *
   * @param <T> {@link Class target type} of the {@link Object} resulting from the conversion.
   * @param value {@link Object} to convert.
   * @return the converted {@link Object} into a value of the target type {@link Class T}.
   * @see #conversionFunction()
   */
  @SuppressWarnings("unchecked")
  public <T> T convert(Object value) {
    return (T) this.conversionFunction().apply(value);
  }
}
