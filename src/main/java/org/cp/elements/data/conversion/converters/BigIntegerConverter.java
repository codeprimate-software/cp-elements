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

import java.math.BigInteger;

import org.cp.elements.data.conversion.AbstractConverter;
import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.data.conversion.Converter;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.lang.annotation.Order;

/**
 * {@link BigIntegerConverter} converts a {@link String} to a {@link BigInteger}.
 *
 * @author John J. Blum
 * @see java.lang.String
 * @see java.math.BigInteger
 * @see org.cp.elements.data.conversion.AbstractConverter
 * @see org.cp.elements.lang.annotation.Order
 * @since 1.0.0
 */
@SuppressWarnings("unused")
@Order(-1_000_000)
public class BigIntegerConverter extends AbstractConverter<String, BigInteger> {

  private final Integer radix;

  /**
   * Constructs a new {@link BigIntegerConverter} with no {@link Integer radix}.
   */
  public BigIntegerConverter() {
    this(null);
  }

  /**
   * Constructs a new {@link BigIntegerConverter} initialized with the given {@link Integer radix}.
   *
   * @param radix {@link Integer} value used to initialize the {@link BigInteger}.
   */
  public BigIntegerConverter(@Nullable Integer radix) {
    this.radix = radix;
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
  @Override
  public boolean canConvert(Class<?> fromType, Class<?> toType) {
    return fromType != null && isAssignableTo(fromType, String.class) && BigInteger.class.equals(toType);
  }

  /**
   * Converts an {@link Object} of {@link Class type S} into an {@link Object} of {@link Class type T}.
   *
   * @param value {@link Object} of {@link Class type S} to convert.
   * @return the converted {@link Object} of {@link Class type T}.
   * @throws ConversionException if the {@link Object} cannot be converted.
   * @see org.cp.elements.data.conversion.ConversionService#convert(Object, Class)
   * @see #convert(Object, Class)
   */
  @Override
  public BigInteger convert(String value) {

    try {
      return (this.radix == null ? new BigInteger(String.valueOf(value).trim())
        : new BigInteger(String.valueOf(value).trim(), this.radix));
    }
    catch (NumberFormatException cause) {
      throw newConversionException(cause, "[%s] is not a valid BigInteger", value);
    }
  }
}
