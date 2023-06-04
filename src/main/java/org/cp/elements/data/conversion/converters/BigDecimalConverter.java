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
import java.math.MathContext;

import org.cp.elements.data.conversion.AbstractConverter;
import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.data.conversion.Converter;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.lang.annotation.Order;

/**
 * {@link BigDecimalConverter} converts a {@link String} to a {@link BigDecimal}.
 *
 * @author John J. Blum
 * @see java.lang.String
 * @see java.math.BigDecimal
 * @see org.cp.elements.data.conversion.AbstractConverter
 * @see org.cp.elements.lang.annotation.Order
 * @since 1.0.0
 */
@SuppressWarnings("unused")
@Order(-1_000_000)
public class BigDecimalConverter extends AbstractConverter<String, BigDecimal> {

  private final MathContext mathContext;

  /**
   * Constructs a new {@link BigDecimalConverter} with no {@link MathContext}.
   */
  public BigDecimalConverter() {
    this(null);
  }

  /**
   * Constructs a new {@link BigDecimalConverter} initialized with the given {@link MathContext}.
   *
   * @param mathContext {@link MathContext} used to initialize the {@link BigDecimal} value.
   * @see java.math.MathContext
   */
  public BigDecimalConverter(@Nullable MathContext mathContext) {
    this.mathContext = mathContext;
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
    return fromType != null && isAssignableTo(fromType, String.class) && BigDecimal.class.equals(toType);
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
  public BigDecimal convert(String value) {

    try {
      return (this.mathContext == null ? new BigDecimal(String.valueOf(value).trim())
        : new BigDecimal(String.valueOf(value).trim(), this.mathContext));
    }
    catch (NumberFormatException cause) {
      throw newConversionException(cause, "[%s] is not a valid BigDecimal", value);
    }
  }
}
