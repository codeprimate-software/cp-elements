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

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.data.conversion.Converter;
import org.cp.elements.data.conversion.DefaultableConverter;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * {@link DateConverter} converts an {@link Object } to a {@link Date}.
 *
 * @author John J. Blum
 * @see java.lang.Object
 * @see java.util.Date
 * @see org.cp.elements.data.conversion.DefaultableConverter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class DateConverter extends DefaultableConverter<Object, Date> {

  protected static final String DEFAULT_PATTERN = "MM/dd/yyyy hh:mm:ss a";

  private final DateFormat dateFormat;

  /**
   * Constructs a new instance of {@link DateConverter} using the default date/time {@link String format pattern}.
   */
  public DateConverter() {
    this(DEFAULT_PATTERN);
  }

  /**
   * Constructs a new instance of {@link DateConverter} using the given, required date/time
   * {@link String format pattern}.
   *
   * @param pattern {@link String} containing the format pattern used to parse date/time {@link String values}.
   */
  public DateConverter(@NotNull String pattern) {
    this.dateFormat = new SimpleDateFormat(pattern);
  }

  /**
   * Gets the configured {@link DateFormat} used to parse date/time {@link String values}.
   *
   * @return the configured {@link DateFormat} used to parse date/time {@link String values}.
   * @see java.text.DateFormat
   */
  protected @NotNull DateFormat getDateFormat() {
    return this.dateFormat;
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
  public boolean canConvert(@Nullable Class<?> fromType, @Nullable Class<?> toType) {
    return isAssignableTo(fromType, Calendar.class, Date.class, Number.class, String.class)
      && Date.class.equals(toType);
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
  public Date convert(Object value) {

    if (value instanceof Calendar) {
      return ((Calendar) value).getTime();
    }
    else if (value instanceof Date) {
      return (Date) value;
    }
    else if (value instanceof Number) {
      return new Date(((Number) value).longValue());
    }
    else if (value instanceof String) {

      String valueString = String.valueOf(value).trim();

      try {
        return StringUtils.isDigits(valueString)
          ? new Date(Long.parseLong(valueString))
          : new Date(getDateFormat().parse(valueString).getTime());
      }
      catch (NumberFormatException | ParseException cause) {
        throw newConversionException(cause, "[%s] is not a valid date/time", value);
      }
    }
    else {
      return super.convert(value);
    }
  }
}
