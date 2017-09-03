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

package org.cp.elements.data.convert.support;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

import org.cp.elements.data.convert.ConversionException;
import org.cp.elements.data.convert.ConverterAdapter;
import org.cp.elements.lang.DateTimeUtils;
import org.cp.elements.lang.StringUtils;

/**
 * The CalendarConverter class converts an Object value into a Calendar.
 *
 * @author John J. Blum
 * @see java.lang.Object
 * @see java.util.Calendar
 * @see org.cp.elements.data.convert.ConverterAdapter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class CalendarConverter extends ConverterAdapter<Object, Calendar> {

  protected static final String DEFAULT_PATTERN = "MM/dd/yyyy hh:mm:ss a";

  private final DateFormat dateFormat;

  public CalendarConverter() {
    this(DEFAULT_PATTERN);
  }

  public CalendarConverter(final String pattern) {
    dateFormat = new SimpleDateFormat(pattern);
  }

  public DateFormat getDateFormat() {
    return dateFormat;
  }

  @Override
  public boolean canConvert(final Class<?> fromType, final Class<?> toType) {
    return (isAssignableTo(fromType, Calendar.class, Date.class, Number.class, String.class)
      && Calendar.class.equals(toType));
  }

  @Override
  public Calendar convert(final Object value) {
    try {
      if (value instanceof Calendar) {
        return (Calendar) value;
      }
      else if (value instanceof Date) {
        return DateTimeUtils.create(((Date) value).getTime());
      }
      else if (value instanceof Number) {
        return DateTimeUtils.create(((Number) value).longValue());
      }
      else if (value instanceof String) {
        final String valueString = String.valueOf(value).trim();

        return (StringUtils.isDigits(valueString) ? DateTimeUtils.create(Long.parseLong(valueString))
          : DateTimeUtils.create(getDateFormat().parse(valueString).getTime()));
      }
      else {
        throw new ConversionException(String.format("The Object value (%1$s) is not a valid date/time!", value));
      }
    }
    catch (ParseException e) {
      throw new ConversionException(String.format("The Object value (%1$s) is not a valid date/time!", value), e);
    }
  }

}
