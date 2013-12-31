/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.util.convert.support;

import java.util.Calendar;
import java.util.Date;

import org.cp.elements.lang.StringUtils;
import org.cp.elements.util.convert.ConversionException;
import org.cp.elements.util.convert.ConverterAdapter;

/**
 * The LongConverter class converts an Object value into a Long.
 * <p/>
 * @author John J. Blum
 * @see java.lang.Long
 * @see java.lang.Object
 * @see org.cp.elements.util.convert.ConverterAdapter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class LongConverter extends ConverterAdapter<Object, Long> {

  protected static final String CONVERSION_EXCEPTION_MESSAGE = "The Object value (%1$s) is not a valid long!";

  @Override
  public boolean canConvert(final Class<?> fromType, final Class<?> toType) {
    return (isAssignableTo(fromType, Number.class, Calendar.class, Date.class, String.class)
      && Long.class.equals(toType));
  }

  @Override
  public Long convert(final Object value) {
    if (value instanceof Number) {
      return ((Number) value).longValue();
    }
    else if (value instanceof Calendar) {
      return ((Calendar) value).getTimeInMillis();
    }
    else if (value instanceof Date) {
      return ((Date) value).getTime();
    }
    else if (value instanceof String && StringUtils.containsDigits(value.toString().trim())) {
      try {
        return Long.parseLong(value.toString().trim());
      }
      catch (NumberFormatException e) {
        throw new ConversionException(String.format(CONVERSION_EXCEPTION_MESSAGE, value), e);
      }
    }
    else {
      throw new ConversionException(String.format(CONVERSION_EXCEPTION_MESSAGE, value));
    }
  }

}
