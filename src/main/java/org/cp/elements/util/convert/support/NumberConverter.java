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

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import org.cp.elements.lang.StringUtils;
import org.cp.elements.util.convert.ConversionException;
import org.cp.elements.util.convert.ConverterAdapter;

/**
 * The NumberConverter class converts an Object value into a Number of a qualified numerical Class type.
 * <p/>
 * @author John J. Blum
 * @see java.lang.Number
 * @see java.lang.Object
 * @see java.lang.String
 * @see java.math.BigDecimal
 * @see java.math.BigInteger
 * @see java.util.concurrent.atomic.AtomicInteger
 * @see java.util.concurrent.atomic.AtomicLong
 * @see org.cp.elements.util.convert.ConverterAdapter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class NumberConverter extends ConverterAdapter<Object, Number> {

  protected static final String CONVERSION_EXCEPTION_MESSAGE = "The Object value (%1$s) is not a valid number of the qualifying type (%2$s)!";

  protected <QT extends Number> QT parseNumber(final String number, final Class<QT> numberType) {
    if (AtomicInteger.class.isAssignableFrom(numberType)) {
      return numberType.cast(new AtomicInteger(Integer.parseInt(number)));
    }
    else if (AtomicLong.class.isAssignableFrom(numberType)) {
      return numberType.cast(new AtomicLong(Long.parseLong(number)));
    }
    else if (BigDecimal.class.isAssignableFrom(numberType)) {
      return numberType.cast(new BigDecimal(number));
    }
    else if (BigInteger.class.isAssignableFrom(numberType)) {
      return numberType.cast(new BigInteger(number));
    }
    else if (Byte.class.isAssignableFrom(numberType)) {
      return numberType.cast(Byte.parseByte(number));
    }
    else if (Short.class.isAssignableFrom(numberType)) {
      return numberType.cast(Short.parseShort(number));
    }
    else if (Integer.class.isAssignableFrom(numberType)) {
      return numberType.cast(Integer.parseInt(number));
    }
    else if (Long.class.isAssignableFrom(numberType)) {
      return numberType.cast(Long.parseLong(number));
    }
    else if (Float.class.isAssignableFrom(numberType)) {
      return numberType.cast(Float.parseFloat(number));
    }
    else if (Double.class.isAssignableFrom(numberType)) {
      return numberType.cast(Double.parseDouble(number));
    }

    throw new ConversionException(String.format("The Class type (%1$s) is not a valid Number type!", numberType));
  }

  protected <QT extends Number> QT toQualifyingNumber(final Number number, final Class<QT> numberType) {
    if (AtomicInteger.class.isAssignableFrom(numberType)) {
      return numberType.cast(new AtomicInteger(number.intValue()));
    }
    else if (AtomicLong.class.isAssignableFrom(numberType)) {
      return numberType.cast(new AtomicLong(number.longValue()));
    }
    else if (BigDecimal.class.isAssignableFrom(numberType)) {
      return numberType.cast(new BigDecimal(number.toString()));
    }
    else if (BigInteger.class.isAssignableFrom(numberType)) {
      return numberType.cast(new BigInteger(number.toString()));
    }
    else if (Byte.class.isAssignableFrom(numberType)) {
      return numberType.cast(number.byteValue());
    }
    else if (Short.class.isAssignableFrom(numberType)) {
      return numberType.cast(number.shortValue());
    }
    else if (Integer.class.isAssignableFrom(numberType)) {
      return numberType.cast(number.intValue());
    }
    else if (Long.class.isAssignableFrom(numberType)) {
      return numberType.cast(number.longValue());
    }
    else if (Float.class.isAssignableFrom(numberType)) {
      return numberType.cast(number.floatValue());
    }
    else if (Double.class.isAssignableFrom(numberType)) {
      return numberType.cast(number.doubleValue());
    }

    throw new ConversionException(String.format("The Class type (%1$s) is not a valid Number type!", numberType));
  }

  @Override
  public boolean canConvert(final Class<?> fromType, final Class<?> toType) {
    return (isAssignableTo(fromType, Number.class, String.class) && isAssignableTo(toType, Number.class));
  }

  @Override
  public <QT extends Number> QT convert(final Object value, final Class<QT> qualifyingType) {
    try {
      if (qualifyingType.isInstance(value)) {
        return qualifyingType.cast(value);
      }
      else if (value instanceof Number) {
        return toQualifyingNumber((Number) value, qualifyingType);
      }
      else if (value instanceof String && StringUtils.containsDigits(value.toString().trim())) {
        return parseNumber(value.toString().trim(), qualifyingType);
      }
      else {
        throw new ConversionException(String.format(CONVERSION_EXCEPTION_MESSAGE, value, qualifyingType.getName()));
      }
    }
    catch (Exception e) {
      if (e instanceof ConversionException) {
        throw (ConversionException) e;
      }
      throw new ConversionException(String.format(CONVERSION_EXCEPTION_MESSAGE, value, qualifyingType.getName()), e);
    }
  }

}
