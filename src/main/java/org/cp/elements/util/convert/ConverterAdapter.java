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

package org.cp.elements.util.convert;

import org.cp.elements.lang.Constants;

/**
 * The ConverterAdapter class is an abstract base class for all Converter implementations implemented as an Adapter
 * throwing UnsupportedOperationExceptions for all unimplemented Converter methods.
 *
 * @author John Blum
 * @see java.lang.UnsupportedOperationException
 * @see org.cp.elements.util.convert.AbstractConverter
 * @see org.cp.elements.util.convert.Converter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class ConverterAdapter<S, T> extends AbstractConverter<S, T> {

  /**
   * Determines whether this Converter can convert values from the source Class type into the target Class type.
   * <p/>
   * @param fromType the Class type to convert from.
   * @param toType the Class type to convert to.
   * @return a boolean indicating whether this Converter can convert values from the source Class type
   * into the target Class type.
   * @see org.cp.elements.util.convert.ConversionService#canConvert(Class, Class)
   */
  @Override
  public boolean canConvert(Class<?> fromType, Class<?> toType) {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Converts a value of type S into a value of type T.
   * <p/>
   * @param value the S typed value to convert into a value of type T.
   * @return the converted value.
   * @throws ConversionException if the value cannot be converted.
   * @see #convert(Object, Class)
   * @see org.cp.elements.util.convert.ConversionService#convert(Object, Class)
   */
  @Override
  public T convert(final S value) {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Converts a value of type S into a value of the qualifying type QT.
   * <p/>
   * @param <QT> qualifying Class type extending the type parameter T.
   * @param value the S typed value to convert into a value of the qualifying type QT.
   * @param qualifyingType the qualifying Class type to convert the value into.
   * @return the converted value.
   * @throws ConversionException if the value cannot be converted.
   * @see #convert(Object)
   * @see org.cp.elements.util.convert.ConversionService#convert(Object, Class)
   */
  @Override
  public <QT extends T> QT convert(final S value, final Class<QT> qualifyingType) {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

}
