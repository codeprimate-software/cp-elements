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

import org.cp.elements.service.ServiceSupport;

/**
 * The ConversionService interface defines a contract for Service objects responsible for performing type conversions.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.service.ServiceSupport
 * @see org.cp.elements.util.convert.AbstractConversionService
 * @see org.cp.elements.util.convert.Converter
 * @see org.cp.elements.util.convert.ConverterRegistry
 * @see org.cp.elements.util.convert.provider.DefaultConversionService
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface ConversionService extends ConverterRegistry, ServiceSupport {

  /**
   * Determines whether this ConversionService can convert a given object into a value of the desired Class type.
   * <p/>
   * @param value the object to convert into a value of the target Class type.
   * @param toType the Class type to convert the Object value into.
   * @return a boolean value indicating whether this ConversionService can convert the object into a value of the
   * desired Class type.
   * @see #canConvert(Class, Class)
   */
  public boolean canConvert(Object value, Class<?> toType);

  /**
   * Determines whether this ConversionService can convert values from a given Class type into the desired Class type.
   * <p/>
   * @param fromType the Class type to convert from.
   * @param toType the Class type to convert to.
   * @return a boolean value indicating whether this ConversionService can convert values from a given Class type
   * into the desired Class type.
   * @see #canConvert(Object, Class)
   * @see org.cp.elements.util.convert.Converter#canConvert(Class, Class)
   */
  public boolean canConvert(Class<?> fromType, Class<?> toType);

  /**
   * Converts the Object value into a value of the target Class type.
   * <p/>
   * @param <T> the target Class type for the conversion.
   * @param value the Object value to convert.
   * @param toType the Class type to convert the Object value into.
   * @return an instance of the Object value converted into a value of the target Class type.
   * @throws ConversionException if converting the Object value into a value of the target Class type results in error.
   * @see java.lang.Class
   * @see org.cp.elements.util.convert.Converter#convert(Object)
   */
  public <T> T convert(Object value, Class<T> toType);

}
