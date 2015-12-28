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

package org.cp.elements.util.convert;

import org.cp.elements.service.ServiceSupport;

/**
 * The ConversionService interface defines a contract for Service objects responsible for performing type conversions.
 *
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
   *
   * @param value the object to convert into a value of the target Class type.
   * @param toType the Class type to convert the Object value into.
   * @return a boolean value indicating whether this ConversionService can convert the object into a value of the
   * desired Class type.
   * @see #canConvert(Class, Class)
   */
  boolean canConvert(Object value, Class<?> toType);

  /**
   * Determines whether this ConversionService can convert values from a given Class type into the desired Class type.
   *
   * @param fromType the Class type to convert from.
   * @param toType the Class type to convert to.
   * @return a boolean value indicating whether this ConversionService can convert values from a given Class type
   * into the desired Class type.
   * @see #canConvert(Object, Class)
   * @see org.cp.elements.util.convert.Converter#canConvert(Class, Class)
   */
  boolean canConvert(Class<?> fromType, Class<?> toType);

  /**
   * Converts the Object value into a value of the target Class type.
   *
   * @param <T> the target Class type for the conversion.
   * @param value the Object value to convert.
   * @param toType the Class type to convert the Object value into.
   * @return an instance of the Object value converted into a value of the target Class type.
   * @throws ConversionException if converting the Object value into a value of the target Class type results in error.
   * @see java.lang.Class
   * @see org.cp.elements.util.convert.Converter#convert(Object)
   */
  <T> T convert(Object value, Class<T> toType);

}
