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

/**
 * The Converter interface defines a contract for objects that convert an Object value from one type to another.
 *
 * @author John J. Blum
 * @param <S> the source Class type to convert from.
 * @param <T> the target Class type to convert to.
 * @see org.cp.elements.util.convert.AbstractConverter
 * @see org.cp.elements.util.convert.ConverterAdapter
 * @see org.cp.elements.util.convert.ConversionService
 * @see org.cp.elements.util.convert.ConversionServiceAware
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Converter<S, T> extends ConversionServiceAware {

  /**
   * Determines whether this Converter can convert values from the source Class type into the target Class type.
   *
   * @param fromType the Class type to convert from.
   * @param toType the Class type to convert to.
   * @return a boolean indicating whether this Converter can convert values from the source Class type
   * into the target Class type.
   * @see org.cp.elements.util.convert.ConversionService#canConvert(Class, Class)
   */
  boolean canConvert(Class<?> fromType, Class<?> toType);

  /**
   * Converts a value of type S into a value of type T.
   *
   * @param value the S typed value to convert into a value of type T.
   * @return the converted value.
   * @throws ConversionException if the value cannot be converted.
   * @see #convert(Object, Class)
   * @see org.cp.elements.util.convert.ConversionService#convert(Object, Class)
   */
  T convert(S value);

  /**
   * Converts a value of type S into a value of the qualifying type QT.
   *
   * @param <QT> qualifying Class type extending the type parameter T.
   * @param value the S typed value to convert into a value of the qualifying type QT.
   * @param qualifyingType the qualifying Class type to convert the value into.
   * @return the converted value.
   * @throws ConversionException if the value cannot be converted.
   * @see #convert(Object)
   * @see org.cp.elements.util.convert.ConversionService#convert(Object, Class)
   */
  <QT extends T> QT convert(S value, Class<QT> qualifyingType);

}
