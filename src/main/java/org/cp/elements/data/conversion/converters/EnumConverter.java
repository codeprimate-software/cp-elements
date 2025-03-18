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

import org.cp.elements.data.conversion.AbstractConverter;
import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.data.conversion.Converter;

/**
 * {@link EnumConverter} converts a {@link String} to an {@link Enum} of the {@link Class qualifying enumerated type}.
 *
 * @author John J. Blum
 * @see java.lang.Enum
 * @see java.lang.String
 * @see org.cp.elements.data.conversion.AbstractConverter
 * @since 1.0.0
 */
@SuppressWarnings({ "rawtypes", "unused" })
public class EnumConverter extends AbstractConverter<String, Enum> {

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
    return String.class.equals(fromType) && isAssignableTo(toType, Enum.class);
  }

  /**
   * Converts an {@link Object} of {@link Class type S} into an {@link Object} of {@link Class type T}.
   *
   * @param value {@link Object} of {@link Class type S} to convert.
   * @return the converted {@link Object} of {@link Class type T}.
   * @throws ConversionException if the {@link Object} cannot be converted.
   * @see org.cp.elements.data.conversion.ConversionService#convert(Object, Class)
   */
  @Override
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public <QT extends Enum> QT convert(String value, Class<QT> enumType) {

    try {

      Enum enumInstance = Enum.valueOf(enumType, value);

      return enumType.cast(enumInstance);
    }
    catch (Exception cause) {
      throw newConversionException(cause, "[%1$s] is not a valid enumerated value of Enum [%2$s]",
        value, enumType.getName());
    }
  }
}
