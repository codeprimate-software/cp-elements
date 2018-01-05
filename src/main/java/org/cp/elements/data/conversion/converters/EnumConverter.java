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

package org.cp.elements.data.conversion.converters;

import static org.cp.elements.lang.ElementsExceptionsFactory.newConversionException;

import org.cp.elements.data.conversion.AbstractConverter;

/**
 * The EnumConverter class converts a String value into an Enum of the qualifying enumerated type.
 *
 * @author John J. Blum
 * @see java.lang.Enum
 * @see java.lang.Object
 * @see org.cp.elements.data.conversion.AbstractConverter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class EnumConverter extends AbstractConverter<String, Enum> {

  /* (non-Javadoc) */
  @Override
  public boolean canConvert(Class<?> fromType, Class<?> toType) {
    return String.class.equals(fromType) && isAssignableTo(toType, Enum.class);
  }

  /* (non-Javadoc) */
  @Override
  @SuppressWarnings("unchecked")
  public <QT extends Enum> QT convert(String value, Class<QT> enumType) {

    try {

      Enum enumInstance = Enum.valueOf(enumType, value);

      return enumType.cast(enumInstance);
    }
    catch (Exception cause) {
      throw newConversionException(cause, "[%1$s] is not a valid enumerated value of Enum [%2$s]", value, enumType);
    }
  }
}
