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

package org.cp.elements.util.convert.support;

import org.cp.elements.util.convert.ConversionException;
import org.cp.elements.util.convert.ConverterAdapter;

/**
 * The EnumConverter class converts a String value into an Enum of the qualifying enumerated type.
 *
 * @author John J. Blum
 * @see java.lang.Enum
 * @see java.lang.Object
 * @see org.cp.elements.util.convert.ConverterAdapter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class EnumConverter extends ConverterAdapter<String, Enum> {

  @Override
  public boolean canConvert(final Class<?> fromType, final Class<?> toType) {
    return (String.class.equals(fromType) && isAssignableTo(toType, Enum.class));
  }

  @Override
  public <QT extends Enum> QT convert(final String value, final Class<QT> enumType) {
    try {
      Enum enumInstance = Enum.valueOf(enumType, value);
      return enumType.cast(enumInstance);
    }
    catch (Exception e) {
      throw new ConversionException(String.format("(%1$s) is not a valid enumerated value of Enum (%2$s)",
        value, enumType), e);
    }
  }

}
