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
import org.cp.elements.lang.StringUtils;

/**
 * The ByteConverter class converts an Object value into a Byte.
 *
 * @author John J. Blum
 * @see java.lang.Byte
 * @see java.lang.Object
 * @see org.cp.elements.data.conversion.AbstractConverter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ByteConverter extends AbstractConverter<Object, Byte> {

  protected static final String CONVERSION_EXCEPTION_MESSAGE = "[%s] is not a valid byte";

  @Override
  public boolean canConvert(Class<?> fromType, Class<?> toType) {
    return isAssignableTo(fromType, Number.class, String.class) && Byte.class.equals(toType);
  }

  @Override
  public Byte convert(Object value) {

    if (value instanceof Number) {
      return ((Number) value).byteValue();
    }
    else if (value instanceof String && StringUtils.containsDigits(value.toString().trim())) {
      try {
        return Byte.parseByte(value.toString().trim());
      }
      catch (NumberFormatException cause) {
        throw newConversionException(cause, CONVERSION_EXCEPTION_MESSAGE, value);
      }
    }
    else {
      throw newConversionException(CONVERSION_EXCEPTION_MESSAGE, value);
    }
  }
}
