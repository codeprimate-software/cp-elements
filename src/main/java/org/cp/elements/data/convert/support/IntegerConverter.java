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

package org.cp.elements.data.convert.support;

import org.cp.elements.data.convert.ConversionException;
import org.cp.elements.data.convert.ConverterAdapter;
import org.cp.elements.lang.StringUtils;

/**
 * The IntegerConverter class converts an Object value into a Integer.
 *
 * @author John J. Blum
 * @see java.lang.Integer
 * @see java.lang.Object
 * @see org.cp.elements.data.convert.ConverterAdapter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class IntegerConverter extends ConverterAdapter<Object, Integer> {

  protected static final String CONVERSION_EXCEPTION_MESSAGE = "The Object value (%1$s) is not a valid integer!";

  @Override
  public boolean canConvert(final Class<?> fromType, final Class<?> toType) {
    return (isAssignableTo(fromType, Number.class, String.class) && Integer.class.equals(toType));
  }

  @Override
  public Integer convert(final Object value) {
    if (value instanceof Number) {
      return ((Number) value).intValue();
    }
    else if (value instanceof String && StringUtils.containsDigits(value.toString().trim())) {
      try {
        return Integer.parseInt(value.toString().trim());
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
