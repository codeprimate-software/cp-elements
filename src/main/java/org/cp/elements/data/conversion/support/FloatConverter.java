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

package org.cp.elements.data.conversion.support;

import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.data.conversion.ConverterAdapter;
import org.cp.elements.lang.StringUtils;

/**
 * The FloatConverter class converts an Object value into a Float.
 *
 * @author John J. Blum
 * @see java.lang.Float
 * @see java.lang.Object
 * @see org.cp.elements.data.conversion.ConverterAdapter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class FloatConverter extends ConverterAdapter<Object, Float> {

  protected static final String CONVERSION_EXCEPTION_MESSAGE = "The Object value (%1$s) is not a valid float!";

  @Override
  public boolean canConvert(final Class<?> fromType, final Class<?> toType) {
    return (isAssignableTo(fromType, Number.class, String.class) && Float.class.equals(toType));
  }

  @Override
  public Float convert(final Object value) {
    if (value instanceof Number) {
      return ((Number) value).floatValue();
    }
    else if (value instanceof String && StringUtils.containsDigits(value.toString().trim())) {
      try {
        return Float.parseFloat(value.toString().trim());
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
