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
package org.cp.elements.data.conversion;

import static org.cp.elements.lang.ElementsExceptionsFactory.newConversionException;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * The {@link AbstractConversionService} class is an abstract base class encapsulating functionality
 * common to all service application components that perform value {@link Class type} conversions.
 *
 * @author John J. Blum
 * @see org.cp.elements.data.conversion.AbstractConverterRegistry
 * @see org.cp.elements.data.conversion.ConversionService
 * @see org.cp.elements.data.conversion.Converter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractConversionService extends AbstractConverterRegistry implements ConversionService {

  /**
   * Converts the given {@link Object} into a {@link Object value} of the {@link Class target type}.
   *
   * @param <T> {@link Class target type} of the conversion.
   * @param value {@link Object} to convert.
   * @param toType {@link Class target type} to convert the {@link Object} into.
   * @return the converted {@link Object} of the {@link Class target type} with the given {@code value}.
   * @throws ConversionException if the {@link Object} could not be converted into a {@link Object value}
   * of the desired {@link Class target type}.
   * @see org.cp.elements.data.conversion.Converter#convert(Object, Class)
   * @see org.cp.elements.data.conversion.Converter#convert(Object)
   */
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public <T> T convert(Object value, Class<T> toType) {

    List<ConverterDescriptor> descriptors = new ArrayList<>(getRegistry().keySet());

    Collections.sort(descriptors);

    for (ConverterDescriptor descriptor : descriptors) {

      Converter converter = descriptor.getConverter();

      if (converter.canConvert(value, toType)) {
        if (descriptor.isExactConversion(toType)) {
          return toType.cast(converter.convert(value));
        }
        else {
          return toType.cast(converter.convert(value, toType));
        }
      }
    }

    throw newConversionException("Cannot convert [%1$s] into Object of type [%2$s]",
      value, toType.getName());
  }
}
