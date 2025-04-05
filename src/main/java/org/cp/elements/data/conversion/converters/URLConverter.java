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

import java.net.URI;
import java.net.URL;

import org.cp.elements.data.conversion.AbstractConverter;
import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.data.conversion.Converter;
import org.cp.elements.lang.annotation.NullSafe;

/**
 * {@link URLConverter} converts an {@link Object} to a {@link URL}.
 *
 * @author John J. Blum
 * @see java.lang.Object
 * @see java.net.URL
 * @see org.cp.elements.data.conversion.AbstractConverter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class URLConverter extends AbstractConverter<Object, URL> {

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
  @NullSafe
  @Override
  public boolean canConvert(Class<?> fromType, Class<?> toType) {
    return fromType != null && isAssignableTo(fromType, URI.class, URL.class, String.class)
      && URL.class.equals(toType);
  }

  /**
   * Converts an {@link Object} to {@link URL}.
   *
   * @param value {@link Object} to convert into a {@link URL}.
   * @return the {@link Object} converted to a {@link URL}.
   * @throws ConversionException if the {@link Object} cannot be converted.
   * @see org.cp.elements.data.conversion.ConversionService#convert(Object, Class)
   * @see #convert(Object, Class)
   * @see java.net.URL
   */
  @Override
  public URL convert(Object value) {

    try {
      return switch (value) {
        case URL url -> url;
        case URI uri -> uri.toURL();
        case String string -> URI.create(string).toURL();
        case null, default -> throw newConversionException("[%s] is not a valid URL", value);
      };
    }
    catch (Exception cause) {
      if (cause instanceof ConversionException conversionException) {
        throw conversionException;
      }

      throw newConversionException(cause, "[%s] is not a valid URL", value);
    }
  }
}
