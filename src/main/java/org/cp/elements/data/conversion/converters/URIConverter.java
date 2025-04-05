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
 * {@link URIConverter} converts an {@link Object} to a {@link URI}.
 *
 * @author John J. Blum
 * @see java.lang.Object
 * @see java.net.URI
 * @see org.cp.elements.data.conversion.AbstractConverter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class URIConverter extends AbstractConverter<Object, URI> {

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
      && URI.class.equals(toType);
  }

  /**
   * Converts an {@link Object} of {@link Class type S} into an {@link Object} of {@link Class type T}.
   *
   * @param value {@link Object} of {@link Class type S} to convert.
   * @return the converted {@link Object} of {@link Class type T}.
   * @throws ConversionException if the {@link Object} cannot be converted.
   * @see org.cp.elements.data.conversion.ConversionService#convert(Object, Class)
   * @see #convert(Object, Class)
   */
  @Override
  public URI convert(Object value) {

    try {
      return switch (value) {
        case URI uri -> uri;
        case URL url -> url.toURI();
        case String string -> URI.create(string.trim());
        case null, default -> throw newConversionException("[%s] is not a valid URI", value);
      };
    }
    catch (Exception cause) {
      if (cause instanceof ConversionException conversionException) {
        throw conversionException;
      }

      throw newConversionException(cause, "[%s] is not a valid URI", value);
    }
  }
}
