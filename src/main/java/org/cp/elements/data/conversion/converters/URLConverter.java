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

import java.net.URI;
import java.net.URL;

import org.cp.elements.data.conversion.AbstractConverter;

/**
 * {@link URLConverter} converts an {@link Object} into a {@link URL}.
 *
 * @author John J. Blum
 * @see java.lang.Object
 * @see java.net.URL
 * @see org.cp.elements.data.conversion.AbstractConverter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class URLConverter extends AbstractConverter<Object, URL> {

  @Override
  public boolean canConvert(Class<?> fromType, Class<?> toType) {
    return isAssignableTo(fromType, URI.class, URL.class, String.class) && URL.class.equals(toType);
  }

  @Override
  public URL convert(Object value) {

    try {
      if (value instanceof URL) {
        return (URL) value;
      }
      else if (value instanceof URI) {
        return ((URI) value).toURL();
      }
      else if (value instanceof String) {
        return new URL(value.toString().trim());
      }
      else {
        throw newConversionException("[%s] is not a valid URL", value);
      }
    }
    catch (Exception cause) {
      throw newConversionException(cause, "[%s] is not a valid URL", value);
    }
  }
}
