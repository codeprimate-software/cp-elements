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

import java.net.URI;
import java.net.URL;

import org.cp.elements.data.convert.ConversionException;
import org.cp.elements.data.convert.ConverterAdapter;

/**
 * The URLConverter class converts an Object value into a URL.
 *
 * @author John J. Blum
 * @see java.lang.Object
 * @see java.net.URL
 * @see org.cp.elements.data.convert.ConverterAdapter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class URLConverter extends ConverterAdapter<Object, URL> {

  @Override
  public boolean canConvert(final Class<?> fromType, final Class<?> toType) {
    return (isAssignableTo(fromType, URI.class, URL.class, String.class) && URL.class.equals(toType));
  }

  @Override
  public URL convert(final Object value) {
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
        throw new ConversionException(String.format("The Object value (%1$s) is not a valid URL!", value));
      }
    }
    catch (Exception e) {
      throw new ConversionException(String.format("The Object value (%1$s) is not a valid URL!", value), e);
    }
  }

}
