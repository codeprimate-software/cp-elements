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

import org.cp.elements.data.convert.ConverterAdapter;

/**
 * The StringConverter class converts an Object value into a String.
 *
 * @author John J. Blum
 * @see java.lang.Object
 * @see java.lang.String
 * @see org.cp.elements.data.convert.ConverterAdapter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class StringConverter extends ConverterAdapter<Object, String> {

  @Override
  public boolean canConvert(final Class<?> fromType, final Class<?> toType) {
    return String.class.equals(toType);
  }

  @Override
  public String convert(final Object value) {
    return String.valueOf(value);
  }

}
