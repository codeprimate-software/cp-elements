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

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import org.cp.elements.data.conversion.ConverterAdapter;

/**
 * The BooleanConverter class converts an Object value into a Boolean.
 *
 * @author John J. Blum
 * @see java.lang.Boolean
 * @see java.lang.Object
 * @see org.cp.elements.data.conversion.ConverterAdapter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class BooleanConverter extends ConverterAdapter<Object, Boolean> {

  protected final Set<String> trueValues;

  public BooleanConverter() {
    this(Boolean.TRUE.toString());
  }

  public BooleanConverter(final String... trueValues) {
    this.trueValues = (trueValues == null ? Collections.<String>emptySet()
      : Collections.unmodifiableSet(new HashSet<>(Arrays.asList(trueValues))));
  }

  protected boolean isTrue(final Object value) {
    final String valueString = String.valueOf(value).trim();
    return (Boolean.parseBoolean(valueString) || isTrue(valueString));
  }

  protected boolean isTrue(final String value) {
    for (String trueValue : trueValues) {
      if (trueValue.equalsIgnoreCase(value)) {
        return true;
      }
    }

    return false;
  }

  @Override
  public boolean canConvert(final Class<?> fromType, final Class<?> toType) {
    return (fromType != null && (Boolean.class.equals(toType) || Boolean.TYPE.equals(toType)));
  }

  @Override
  public Boolean convert(final Object value) {
    if (value instanceof Boolean) {
      return (Boolean) value;
    }

    return isTrue(value);
  }

}
