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

import static org.cp.elements.util.ArrayUtils.nullSafeArray;
import static org.cp.elements.util.CollectionUtils.asSet;

import java.util.Collections;
import java.util.Set;

import org.cp.elements.data.conversion.AbstractConverter;

/**
 * The BooleanConverter class converts an Object value into a Boolean.
 *
 * @author John J. Blum
 * @see java.lang.Boolean
 * @see java.lang.Object
 * @see org.cp.elements.data.conversion.AbstractConverter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class BooleanConverter extends AbstractConverter<Object, Boolean> {

  protected final Set<String> trueValues;

  public BooleanConverter() {
    this(Boolean.TRUE.toString());
  }

  public BooleanConverter(String... trueValues) {
    this.trueValues = Collections.unmodifiableSet(asSet(nullSafeArray(trueValues, String.class)));
  }

  protected boolean isTrue(Object value) {

    String valueString = String.valueOf(value).trim();

    return Boolean.parseBoolean(valueString) || isTrue(valueString);
  }

  protected boolean isTrue(String value) {

    for (String trueValue : this.trueValues) {
      if (trueValue.equalsIgnoreCase(value)) {
        return true;
      }
    }

    return false;
  }

  @Override
  public boolean canConvert(Class<?> fromType, Class<?> toType) {
    return fromType != null && (Boolean.class.equals(toType) || Boolean.TYPE.equals(toType));
  }

  @Override
  public Boolean convert(Object value) {
    return value instanceof Boolean ? (Boolean) value : isTrue(value);
  }
}
