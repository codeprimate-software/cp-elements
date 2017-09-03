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

/**
 * The CharacterConverter class converts an Object value into a Character.
 *
 * @author John J. Blum
 * @see java.lang.Character
 * @see java.lang.Object
 * @see org.cp.elements.data.convert.ConverterAdapter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class CharacterConverter extends ConverterAdapter<Object, Character> {

  @Override
  public boolean canConvert(final Class<?> fromType, final Class<?> toType) {
    return (isAssignableTo(fromType, Character.class, String.class) && Character.class.equals(toType));
  }

  @Override
  public Character convert(final Object value) {
    if (value instanceof Character) {
      return (Character) value;
    }
    else if (value instanceof String) {
      final String valueString = value.toString();
      return (valueString.length() > 0 ? valueString.charAt(0) : '\0');
    }
    else {
      throw new ConversionException(String.format("The Object value (%1$s) is not a valid character!", value));
    }
  }

}
