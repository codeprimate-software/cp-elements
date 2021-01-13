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

package org.cp.elements.data.conversion.support;

import static org.cp.elements.lang.ElementsExceptionsFactory.newConversionException;

import java.beans.PropertyEditor;

import org.cp.elements.data.conversion.AbstractConverter;
import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.data.conversion.Converter;
import org.cp.elements.lang.Assert;

/**
 * The {@link PropertyEditingConverterAdapter} class is an Adapter adapting the {@link PropertyEditor} interface
 * into an instance of {@link Converter}.
 *
 * @author John Blum
 * @see java.beans.PropertyEditor
 * @see org.cp.elements.data.conversion.Converter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class PropertyEditingConverterAdapter extends AbstractConverter<String, Object> {

  private final PropertyEditor propertyEditor;

  /**
   * Factory method used to construct a new instance of the {@link PropertyEditingConverterAdapter} initialized with
   * the given {@link PropertyEditor}.
   *
   * @param propertyEditor {@link PropertyEditor} backing this {@link Converter}.
   * @return a new instance of {@link PropertyEditingConverterAdapter} initialized with
   * the given {@link PropertyEditor}.
   * @throws IllegalArgumentException if {@link PropertyEditor} is {@literal null}.
   * @see #PropertyEditingConverterAdapter(PropertyEditor)
   * @see java.beans.PropertyEditor
   */
  public static PropertyEditingConverterAdapter of(PropertyEditor propertyEditor) {
    return new PropertyEditingConverterAdapter(propertyEditor);
  }

  /**
   * Constructs a new instance of {@link PropertyEditingConverterAdapter} initialized with
   * the given {@link PropertyEditor}.
   *
   * @param propertyEditor {@link PropertyEditor} backing this {@link Converter}.
   * @throws IllegalArgumentException if {@link PropertyEditor} is {@literal null}.
   * @see java.beans.PropertyEditor
   */
  protected PropertyEditingConverterAdapter(PropertyEditor propertyEditor) {

    Assert.notNull(propertyEditor, "PropertyEditor is required");

    this.propertyEditor = propertyEditor;
  }

  /**
   * Returns a reference to the configured {@link PropertyEditor}.
   *
   * @return a reference to the configured {@link PropertyEditor}.
   * @see java.beans.PropertyEditor
   */
  protected PropertyEditor getPropertyEditor() {
    return this.propertyEditor;
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
  public Object convert(String value) {

    try {
      getPropertyEditor().setAsText(value);

      return getPropertyEditor().getValue();
    }
    catch (IllegalArgumentException cause) {
      throw newConversionException(cause, "Cannot convert [%s] to an Object type", value);
    }
  }
}
