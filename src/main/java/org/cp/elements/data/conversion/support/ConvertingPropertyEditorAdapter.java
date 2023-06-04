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

import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalArgumentException;

import java.beans.PropertyEditor;
import java.beans.PropertyEditorSupport;

import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.data.conversion.Converter;
import org.cp.elements.lang.Assert;

/**
 * The {@link ConvertingPropertyEditorAdapter} class is an Adapter adapting the {@link Converter} interface
 * into an instance of {@link PropertyEditor}.
 *
 * @author John Blum
 * @see java.beans.PropertyEditor
 * @see org.cp.elements.data.conversion.Converter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ConvertingPropertyEditorAdapter extends PropertyEditorSupport {

  private final Converter<String, ?> converter;

  /**
   * Constructs a new the {@link ConvertingPropertyEditorAdapter} initialized with
   * the given {@link Converter}.
   *
   * @param converter {@link Converter} backing this {@link PropertyEditor}.
   * @return a new instance of {@link ConvertingPropertyEditorAdapter} initialized with
   * the given {@link Converter}.
   * @throws IllegalArgumentException if {@link Converter} is {@literal null}.
   * @see #ConvertingPropertyEditorAdapter(Converter)
   * @see org.cp.elements.data.conversion.Converter
   */
  public static ConvertingPropertyEditorAdapter of(Converter<String, ?> converter) {
    return new ConvertingPropertyEditorAdapter(converter);
  }

  /**
   * Constructs a new the {@link ConvertingPropertyEditorAdapter} initialized with
   * the given {@link Converter}.
   *
   * @param converter {@link Converter} backing this {@link PropertyEditor}.
   * @throws IllegalArgumentException if {@link Converter} is {@literal null}.
   * @see org.cp.elements.data.conversion.Converter
   */
  protected ConvertingPropertyEditorAdapter(Converter<String, ?> converter) {

    Assert.notNull(converter, "Converter is required");

    this.converter = converter;
  }

  /**
   * Returns a reference to the configured {@link Converter}.
   *
   * @return a reference to the configured {@link Converter}.
   * @see org.cp.elements.data.conversion.Converter
   */
  protected Converter<String, ?> getConverter() {
    return this.converter;
  }

  /**
   * Converts the given {@link String text} into an {@link Object}.
   *
   * @param text {@link String} to convert.
   * @throws IllegalArgumentException if the given {@link String text}
   * cannot be {@link Converter#convert(Object) converted} into an {@link Object}.
   * @see org.cp.elements.data.conversion.Converter#convert(Object)
   * @see #getConverter()
   */
  @Override
  public void setAsText(String text) throws IllegalArgumentException {

    try {
      setValue(getConverter().convert(text));
    }
    catch (ConversionException cause) {
      throw newIllegalArgumentException(cause, "Could not set text [%s] as value", text);
    }
  }
}
