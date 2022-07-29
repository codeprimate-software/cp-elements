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
package org.cp.elements.data.struct;

import java.util.Optional;
import java.util.ServiceLoader;

import org.cp.elements.data.conversion.ConversionService;
import org.cp.elements.lang.StringUtils;

/**
 * Implementation of the {@link SimpleKeyValue} data structure where both the key and value are {@link String Strings}.
 *
 * @author John Blum
 * @see java.lang.String
 * @see org.cp.elements.data.struct.SimpleKeyValue
 * @see org.cp.elements.data.conversion.ConversionService
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class StringBasedKeyValue extends SimpleKeyValue<String, String> {

  private final ConversionService conversionService = ServiceLoader.load(ConversionService.class).iterator().next();

  /**
   * Constructs an instance of the {@link StringBasedKeyValue} initialized with the given {@link String key}
   * having no value.
   *
   * @param key {@link String key} in the key/value data structure.
   */
  protected StringBasedKeyValue(String key) {
    super(key);
  }

  /**
   * Constructs an instance of the {@link StringBasedKeyValue} initialized with the given {@link String key}
   * and {@link String value}.
   *
   * @param key {@link String key} in the key/value data structure.
   * @param value {@link String value} in the key/value data structure.
   */
  protected StringBasedKeyValue(String key, String value) {
    super(key, value);
  }

  /**
   * Returns a reference to the configured {@link ConversionService} used to convert values
   * into the desired {@link Class type}.
   *
   * @return a reference to the configured {@link ConversionService}.
   * @see org.cp.elements.data.conversion.ConversionService
   */
  protected ConversionService getConversionService() {
    return this.conversionService;
  }

  /**
   * Returns the {@link String value} as an instance of the given {@link Class type}.
   *
   * @param <T> {@link Class type} used in the conversion of the {@link String value}.
   * @param type {@link Class type} used in the conversion of the {@link String value}.
   * @return the {@link String value} of this key/value data structure as an instance
   * of the given {@link Class type}.
   * @see #getConversionService()
   * @see #getValue()
   * @see java.lang.Class
   */
  public <T> Optional<T> getValueAs(Class<T> type) {

    return getValue()
      .filter(StringUtils::hasText)
      .map(value -> getConversionService().convert(value, type));
  }

  /**
   * Returns the {@link String value} as an instance of the given {@link Class type}
   * or returns the {@literal defaultValue} if the {@link #getValue() value} is {@literal null}
   * or {@link String blank/empty}.
   *
   * @param <T> {@link Class type} used in the conversion of the {@link String value}.
   * @param type {@link Class type} used in the conversion of the {@link String value}.
   * @param defaultValue value to return if {@link #getValue() value} is {@literal null}
   * or {@link #getValue() value} is {@link String blank/empty}.
   * @return the {@link String value} of this key/value data structure as an instance
   * of the given {@link Class type}.
   * @see #getConversionService()
   * @see #getValue()
   * @see java.lang.Class
   */
  public <T> T getValueAs(Class<T> type, T defaultValue) {

    return getValue()
      .filter(StringUtils::hasText)
      .map(value -> getConversionService().convert(value, type))
      .orElse(defaultValue);
  }
}
