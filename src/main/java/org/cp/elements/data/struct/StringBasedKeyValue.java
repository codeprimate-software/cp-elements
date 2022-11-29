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

import org.cp.elements.data.conversion.ConversionService;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Abstract extension and implementation of the {@link SimpleKeyValue} data structure where both the {@link Object key}
 * and {@link Object value} are {@link String Strings}.
 *
 * @author John Blum
 * @see java.lang.String
 * @see org.cp.elements.data.struct.SimpleKeyValue
 * @see org.cp.elements.data.conversion.ConversionService
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class StringBasedKeyValue extends SimpleKeyValue<String, String> {

  private final ConversionService conversionService = ConversionService.getLoader().getServiceInstance();

  /**
   * Constructs a new instance of {@link StringBasedKeyValue} initialized with the given {@link String key}
   * having no value.
   *
   * @param key {@link String key} in this key/value data structure.
   */
  protected StringBasedKeyValue(@NotNull String key) {
    super(key);
  }

  /**
   * Constructs a new instance of {@link StringBasedKeyValue} initialized with the given {@link String key}
   * and {@link String value}.
   *
   * @param key {@link String key} in this key/value data structure; must not be {@literal null} or {@literal empty}.
   * @param value {@link String value} in this key/value data structure.
   * @throws IllegalArgumentException if the {@link String key} is {@literal null} or {@literal empty}.
   */
  protected StringBasedKeyValue(@NotNull String key, @Nullable String value) {
    super(StringUtils.requireText(key, "Key [%s] is required"), value);
  }

  /**
   * Returns a reference to the configured {@link ConversionService} used to convert {@link String values}
   * into the requested {@link Class type}.
   *
   * @return a reference to the configured {@link ConversionService}.
   * @see org.cp.elements.data.conversion.ConversionService
   */
  protected @NotNull ConversionService getConversionService() {
    return this.conversionService;
  }

  @Override
  public Optional<String> getValue() {
    return super.getValue()
      .filter(StringUtils::hasText);
  }

  /**
   * Returns the {@link String value} as an instance of the requested, required {@link Class type}.
   *
   * @param <T> {@link Class type} used in the conversion of the {@link String value}.
   * @param type {@link Class type} used in the conversion of the {@link String value}.
   * @return the {@link String value} of this key/value data structure as an instance of
   * the requested {@link Class type}.
   * @see org.cp.elements.data.conversion.ConversionService#convert(Object, Class)
   * @see #getConversionService()
   * @see java.lang.Class
   * @see #getValue()
   */
  public <T> Optional<T> getValueAs(@NotNull Class<T> type) {

    return getValue()
      .filter(StringUtils::hasText)
      .map(value -> getConversionService().convert(value, type));
  }

  /**
   * Returns the {@link String value} as an instance of the requested, required {@link Class type}
   * or returns the {@link T defaultValue} if the {@link #getValue() value} is {@literal null}
   * or {@link String empty}.
   *
   * @param <T> {@link Class type} used in the conversion of the {@link String value}.
   * @param type {@link Class type} used in the conversion of the {@link String value}.
   * @param defaultValue {@link T value} to return if {@link #getValue() value} is {@literal null}
   * or {@link #getValue() value} is {@link String empty}.
   * @return the {@link String value} of this key/value data structure as an instance of
   * the requested {@link Class type}.
   * @see #getConversionService()
   * @see #getValue()
   * @see java.lang.Class
   */
  public @Nullable <T> T getValueAs(@NotNull Class<T> type, @Nullable T defaultValue) {

    return getValue()
      .filter(StringUtils::hasText)
      .map(value -> getConversionService().convert(value, type))
      .orElse(defaultValue);
  }
}
