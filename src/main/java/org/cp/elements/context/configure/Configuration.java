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
package org.cp.elements.context.configure;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Iterator;
import java.util.Optional;
import java.util.function.Supplier;

import org.cp.elements.context.annotation.Profile;
import org.cp.elements.data.conversion.provider.SimpleTypeConversions;
import org.cp.elements.lang.Constants;
import org.cp.elements.lang.Nameable;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.util.ArrayUtils;

/**
 * Abstract Data Type (ADT) modeling configuration metadata used to configure any application or program.
 *
 * @author John Blum
 * @see java.io.Serializable
 * @see java.lang.FunctionalInterface
 * @see java.lang.Iterable
 * @see org.cp.elements.context.annotation.Profile
 * @see org.cp.elements.lang.Nameable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
@FunctionalInterface
public interface Configuration extends Iterable<String>, Nameable<String>, Serializable {

  boolean REQUIRED = true;
  boolean NOT_REQUIRED = false;
  boolean DEFAULT_REQUIRED = REQUIRED;

  /**
   * Determines whether the configuration property identified by the given {@link String name} is present
   * in the configuration metadata.
   *
   * Even if the configuration property was declared (present) does not mean the property is defined (set).
   *
   * @param propertyName {@link String} containing the {@literal name} of the configuration property.
   * @return a boolean value indicating whether the property identified by the given {@link String name}
   * is present (declared) in the configuration metadata.
   * @see #isSet(String)
   */
  @NullSafe
  default boolean isPresent(@Nullable String propertyName) {
    return isSet(propertyName);
  }

  /**
   * Determines whether the configuration property identified by the given {@link String name} is set
   * in the configuration metadata.
   *
   * If a configuration property is set then it means the property was both declared and defined with a value.
   *
   * @param propertyName {@link String} containing the {@literal name} of the configuration property.
   * @return a boolean value indicating whether the configuration property identified by the given {@link String name}
   * is set in the configuration metadata.
   * @see #getPropertyValue(String, boolean)
   * @see #isPresent(String)
   */
  @NullSafe
  default boolean isSet(@Nullable String propertyName) {
    return StringUtils.hasText(propertyName) && StringUtils.hasText(getPropertyValue(propertyName, NOT_REQUIRED));
  }

  /**
   * Returns the {@link String name} of this {@link Configuration}.
   *
   * By default, this method returns the {@link String} of the implementing {@link Configuration} class;
   * this method should be overridden.
   *
   * @return the {@link String name} of this {@link Configuration}.
   * @see org.cp.elements.lang.Nameable#getName()
   */
  @Override
  default String getName() {
    return getClass().getName();
  }

  /**
   * Returns an array of {@link String values} declaring the {@literal names} of {@link Profile Profiles}
   * for which this {@link Configuration} should be applied.
   *
   * @return an array of {@link String Strings} declaring the {@literal names} of {@link Profile Profiles}
   * for which this {@link Configuration} should be applied.
   * @see org.cp.elements.context.annotation.Profile
   */
  default String[] getProfiles() {

    Class<?> type = getClass();

    return type.isAnnotationPresent(Profile.class)
      ? Arrays.stream(ArrayUtils.nullSafeArray(type.getAnnotation(Profile.class).names(), String.class))
        .filter(StringUtils::hasText)
        .toArray(String[]::new)
      : StringUtils.EMPTY_STRING_ARRAY;
  }

  /**
   * Gets the {@link String value} of the configuration property identified by the given {@link String name}.
   *
   * The configuration property is required to be declared and defined otherwise a {@link ConfigurationException}
   * will be thrown.
   *
   * @param propertyName {@link String} containing the {@literal name} of the configuration property.
   * @return the {@link String value} of the {@link String named} configuration property.
   * @throws ConfigurationException if the configuration property is undeclared or undefined.
   * @see #getPropertyValue(String, boolean)
   */
  default @NotNull String getPropertyValue(String propertyName) {
    return getPropertyValue(propertyName, REQUIRED);
  }

  /**
   * Gets the {@link String value} of the configuration property identified by the given {@link String name}.
   *
   * The {@code required} parameter is used to indicate whether the configuration property is {@literal required} or not
   * and whether a {@link ConfigurationException} should be thrown when the property is undeclared or undefined.
   *
   * @param propertyName {@link String} containing the {@literal name} of the configuration property.
   * @param required boolean value used to indicate whether the configuration property is required
   * to be both declared and defined.
   * @return the {@link String value} of the {@link String named} configuration property. May return
   * a {@literal null} {@link String value} for the configuration property if not required.
   * @throws ConfigurationException if the configuration property is required and the property
   * is undeclared or undefined.
   */
  String getPropertyValue(String propertyName, boolean required);

  /**
   * Gets the {@link String value} of the configuration property identified by the given {@link String name}.
   *
   * By passing an {@link String argument} to the {@code defaultPropertyValue} parameter, this effectively overrides
   * the required parameter to indicate that the configuration property is not required to be declared or defined,
   * and therefore falls back to the {@link String default value}.
   *
   * @param propertyName {@link String} containing the {@literal name} of the configuration property.
   * @param defaultPropertyValue {@link String default value} returned for the configuration property
   * when the property is undeclared or undefined.
   * @return the {@link String value} of the configuration property identified by the given {@link String name},
   * or the {@link String default value} if the property is undeclared or undefined.
   * @see #getPropertyValue(String, boolean)
   */
  default @Nullable String getPropertyValue(String propertyName, @Nullable String defaultPropertyValue) {
    return StringUtils.defaultIfBlank(getPropertyValue(propertyName, NOT_REQUIRED), defaultPropertyValue);
  }

  /**
   * Gets the {@link String value} of the configuration property identified by the given {@link String name}.
   *
   * By passing {@link Supplier} to the {@code defaultPropertyValue} parameter, this effectively overrides
   * the required parameter to indicate that the configuration property is not required to be declared or defined,
   * and therefore falls back to a {@link String default value} returned by the given {@link Supplier}.
   *
   * @param propertyName {@link String} containing the {@literal name} of the configuration property.
   * @param defaultPropertyValue {@link Supplier} used to supply the {@link String default value}
   * for the {@link String named} configuration property when undeclared or undefined.
   * @return the {@link String value} of the configuration property identified by the given {@link String name},
   * or the {@link String default value} supplied by the given {@link Supplier} when the property
   * is undeclared or undefined.
   * @see #getPropertyValue(String, boolean)
   * @see java.util.function.Supplier
   */
  default @Nullable String getPropertyValue(String propertyName, @Nullable Supplier<String> defaultPropertyValue) {

    return Optional.ofNullable(getPropertyValue(propertyName, NOT_REQUIRED))
      .filter(StringUtils::hasText)
      .orElseGet(defaultPropertyValue);
  }

  /**
   * Gets the value of the configuration property identified by the given {@link String name} as a {@link T value}
   * of the specified {@link Class type T}.
   *
   * The configuration property is required to be declared and defined otherwise a {@link ConfigurationException}
   * will be thrown.
   *
   * @param <T> {@link Class type} to convert the returned value of the configuration property to.
   * @param propertyName {@link String} containing the {@literal name} of the configuration property.
   * @param type expected {@link Class type} of the configuration property value.
   * @return the value of the configuration property identified by the given {@link String name}
   * as a {@link T value} of the specified {@link Class type T}.
   * @throws ConfigurationException if the configuration property is undeclared or undefined.
   * @see #getPropertyValueAs(String, Class, boolean)
   */
  default <T> T getPropertyValueAs(String propertyName, Class<T> type) {
    return getPropertyValueAs(propertyName, type, REQUIRED);
  }

  /**
   * Gets the value of the configuration property identified by the given {@link String name} as a {@link T value}
   * of the specified {@link Class type T}.
   *
   * The {@code required} parameter is used to indicate whether the configuration property is {@literal required} or not
   * and whether a {@link ConfigurationException} should be thrown when the property is undeclared or undefined.
   *
   * @param <T> {@link Class type} to convert the returned value of the configuration property to.
   * @param propertyName {@link String} containing the {@literal name} of the configuration property.
   * @param type expected {@link Class type} of the configuration property value.
   * @param required boolean value used to indicate whether the configuration property is required
   * to be both declared and defined.
   * @return the value of the configuration property identified by the given {@link String name}
   * as a {@link T value} of the specified {@link Class type T}.
   * @throws ConfigurationException if the configuration property is required and the property
   * is undeclared or undefined.
   * @see org.cp.elements.data.conversion.provider.SimpleTypeConversions
   * @see #getPropertyValue(String, boolean)
   */
  default <T> T getPropertyValueAs(String propertyName, Class<T> type, boolean required) {
    return SimpleTypeConversions.findBy(type).convert(getPropertyValue(propertyName, required));
  }

  /**
   * Gets the value of the configuration property identified by the given {@link String name} as a {@link T value}
   * of the specified {@link Class type T}.
   *
   * By passing an {@link T argument} to the {@code defaultPropertyValue} parameter, this effectively overrides
   * the required parameter to indicate that the configuration property is not required to be declared or defined,
   * and therefore falls back to the {@link T default value}.
   *
   * @param <T> {@link Class type} to convert the returned value of the configuration property to.
   * @param propertyName {@link String} containing the {@literal name} of the configuration property.
   * @param type expected {@link Class type} of the configuration property value.
   * @param defaultPropertyValue {@link T default value} returned for the configuration property
   * when the property is undeclared or undefined.
   * @return the value of the configuration property identified by the given {@link String name}
   * as a {@link T value} of the specified {@link Class type T}, or the {@link T default value}
   * when the property is undeclared or undefined.
   * @see #getPropertyValueAs(String, Class, boolean)
   * @see java.util.function.Supplier
   */
  @SuppressWarnings("unchecked")
  default @Nullable <T> T getPropertyValueAs(String propertyName, Class<T> type, @Nullable T defaultPropertyValue) {

    return ObjectUtils.returnFirstNonNullValue(getPropertyValueAs(propertyName, type, NOT_REQUIRED),
      defaultPropertyValue);
  }

  /**
   * Gets the value of the configuration property identified by the given {@link String name} as a {@link T value}
   * of the specified {@link Class type T}.
   *
   * @param <T> {@link Class type} to convert the returned value of the configuration property to.
   * @param propertyName {@link String} containing the {@literal name} of the configuration property.
   * @param type expected {@link Class type} of the configuration property value.
   * @param defaultPropertyValue {@link Supplier} used to supply the {@link T default value}
   * for the {@link String named} configuration property when undeclared or undefined.
   * @return the value of the configuration property identified by the given {@link String name}
   * as a {@link T value} of {@link Class type T}, or the {@link T default value} supplied by
   * the given {@link Supplier} when the property is undeclared or undefined.
   * @see #getPropertyValueAs(String, Class, boolean)
   * @see java.util.function.Supplier
   */
  default @Nullable <T> T getPropertyValueAs(String propertyName, Class<T> type,
      @Nullable Supplier<T> defaultPropertyValue) {

    return Optional.ofNullable(getPropertyValueAs(propertyName, type, NOT_REQUIRED))
      .orElseGet(defaultPropertyValue);
  }

  /**
   * Builder method used to compose a {@literal Composite} of {@link Configuration} objects.
   *
   * @param configuration {@link Configuration} to compose with this {@link Configuration}.
   * @return a {@literal Composite} {@link Configuration} consisting of this {@link Configuration}
   * and the given {@link Configuration} if not {@literal null}. Returns this {@link Configuration}
   * if the given {@link Configuration} is {@literal null}.
   * @see <a href="https://en.wikipedia.org/wiki/Composite_pattern">Composite Software Design Pattern</a>
   */
  default @NotNull Configuration andThen(@Nullable Configuration configuration) {

    return configuration == null ? this : (propertyName, required) ->
      this.getPropertyValue(propertyName, () -> configuration.getPropertyValue(propertyName, required));
  }

  /**
   * Returns an {@literal Iterator} to iterate over the {@link String property names}
   * declared in this {@link Configuration}.
   *
   * @return an {@link Iterator} iterating over the {@link String property names}
   * declared in this {@link Configuration}.
   * @see java.util.Iterator
   */
  @Override
  default Iterator<String> iterator() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }
}
