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
package org.cp.elements.util;

import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.ServiceLoader;
import java.util.function.Predicate;

import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.data.conversion.ConversionService;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Wrapper for a {@link Properties} object in order to encapsulate functionality for conveniently accessing properties
 * and converting {@link Object values} to a specific {@link Class type}.
 *
 * @author John J. Blum
 * @see java.lang.Iterable
 * @see java.util.Properties
 * @see org.cp.elements.data.conversion.ConversionService
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class PropertiesAdapter implements Iterable<String> {

  /**
   * Factory method used to construct a new instance of {@link PropertiesAdapter} with no {@link Properties}.
   *
   * @return a new {@link PropertiesAdapter} with no {@link Properties}.
   * @see #PropertiesAdapter(Properties)
   * @see java.util.Properties
   */
  @NullSafe
  public static @NotNull PropertiesAdapter empty() {
    return new PropertiesAdapter(new Properties());
  }

  /**
   * Factory method used to construct a new instance of {@link PropertiesAdapter} initialized with the given,
   * required {@link Properties}.
   *
   * @param properties {@link Properties} to adapt; must not be {@literal null}.
   * @return a new {@link PropertiesAdapter} adapting the given, required {@link Properties}.
   * @throws IllegalArgumentException if the {@link Properties} are {@literal null}.
   * @see #PropertiesAdapter(Properties)
   * @see java.util.Properties
   */
  public static @NotNull PropertiesAdapter from(@NotNull Properties properties) {
    return new PropertiesAdapter(properties);
  }

  private final ConversionService conversionService;

  private final Properties delegate;

  /**
   * Constructs a new instance of {@link PropertiesAdapter} initialized with the given, required {@link Properties}.
   *
   * Additionally, this constructor will load the configured {@link ConversionService}.
   *
   * @param properties {@link Properties} to adapt; must not be {@literal null}.
   * @throws IllegalArgumentException if the {@link Properties} are {@literal null}.
   * @see org.cp.elements.data.conversion.ConversionService
   * @see java.util.Properties
   */
  public PropertiesAdapter(@NotNull Properties properties) {

    this.delegate = ObjectUtils.requireObject(properties, "Properties to adapt is required");
    this.conversionService = ServiceLoader.load(ConversionService.class).iterator().next();
  }

  /**
   * Returns a reference to the configured {@link ConversionService} used to convert property values
   * into a value of the requested {@link Class type}.
   *
   * @return a reference to the configured {@link ConversionService} used to convert property values
   * into a value of the requested {@link Class type}.
   * @see org.cp.elements.data.conversion.ConversionService
   */
  protected @NotNull ConversionService getConversionService() {
    return this.conversionService;
  }

  /**
   * Returns the {@link Properties} adapted (wrapped) by this {@link PropertiesAdapter}.
   *
   * @return the {@link Properties} adapted (wrapped) by this {@link PropertiesAdapter};
   * never {@literal null}.
   * @see java.util.Properties
   */
  protected @NotNull Properties getProperties() {
    return this.delegate;
  }

  /**
   * Determines whether this {@link PropertiesAdapter} contains any properties.
   *
   * @return a boolean value indicating whether this {@link PropertiesAdapter} contains any properties.
   * @see java.util.Properties#isEmpty()
   * @see #getProperties()
   */
  public boolean isEmpty() {
    return getProperties().isEmpty();
  }

  /**
   * Determines whether the property identified by {@link String name} is set.
   *
   * @param propertyName {@link String} containing the {@literal name} of the property.
   * @return a boolean value indicating whether the {@link String named} property is set.
   * @see org.cp.elements.lang.StringUtils#hasText(String)
   * @see #get(String)
   */
  public boolean isSet(@NotNull String propertyName) {
    return StringUtils.hasText(get(propertyName));
  }

  /**
   * Determines whether the property identified by {@link String name} is unset.
   *
   * @param propertyName {@link String} containing the {@literal name} of the property.
   * @return a boolean value indicating whether the {@link String named} property exits but is unset.
   * @see #contains(String)
   * @see #isSet(String)
   */
  public boolean isUnset(@NotNull String propertyName) {
    return contains(propertyName) && !isSet(propertyName);
  }

  /**
   * Determines whether the {@link String named} property is present (declared or defined) in
   * this {@link PropertiesAdapter}.
   *
   * @param propertyName {@link String} containing the {@literal name} of the property to check for presence
   * (declaration of definition) in this {@link PropertiesAdapter}.
   * @return a boolean value indicating whether the {@link String named} property is present (declared or defined)
   * in this {@link PropertiesAdapter}.
   * @see java.util.Properties#containsKey(Object)
   * @see #getProperties()
   */
  public boolean contains(@NotNull String propertyName) {
    return getProperties().containsKey(propertyName);
  }

  /**
   * Converts the {@link String value} of the {@link String named} property into an instance of
   * the requested {@link Class type}.
   *
   * @param <T> {@link Class type} of the return value.
   * @param propertyName {@link String} containing the {@literal name} of the property to get.
   * @param type requested {@link Class} type in which to convert the {@link String value}
   * of the {@link String named} property; must not be {@literal null}.
   * @return the {@link String value} of the {@link String named} property converted into
   * an instance of the requested {@link Class type}.
   * @throws ConversionException if the {@link String value} of the {@link String named} property
   * cannot be converted into an instance of the requested {@link Class type}.
   * @throws IllegalArgumentException if the {@link Class type} is {@literal null}.
   * @see org.cp.elements.data.conversion.ConversionService#convert(Object, Class)
   * @see #getConversionService()
   * @see java.lang.Class
   * @see #get(String)
   */
  protected <T> T convert(@NotNull String propertyName, @NotNull Class<T> type) {

    return getConversionService().convert(get(propertyName),
      ObjectUtils.requireObject(type, "Class type to convert the property value to is required"));
  }

  /**
   * Defaults the {@link String value} for the {@link String named} property if the property
   * was not declared or defined (set).
   *
   * @param <T> {@link Class type} of the return value.
   * @param propertyName {@link String} containing the {@literal name} of the property.
   * @param type requested {@link Class type} of the {@link String named} property {@link String value};
   * must not be {@literal null}.
   * @param defaultValue {@link T default value} to return if the {@link String named} property
   * was not declared or defined (set).
   * @return the value of the named property as a instance of the specified {@link Class} type
   * or return the default value if the named property does not exist.
   * @throws IllegalArgumentException if the {@link Class type} is {@literal null}.
   * @see #convert(String, Class)
   * @see java.lang.Class
   * @see #isSet(String)
   */
  protected @Nullable <T> T returnDefaultValueIfNotSet(@NotNull String propertyName, @NotNull Class<T> type,
      @Nullable T defaultValue) {

    return isSet(propertyName) ? convert(propertyName, type) : defaultValue;
  }

  /**
   * Returns {@literal null} for a {@literal "null"} {@link String value} or simply returns the {@link String value}.
   *
   * The {@link String value} is safely {@link String#trim() trimmed} before evaluation.
   *
   * @param value {@link String} to evaluate.
   * @return {@literal null} for a {@literal "null"} {@link String value} or return the {@link String value}
   * if not {@literal null} or {@literal "null"}.
   * @see java.lang.String
   */
  protected @Nullable String valueOf(@Nullable String value) {
    return "null".equalsIgnoreCase(String.valueOf(value).trim()) ? null : value;
  }

  /**
   * {@link Predicate Filters} properties from this {@link PropertiesAdapter} by {@link String name}.
   *
   * @param filter {@link Predicate} used to filter properties from this {@link PropertiesAdapter};
   * must not be {@literal null}.
   * @return a new instance of {@link PropertiesAdapter} containing only filtered properties.
   * @throws IllegalArgumentException if the {@link Predicate filter} is {@literal null}.
   * @see java.util.function.Predicate
   * @see org.cp.elements.lang.Filter
   * @see java.util.Properties
   * @see #from(Properties)
   */
  public @NotNull PropertiesAdapter filter(@NotNull Predicate<String> filter) {

    Assert.notNull(filter, "Predicate used to filter properties is required");

    Properties properties = new Properties();

    for (String propertyName : this) {
      if (filter.test(propertyName)) {
        properties.setProperty(propertyName, get(propertyName));
      }
    }

    return from(properties);
  }

  /**
   * Gets the assigned {@link String value} of {@link String named} property.
   *
   * @param propertyName {@link String} containing the {@literal name} of the property.
   * @return the assigned {@link String value} of {@link String named} property.
   * @see #get(String, String)
   */
  public @Nullable String get(@NotNull String propertyName) {
    return get(propertyName, null);
  }

  /**
   * Gets the assigned {@link String value} of the {@link String named} property or returns
   * the {@link String default value} if the {@link String named} property does not exist.
   *
   * @param propertyName {@link String} containing the {@literal name} of the property.
   * @param defaultValue {@link String default value} to return if the {@link String named} property does not exist.
   * @return the assigned {@link String value} of the {@link String named} property or returns
   * the {@link String default value} if the {@link String named} property does not exist.
   * @see java.util.Properties#getProperty(String, String)
   * @see #getProperties()
   */
  public @Nullable String get(@NotNull String propertyName, @Nullable String defaultValue) {
    return valueOf(getProperties().getProperty(propertyName, defaultValue));
  }

  /**
   * Gets the assigned {@link String value} of the {@link String named} property as an instance of
   * the requested {@link Class type}.
   *
   * @param <T> {@link Class type} of the return value.
   * @param propertyName {@link String} containing the {@literal name} of the property.
   * @param type requested {@link Class type} of the {@link String value} to return for the property.
   * @return the assigned {@link String value} of the {@link String named} property as an instance of
   * the requested {@link Class type}.
   * @throws IllegalArgumentException if the {@link Class type} is {@literal null}.
   * @see #getAsType(String, Class, Object)
   */
  public @Nullable <T> T getAsType(@NotNull String propertyName, @NotNull Class<T> type) {
    return getAsType(propertyName, type, null);
  }

  /**
   * Gets the assigned {@link String value} of the {@link String named} property as an instance of
   * the requested {@link Class} type or the {@link T default value} if the {@link String named} property
   * was not declared or is undefined (not set).
   *
   * @param <T> {@link Class type} of the return value.
   * @param propertyName {@link String} containing the {@literal name} of the property.
   * @param type requested {@link Class type} of the {@link String value} to return for the property.
   * @param defaultValue {@link T default value} to return if the {@link String named} property
   * was not declared or is undefined (not set).
   * @return the assigned {@link String value} of the {@link String named} property as an instance of
   * the requested {@link Class} type or the {@link T default value} if the {@link String named} property
   * was not declared or is undefined (not set).
   * @throws IllegalArgumentException if the {@link Class type} is {@literal null}.
   * @see #returnDefaultValueIfNotSet(String, Class, Object)
   */
  public @Nullable <T> T getAsType(@NotNull String propertyName, @NotNull Class<T> type, @Nullable T defaultValue) {
    return returnDefaultValueIfNotSet(propertyName, type, defaultValue);
  }

  /**
   * Iterates over property {@link String names} in this {@link PropertiesAdapter}.
   *
   * @return an unmodifiable {@link Iterator} to iterate over the {@link String names} of the properties
   * in this {@link PropertiesAdapter}.
   * @see java.util.Properties#stringPropertyNames()
   * @see java.util.Iterator
   * @see #getProperties()
   */
  @Override
  public @NotNull Iterator<String> iterator() {
    return Collections.unmodifiableSet(getProperties().stringPropertyNames()).iterator();
  }

  /**
   * Returns the {@link Integer number of properties} defined in this {@link PropertiesAdapter}.
   *
   * @return the {@link Integer number of properties} defined in this {@link PropertiesAdapter}.
   * @see java.util.Properties#size()
   * @see #getProperties()
   */
  public int size() {
    return getProperties().size();
  }

  /**
   * Determines whether this {@link PropertiesAdapter} is equal to the given {@link Object}.
   *
   * @param obj {@link Object} evaluated for equality with this {@link PropertiesAdapter}.
   * @return a boolean value indicating whether this {@link PropertiesAdapter} is equal to the given {@link Object}.
   * @see java.lang.Object#equals(Object)
   */
  @Override
  public boolean equals(Object obj) {

    if (this == obj) {
      return true;
    }

    if (!(obj instanceof PropertiesAdapter)) {
      return false;
    }

    PropertiesAdapter that = (PropertiesAdapter) obj;

    return this.getProperties().equals(that.getProperties());
  }

  /**
   * Computes the hash code of this {@link PropertiesAdapter}.
   *
   * @return an integer containing the hash code of this {@link PropertiesAdapter}.
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode() {
    return ObjectUtils.hashCodeOf(getProperties());
  }

  /**
   * Return a {@link String} representation of this {@link PropertiesAdapter}.
   *
   * @return a {@link String} describing this {@link PropertiesAdapter}.
   * @see org.cp.elements.util.MapUtils#toString(Map)
   * @see #toMap()
   */
  @Override
  public @NotNull String toString() {
    return MapUtils.toString(toMap());
  }

  /**
   * Converts this {@link PropertiesAdapter} into a {@link Properties} object.
   *
   * @return a {@link Properties} object from this {@link PropertiesAdapter}.
   * @see java.util.Properties
   */
  public @NotNull Properties toProperties() {

    Properties properties = new Properties();

    for (String propertyName : this) {
      properties.setProperty(propertyName, get(propertyName));
    }

    return properties;
  }

  /**
   * Converts this {@link PropertiesAdapter} into a {@link Map}.
   *
   * @return a {@link Map} containing the properties and values of this {@link PropertiesAdapter}.
   * @see java.util.Map
   * @see #iterator()
   * @see #get(String)
   */
  public @NotNull Map<String, String> toMap() {

    Map<String, String> map = new HashMap<>(size());

    for (String propertyName : this) {
      map.put(propertyName, get(propertyName));
    }

    return map;
  }
}
