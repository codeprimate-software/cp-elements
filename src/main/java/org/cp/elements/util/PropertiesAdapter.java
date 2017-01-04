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

package org.cp.elements.util;

import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Filter;
import org.cp.elements.lang.NullSafe;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.util.convert.ConversionService;
import org.cp.elements.util.convert.provider.DefaultConversionService;

/**
 * The {@link PropertiesAdapter} class is a wrapper around a {@link Properties} object to encapsulate functionality
 * for conveniently accessing properties and converting value to a specific type.
 *
 * @author John J. Blum
 * @see java.lang.Iterable
 * @see java.util.Properties
 * @see org.cp.elements.util.convert.ConversionService
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class PropertiesAdapter implements Iterable<String> {

  private final ConversionService conversionService;

  private final Properties delegate;

  /**
   * Factory method to get an instance of the PropertiesAdapter class initialized with the given {@link Properties}.
   *
   * @param properties the {@link Properties} to wrap.
   * @return an instance of the {@link PropertiesAdapter} initialized with the given {@link Properties}.
   */
  public static PropertiesAdapter from(Properties properties) {
    return new PropertiesAdapter(properties);
  }

  /**
   * Constructs an instance of the PropertiesAdapter class initialized with the given {@link Properties}.
   *
   * @param properties the {@link Properties} to wrap with this wrapper.
   */
  public PropertiesAdapter(Properties properties) {
    Assert.notNull(properties, "The Properties to wrap cannot be null");
    this.delegate = properties;
    this.conversionService = new DefaultConversionService();
  }

  /**
   * Returns a reference to the {@link ConversionService} used to convert the property value
   * into a value of the desired class type.
   *
   * @return a reference to the {@link ConversionService} used ot convert the property value
   * into a value of the desired class type.
   * @see org.cp.elements.util.convert.ConversionService
   */
  protected ConversionService getConversionService() {
    return this.conversionService;
  }

  /**
   * Returns the {@link Properties} wrapped by this Adapter.
   *
   * @return the {@link Properties} wrapped by this Adapter.
   * @see java.util.Properties
   */
  protected Properties getProperties() {
    return this.delegate;
  }

  /**
   * Determines whether the named property is present in this {@link Properties} adapter.
   *
   * @param propertyName the name of the property to check for presence in this adapter.
   * @return a boolean value indicating whether the named property is present in this set of {@link Properties}.
   * @see java.util.Properties#containsKey(Object)
   * @see #getProperties()
   */
  public boolean contains(String propertyName) {
    return getProperties().containsKey(propertyName);
  }

  /**
   * Determines whether this {@link Properties} object contains any properties.
   *
   * @return a boolean value indicating whether this {@link Properties} object contains any properties.
   * @see java.util.Properties#isEmpty()
   * @see #getProperties()
   */
  public boolean isEmpty() {
    return getProperties().isEmpty();
  }

  /**
   * Determines whether the property identified by name is set.
   *
   * @param propertyName the name of the property.
   * @return a boolean value indicating whether the named property is set.
   * @see org.cp.elements.lang.StringUtils#hasText(String)
   * @see #get(String)
   */
  public boolean isSet(String propertyName) {
    return StringUtils.hasText(get(propertyName));
  }

  /**
   * Determines whether the property identified by name is unset.
   *
   * @param propertyName the name of the property.
   * @return a boolean value indicating whether the named property exits but is unset.
   * @see #contains(String)
   * @see #isSet(String)
   */
  public boolean isUnset(String propertyName) {
    return (contains(propertyName) && !isSet(propertyName));
  }

  /**
   * Converts the value of the named property into an instance of the given {@link Class} type.
   *
   * @param <T> Class type of the return value.
   * @param propertyName the name of the property to get.
   * @param type the desired {@link Class} type to convert the value of the named property to.
   * @return the value of the named property converted to an instance of the given {@link Class} type.
   * @see org.cp.elements.util.convert.ConversionService#convert(Object, Class)
   * @see java.lang.Class
   * @see #getConversionService()
   * @see #get(String) \
   */
  protected <T> T convert(String propertyName, Class<T> type) {
    return getConversionService().convert(get(propertyName), type);
  }

  /**
   * Defaults of the value for the named property if the property does not exist.
   *
   * @param <T> {@link Class} type of the return value.
   * @param propertyName the name of the property to get.
   * @param defaultValue the default value to return if the named property does not exist.
   * @param type the desired {@link Class} type of the named property value.
   * @return the value of the named property as a instance of the specified {@link Class} type
   * or return the default value if the named property does not exist.
   * @see java.lang.Class
   * @see #isSet(String)
   * @see #convert(String, Class)
   */
  protected <T> T defaultIfNotSet(String propertyName, T defaultValue, Class<T> type) {
    return (isSet(propertyName) ? convert(propertyName, type) : defaultValue);
  }

  /**
   * Returns {@literal null} for a {@literal "null"} {@link String} value or simply returns the {@link String} value.
   * The {@link String} value is safely trimmed before evaluation.
   *
   * @param value {@link String} value to evaluate.
   * @return a {@literal null} for a {@literal "null"} {@link String} value.
   * @see java.lang.String
   */
  @NullSafe
  protected String valueOf(String value) {
    return ("null".equalsIgnoreCase(String.valueOf(value).trim()) ? null : value);
  }

  /**
   * Filters the properties from this adapter by name.
   *
   * @param filter the {@link Filter} used to filter the properties of this adapter.
   * @return a newly constructed instance of the {@link PropertiesAdapter} containing only the filtered properties.
   * @see org.cp.elements.lang.Filter
   * @see java.util.Properties
   * @see #from(Properties)
   */
  public PropertiesAdapter filter(Filter<String> filter) {
    Properties properties = new Properties();

    for (String propertyName : this) {
      if (filter.accept(propertyName)) {
        properties.setProperty(propertyName, get(propertyName));
      }
    }

    return from(properties);
  }

  /**
   * Gets the assigned value of named property as a {@link String}.
   *
   * @param propertyName the name of the property to get.
   * @return the assigned value of the named property as a {@link String}.
   * @see #get(String, String)
   */
  public String get(String propertyName) {
    return get(propertyName, null);
  }

  /**
   * Gets the assigned value of the named property as a {@link String} or returns the default value
   * if the named property does not exist.
   *
   * @param propertyName the name of the property to get.
   * @param defaultValue the default value to return if the named property does not exist.
   * @return the assigned value of the named property as a {@link String} or default value
   * if the named property does not exist.
   * @see java.util.Properties#getProperty(String)
   * @see #getProperties()
   */
  public String get(String propertyName, String defaultValue) {
    return valueOf(getProperties().getProperty(propertyName, defaultValue));
  }

  /**
   * Gets the assigned value of the named property as an instance of the specified {@link Class} type.
   *
   * @param <T> {@link Class} type of the return value.
   * @param propertyName the name of the property to get.
   * @param type Class type of the value to return for the specified property.
   * @return the assigned value of the named property as an instance of the specified {@link Class} type.
   * @see #getAsType(String, Class, Object)
   */
  public <T> T getAsType(String propertyName, Class<T> type) {
    return getAsType(propertyName, type, null);
  }

  /**
   * Gets the assigned value of the named property as an instance of the specified {@link Class} type
   * or the default value if the named property does not exist.
   *
   * @param <T> {@link Class} type of the return value.
   * @param propertyName the name of the property to get.
   * @param type Class type of the value to return for the specified property.
   * @param defaultValue the default value to return if the named property does not exist.
   * @return the assigned value of the named property as an instance of the specified {@link Class} type
   * or the default value if the named property does not exist.
   * @see #defaultIfNotSet(String, Object, Class)
   * @see java.lang.Class
   */
  public <T> T getAsType(String propertyName, Class<T> type, T defaultValue) {
    return defaultIfNotSet(propertyName, defaultValue, type);
  }

  /**
   * Iterates the property names in this {@link Properties} object.
   *
   * @return an {@link Iterator} to iterate the name of the properties in this {@link Properties} object.
   * @see java.util.Properties#stringPropertyNames()
   * @see java.util.Iterator
   * @see #getProperties()
   */
  @Override
  public Iterator<String> iterator() {
    return Collections.unmodifiableSet(getProperties().stringPropertyNames()).iterator();
  }

  /**
   * Returns the number of properties in this {@link Properties} object.
   *
   * @return an integer value indicating the number of properties in this {@link Properties} object.
   * @see java.util.Properties#size()
   * @see #getProperties()
   */
  public int size() {
    return getProperties().size();
  }

  /**
   * @inheritDoc
   */
  @Override
  public boolean equals(Object obj) {
    if (obj == this) {
      return true;
    }

    if (!(obj instanceof PropertiesAdapter)) {
      return false;
    }

    PropertiesAdapter that = (PropertiesAdapter) obj;

    return this.getProperties().equals(that.getProperties());
  }

  /**
   * @inheritDoc
   */
  @Override
  public int hashCode() {
    int hashValue = 17;
    hashValue = 37 * hashValue + getProperties().hashCode();
    return hashValue;
  }

  /**
   * @inheritDoc
   */
  @Override
  public String toString() {
    return MapUtils.toString(toMap());
  }

  /**
   * Converts this {@link PropertiesAdapter} into a {@link Map}.
   *
   * @return a {@link Map} containing the properties and values of this {@link PropertiesAdapter}.
   * @see java.util.Map
   * @see #iterator()
   * @see #get(String)
   */
  public Map<String, String> toMap() {
    Map<String, String> map = new HashMap<>(size());

    for (String propertyName : this) {
      map.put(propertyName, get(propertyName));
    }

    return map;
  }
}
