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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.Map;
import java.util.Properties;

import org.cp.elements.io.NoSuchFileException;

/**
 * The PropertiesBuilder class is a Builder object for constructing an instance of the {@link Properties} class.
 *
 * @author John J. Blum
 * @see java.util.Properties
 * @see org.cp.elements.util.PropertiesAdapter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class PropertiesBuilder {

  private final Properties properties;

  /**
   * Factory method to load the {@link Properties} contained in the specified {@link File}.
   *
   * @param properties the properties {@link File} to load.
   * @return an instance of the {@link PropertiesBuilder} class initialized with the properties
   * contained in the given {@link File}.
   * @throws NoSuchFileException if the specified {@link File} cannot be found.
   * @throws SystemException if the properties from the specified {@link File} cannot be loaded.
   * @see java.io.File
   * @see #from(InputStream)
   */
  public static PropertiesBuilder from(File properties) {
    try {
      return from(new FileInputStream(properties));
    }
    catch (FileNotFoundException e) {
      throw new NoSuchFileException(String.format("[%1$s] not found", properties), e);
    }
  }

  /**
   * Factory method to load {@link Properties} from the given {@link InputStream}.
   *
   * @param inputStream a input source containing the {@link Properties} to load.
   * @return an instance of the {@link PropertiesBuilder} class initialized with the properties
   * from the given input source.
   * @throws SystemException if the properties from the given {@link InputStream} could not be loaded.
   * @see java.util.Properties#load(InputStream)
   * @see java.io.InputStream
   * @see #from(Properties)
   */
  public static PropertiesBuilder from(InputStream inputStream) {
    try {
      Properties defaults = new Properties();
      defaults.load(inputStream);
      return from(defaults);
    }
    catch (IOException e) {
      throw new SystemException(String.format("failed to load properties from input stream [%1$s]", inputStream), e);
    }
  }

  /**
   * Factory method to initialize a new instance of the {@link PropertiesBuilder} with the key/values
   * from the given {@link Map}.
   *
   * @param map the {@link Map} used to initialize the new {@link PropertiesBuilder}.
   * @return a new {@link PropertiesBuilder} initialized with the key/values from the given {@link Map}.
   * @see java.util.Properties
   * @see java.util.Map
   * @see #from(Properties)
   */
  public static PropertiesBuilder from(Map<String, Object> map) {
    Properties properties = new Properties();

    for (Map.Entry<String, Object> entry : map.entrySet()) {
      properties.setProperty(entry.getKey(), String.valueOf(entry.getValue()));
    }

    return from(properties);
  }

  /**
   * Factory method to construct a new instance of the {@link PropertiesBuilder} class
   * initialized with the specified {@link Properties}.
   *
   * @param properties the {@link Properties} used to initialize the {@link PropertiesBuilder}.
   * @return a newly constructed {@link PropertiesBuilder} initialized with the specified {@link Properties}.
   * @see java.util.Properties
   * @see #PropertiesBuilder(Properties)
   */
  public static PropertiesBuilder from(Properties properties) {
    return new PropertiesBuilder(properties);
  }

  /**
   * Factory method to construct a new isntance of the {@link PropertiesBuilder} class initialized
   * from the System properties.
   *
   * @return a newly constructed {@link PropertiesBuilder} initialized with the System properties.
   * @see java.lang.System#getProperties()
   * @see #from(Properties)
   */
  public static PropertiesBuilder fromSystemProperties() {
    return from(System.getProperties());
  }

  /**
   * Factory method used to construct a new, empty {@link PropertiesBuilder} instance.  This factory method serves
   * the same purpose as the {@code new} operator
   *
   * @return an instance of the {@link PropertiesBuilder} class.
   * @see #PropertiesBuilder()
   */
  public static PropertiesBuilder newInstance() {
    return new PropertiesBuilder();
  }

  /**
   * Constructs an instance of the PropertiesBuilder class with an empty set of {@link Properties}.
   */
  public PropertiesBuilder() {
    properties = new Properties();
  }

  /**
   * Constructs an instance of the PropertiesBuilder class initialized with the given set of default {@link Properties}.
   *
   * @param defaults the {@link Properties} used as defaults.
   * @see java.util.Properties
   */
  public PropertiesBuilder(Properties defaults) {
    properties = new Properties();
    properties.putAll(defaults);
  }

  /**
   * Returns the raw {@link Properties} object being constructed by this builder.
   *
   * @return a reference to the {@link Properties} object under construction.
   * @see java.util.Properties
   */
  protected Properties getProperties() {
    return properties;
  }

  /**
   * Sets the named property to the given value.
   *
   * @param propertyName the name of the property to set.
   * @param propertyValue the value to assign to the property.
   * @return a reference to this builder.
   * @see #set(String, String)
   */
  public PropertiesBuilder set(String propertyName, Object propertyValue) {
    return set(propertyName, String.valueOf(propertyValue));
  }

  /**
   * Sets the named property to the given value.
   *
   * @param propertyName the name of the property to set.
   * @param propertyValue the value to assign to the property.
   * @return a reference to this builder.
   * @see java.util.Properties#setProperty(String, String)
   * @see #getProperties()
   */
  public PropertiesBuilder set(String propertyName, String propertyValue) {
    getProperties().setProperty(propertyName, propertyValue);
    return this;
  }

  /**
   * Returns the constructed {@link Properties} object.
   *
   * @return the constructed {@link Properties} object.
   * @see java.util.Properties
   * @see #getProperties()
   */
  public Properties build() {
    return getProperties();
  }

  /**
   * Constructs a {@link PropertiesAdapter} from this {@link PropertiesBuilder}.
   *
   * @return a {@link PropertiesAdapter} from this {@link PropertiesBuilder}.
   * @see org.cp.elements.util.PropertiesAdapter
   * @see #build()
   */
  public PropertiesAdapter toPropertiesAdapter() {
    return PropertiesAdapter.from(build());
  }

  /**
   * Returns a String description of the constructed {@link Properties}.
   *
   * @return a String describing the constructed {@link Properties}.
   * @see java.util.Properties#toString()
   * @see #getProperties()
   */
  @Override
  public String toString() {
    return getProperties().toString();
  }

}
