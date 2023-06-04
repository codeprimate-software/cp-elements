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

import static org.cp.elements.lang.ElementsExceptionsFactory.newSystemException;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.util.Map;
import java.util.Objects;
import java.util.Properties;

import org.cp.elements.io.NoSuchFileException;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Builder;
import org.cp.elements.lang.NullSafeOperations;
import org.cp.elements.lang.annotation.Dsl;
import org.cp.elements.lang.annotation.FluentApi;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;

/**
 * {@link PropertiesBuilder} is a {@link Builder} object for constructing a new instance of Java {@link Properties}.
 *
 * @author John J. Blum
 * @see java.util.Properties
 * @see org.cp.elements.lang.Builder
 * @see org.cp.elements.lang.annotation.Dsl
 * @see org.cp.elements.lang.annotation.FluentApi
 * @see org.cp.elements.util.PropertiesAdapter
 * @see <a href="https://en.wikipedia.org/wiki/Builder_pattern">Builder Software Design Pattern</a>
 * @since 1.0.0
 */
@FluentApi
@SuppressWarnings("unused")
public class PropertiesBuilder implements Builder<Properties> {

  /**
   * Factory method used to construct a new instance of {@link PropertiesBuilder} initialized with {@link Properties}
   * loaded from the given, required {@link File}.
   *
   * @param properties {@link File} containing properties to load; must not be {@literal null}.
   * @return a new {@link PropertiesBuilder} initialized with {@link Properties} loaded from
   * the given, required {@link File}.
   * @throws NoSuchFileException if the {@link File} cannot be found.
   * @throws SystemException if properties from the given, required {@link File} cannot be loaded.
   * @see org.cp.elements.lang.annotation.Dsl
   * @see #from(InputStream)
   * @see java.io.File
   */
  @Dsl
  public static @NotNull PropertiesBuilder from(@NotNull File properties) {

    Assert.notNull(properties, "Properties file is required");

    try (FileInputStream in = new FileInputStream(properties)) {
      return from(in);
    }
    catch (FileNotFoundException cause) {
      throw new NoSuchFileException(String.format("[%1$s] not found", properties), cause);
    }
    catch (IOException cause) {
      throw newSystemException(cause, "Failed to load properties from file [%s]", properties);
    }
  }

  /**
   * Factory method used to construct a new instance of {@link PropertiesBuilder} initialized with {@link Properties}
   * loaded from the given, required {@link InputStream}.
   *
   * @param inputStream input source containing {@link Properties} to load; must not be {@literal null}.
   * @return a new {@link PropertiesBuilder} initialized with properties loaded from the given,
   * required {@link InputStream}.
   * @throws IllegalArgumentException if the {@link InputStream} is {@literal null}.
   * @throws SystemException if properties from the given {@link InputStream} could not be loaded.
   * @see java.util.Properties#load(InputStream)
   * @see org.cp.elements.lang.annotation.Dsl
   * @see java.io.InputStream
   * @see #from(Properties)
   */
  @Dsl
  public static @NotNull PropertiesBuilder from(@NotNull InputStream inputStream) {

    Assert.notNull(inputStream, "InputStream is required");

    try {
      Properties defaults = new Properties();
      defaults.load(inputStream);
      return from(defaults);
    }
    catch (IOException cause) {
      throw newSystemException(cause, "Failed to load properties from input stream [%s]", inputStream);
    }
  }

  /**
   * Factory method to initialize a new instance of the {@link PropertiesBuilder} with the key/values
   * from the given {@link Map}.
   *
   * @param map the {@link Map} used to initialize the new {@link PropertiesBuilder}.
   * @return a new {@link PropertiesBuilder} initialized with the key/values from the given {@link Map}.
   * @throws IllegalArgumentException if the {@link Map} is {@literal null} or the given {@link Map}
   * contains a singl {@literal nul} key.
   * @see java.util.Properties
   * @see java.util.Map
   * @see #from(Properties)
   */
  public static @NotNull PropertiesBuilder from(@NotNull Map<String, ?> map) {

    Assert.notNull(map, "Map is required");

    long nullKeyCount = map.keySet().stream()
      .filter(Objects::isNull)
      .count();

    Assert.isTrue(nullKeyCount == 0, "Map must not contain null keys");

    Properties properties = new Properties();

    for (Map.Entry<String, ?> entry : map.entrySet()) {
      properties.setProperty(entry.getKey(), String.valueOf(entry.getValue()));
    }

    return from(properties);
  }

  /**
   * Factory method used to construct a new instance of {@link PropertiesBuilder} initialized with
   * the given {@link Properties} used as {@literal defaults}.
   *
   * @param defaults {@link Properties} used to initialize the {@link PropertiesBuilder} as {@literal defaults}.
   * @return a new {@link PropertiesBuilder} initialized with the given {@link Properties} as {@literal defaults}.
   * @see org.cp.elements.lang.annotation.Dsl
   * @see #PropertiesBuilder(Properties)
   * @see java.util.Properties
   */
  @Dsl
  @NullSafe
  public static @NotNull PropertiesBuilder from(@Nullable Properties defaults) {
    return new PropertiesBuilder(defaults);
  }

  /**
   * Factory method used to construct a new instance of {@link PropertiesBuilder} initialized with {@link Properties}
   * loaded from the given, required {@link Reader}.
   *
   * @param reader {@link Reader} containing the {@link Properties} to load; must not be {@literal null}.
   * @return a new {@link PropertiesBuilder} initialized with {@link Properties} loaded from the given,
   * required {@link Reader}.
   * @throws IllegalArgumentException if the {@link Reader} is {@literal null}.
   * @throws SystemException if properties from the given {@link Reader} could not be loaded.
   * @see org.cp.elements.lang.annotation.Dsl
   * @see java.util.Properties#load(Reader)
   * @see #from(Properties)
   * @see java.io.Reader
   */
  @Dsl
  public static @NotNull PropertiesBuilder from(@NotNull Reader reader) {

    Assert.notNull(reader, "Reader is required");

    try {
      Properties defaults = new Properties();
      defaults.load(reader);
      return from(defaults);
    }
    catch (IOException cause) {
      throw newSystemException(cause, "Failed to load properties from reader [%s]", reader);
    }
  }

  /**
   * Factory method used to construct a new instance of {@link PropertiesBuilder} initialized from
   * the given {@literal associative array}, of the form:
   * {@literal [ "keyOne=valueOne", "keyTwo=valueTwo", ..., "keyN=valueN" ]}.
   *
   * @param associativeArray {@link String array} of {@literal String key/value mappings}.
   * @return a new {@link PropertiesBuilder}.
   * @see #from(Map)
   */
  public static @NotNull PropertiesBuilder fromAssociativeArray(String[] associativeArray) {
    return from(MapUtils.fromAssociativeArray(associativeArray));
  }

  /**
   * Factory method used to construct a new instance of {@link PropertiesBuilder} initialized with
   * {@link System#getenv() System environment variables}.
   *
   * @return a new {@link PropertiesBuilder} initialized with {@link System#getenv() System environment variables}.
   * @see org.cp.elements.lang.annotation.Dsl
   * @see java.lang.System#getenv()
   * @see #fromSystemProperties()
   * @see #from(Properties)
   */
  @Dsl
  public static @NotNull PropertiesBuilder fromEnvironmentVariables() {
    return from(System.getenv());
  }

  /**
   * Factory method used to construct a new instance of {@link PropertiesBuilder} initialized from
   * Java {@link System#getProperties() System properties}.
   *
   * @return a new {@link PropertiesBuilder} initialized with Java {@link System#getProperties() System properties}.
   * @see org.cp.elements.lang.annotation.Dsl
   * @see java.lang.System#getProperties()
   * @see #fromEnvironmentVariables()
   * @see #from(Properties)
   */
  @Dsl
  public static @NotNull PropertiesBuilder fromSystemProperties() {
    return from(System.getProperties());
  }

  /**
   * Factory method used to construct a new instance of {@link PropertiesBuilder} initialized with {@link Properties}
   * loaded from the given, required {@literal XML} {@link InputStream}.
   *
   * @param inputStream {@literal XML} input source containing {@link Properties} to load; must not be {@literal null}.
   * @return a new {@link PropertiesBuilder} initialized with properties loaded from the given, required {@literal XML}
   * input source.
   * @throws IllegalArgumentException if the {@link InputStream} is {@literal null}.
   * @throws SystemException if properties from the given, required {@literal XML} {@link InputStream}
   * could not be loaded.
   * @see java.util.Properties#loadFromXML(InputStream)
   * @see org.cp.elements.lang.annotation.Dsl
   * @see java.io.InputStream
   * @see #from(Properties)
   */
  @Dsl
  public static @NotNull PropertiesBuilder fromXml(@NotNull InputStream inputStream) {

    Assert.notNull(inputStream, "InputStream is required");

    try {
      Properties defaults = new Properties();
      defaults.loadFromXML(inputStream);
      return from(defaults);
    }
    catch (IOException cause) {
      throw newSystemException(cause, "Failed to load properties from input stream [%s]", inputStream);
    }
  }

  /**
   * Factory method used to construct a new instance of {@link PropertiesBuilder} with no {@link Properties}.
   *
   * This factory method serves the same purpose as the {@code new} operator, but can be used in
   * a {@link Dsl DSL} style.
   *
   * @return a new {@link PropertiesBuilder} instance.
   * @see org.cp.elements.lang.annotation.Dsl
   * @see #PropertiesBuilder()
   */
  @Dsl
  public static @NotNull PropertiesBuilder newInstance() {
    return new PropertiesBuilder();
  }

  private final Properties properties;

  /**
   * Constructs a new {@link PropertiesBuilder} initialized with an empty set of {@link Properties}.
   *
   * @see java.util.Properties
   */
  public PropertiesBuilder() {
    this.properties = new Properties();
  }

  /**
   * Constructs a new {@link PropertiesBuilder} initialized with the given set of {@link Properties}
   * used as {@literal defaults}.
   *
   * @param defaults {@link Properties} used as {@literal defaults}; may be {@literal null}.
   * @see java.util.Properties
   */
  public PropertiesBuilder(@Nullable Properties defaults) {
    this.properties = new Properties();
    NullSafeOperations.ifNotNullDo(defaults, this.properties::putAll);
  }

  /**
   * Returns the raw {@link Properties} object being constructed by this {@link Builder}.
   *
   * @return a reference to the raw {@link Properties} object under construction.
   * @see java.util.Properties
   */
  protected @NotNull Properties getProperties() {
    return this.properties;
  }

  /**
   * Sets the {@link String named} property to the given {@link Object value}.
   *
   * The {@link String} of the given {@link Object value} is computed with {@link String#valueOf(Object)}.
   *
   * @param propertyName {@link String} containing the {@literal name} of the property to set;
   * must not be {@literal null}.
   * @param propertyValue {@link Object} containing the {@literal value} to assign to the property.
   * @return this {@link Builder}.
   * @see org.cp.elements.lang.annotation.Dsl
   * @see #set(String, String)
   */
  @Dsl
  public @NotNull PropertiesBuilder set(@NotNull String propertyName, @Nullable Object propertyValue) {
    return set(propertyName, String.valueOf(propertyValue));
  }

  /**
   * Sets the {@link String named} property to the given, required {@link String value}.
   *
   * @param propertyName {@link String} containing the {@literal name} of the property to set;
   * must not be {@literal null}.
   * @param propertyValue {@link String} containing the {@literal value} to assign to the property;
   * must not be {@literal null}.
   * @return this {@link Builder}.
   * @see java.util.Properties#setProperty(String, String)
   * @see org.cp.elements.lang.annotation.Dsl
   * @see #getProperties()
   */
  @Dsl
  public @NotNull PropertiesBuilder set(@NotNull String propertyName, @NotNull String propertyValue) {
    getProperties().setProperty(propertyName, propertyValue);
    return this;
  }

  /**
   * Returns the constructed {@link Properties} object.
   *
   * @return the constructed {@link Properties} object; never {@literal null}.
   * @see java.util.Properties
   * @see #getProperties()
   */
  public @NotNull Properties build() {
    return getProperties();
  }

  /**
   * Constructs a new {@link PropertiesAdapter} from this {@link PropertiesBuilder}.
   *
   * @return a {@link PropertiesAdapter} from this {@link PropertiesBuilder}.
   * @see org.cp.elements.util.PropertiesAdapter
   * @see org.cp.elements.lang.annotation.Dsl
   * @see #build()
   */
  @Dsl
  public @NotNull PropertiesAdapter buildPropertiesAdapter() {
    return PropertiesAdapter.from(build());
  }

  /**
   * Returns a {@link String} representation of the constructed {@link Properties}.
   *
   * @return a {@link String} describing the constructed {@link Properties}.
   * @see java.util.Properties#toString()
   * @see java.lang.String
   * @see #getProperties()
   */
  @Override
  public String toString() {
    return getProperties().toString();
  }
}
