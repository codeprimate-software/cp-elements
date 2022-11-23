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

import java.util.Properties;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.FluentApiExtension;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.Dsl;
import org.cp.elements.lang.annotation.FluentApi;
import org.cp.elements.lang.annotation.NotNull;

/**
 * Abstract class used to set properties on a {@link Properties} object.
 *
 * @author John J. Blum
 * @see java.util.Properties
 * @see org.cp.elements.lang.FluentApiExtension
 * @see org.cp.elements.lang.annotation.FluentApi
 * @since 1.0.0
 */
@FluentApi
@SuppressWarnings("unused")
public abstract class PropertiesSetter implements FluentApiExtension {

  /**
   * Factory method used to construct a new instance of {@link PropertiesSetter} initialized with the given,
   * required {@link String property name}.
   *
   * @param propertyName {@link String} containing the {@literal name} of the property to set;
   * must not be {@literal null} or {@literal empty}.
   * @return a new {@link PropertiesSetter} initialized with the given, required {@link String property name}.
   * @throws IllegalArgumentException if the {@link String property name} was not specified.
   * @see org.cp.elements.util.PropertiesSetter
   * @see org.cp.elements.lang.annotation.Dsl
   */
  @Dsl
  public static @NotNull PropertiesSetter set(@NotNull String propertyName) {
    return new PropertiesSetter(propertyName) { };
  }

  private Properties properties;

  private final String propertyName;

  /**
   * Constructs a new instance of {@link PropertiesSetter} initialized with the given,
   * required {@link String property name}.
   *
   * @param propertyName {@link String} containing the {@literal name} of the property to set;
   * must not be {@literal null} or {@literal empty}.
   * @throws IllegalArgumentException if the {@link String property name} was not specified.
   */
  protected PropertiesSetter(@NotNull String propertyName) {
    this.propertyName = StringUtils.requireText(propertyName, "Property name [%s] must be specified");
  }

  /**
   * Returns a reference to the {@link Properties} object on which the property will be set.
   *
   * @return a reference to the {@link Properties} object on which the property will be set.
   * @throws IllegalStateException if the {@link Properties} object is {@literal null}.
   * @see java.util.Properties
   */
  protected @NotNull Properties getProperties() {
    return ObjectUtils.requireState(this.properties, "Properties were not initialized");
  }

  /**
   * Returns the {@link String name} of the property to set.
   *
   * @return the {@link String name} of the property to set.
   */
  protected @NotNull String getPropertyName() {
    return this.propertyName;
  }

  /**
   * Sets a reference to the {@link Properties} object on which the property will be set.
   *
   * @param properties {@link Properties} object on which the property will be set.
   * @return this {@link PropertiesSetter}.
   * @throws IllegalArgumentException if the {@link Properties} object is {@literal null}.
   * @see org.cp.elements.lang.annotation.Dsl
   * @see java.util.Properties
   */
  @Dsl
  public @NotNull PropertiesSetter of(@NotNull Properties properties) {
    this.properties = ObjectUtils.requireObject(properties, "Properties are required");
    return this;
  }

  /**
   * Sets the {@link #getPropertyName() configured property} to the given {@link String value}.
   *
   * @param propertyValue {@link String} containing the {@literal value} used to set the property.
   * @throws IllegalArgumentException if the {@link String property value} is {@literal null}.
   * @throws IllegalStateException if the {@link Properties} object is {@literal null}.
   * @see java.util.Properties#setProperty(String, String)
   * @see org.cp.elements.lang.annotation.Dsl
   * @see #getPropertyName()
   * @see #getProperties()
   */
  @Dsl
  public void to(@NotNull String propertyValue) {
    Assert.notNull(propertyValue, "Property value is required");
    getProperties().setProperty(getPropertyName(), propertyValue);
  }

  /**
   * Sets the {@link #getPropertyName() configured property} to the {@link String value} of the property
   * from the given, required {@link Properties}.
   *
   * @param source {@link Properties} used as the source of the value for the named property to set.
   * @throws IllegalArgumentException if source {@link Properties} are {@literal null}.
   * @throws IllegalStateException if target {@link Properties} are {@literal null}
   * or the source {@link Properties} does not contain the configured {@link #getPropertyName() property name}.
   * @see java.util.Properties#setProperty(String, String)
   * @see java.util.Properties#getProperty(String)
   * @see org.cp.elements.lang.annotation.Dsl
   * @see #getPropertyName()
   * @see #getProperties()
   */
  @Dsl
  public void with(@NotNull Properties source) {

    Assert.notNull(source, "Source Properties are required");

    String propertyName = getPropertyName();

    Assert.state(source.containsKey(propertyName),
      "Source Properties does not contain property name [%s]", propertyName);

    getProperties().setProperty(propertyName, source.getProperty(propertyName));
  }
}
