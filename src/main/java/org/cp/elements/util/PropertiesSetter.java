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
import org.cp.elements.lang.annotation.FluentApi;

/**
 * The {@link PropertiesSetter} class is abstract utility class for setting properties on a {@link Properties} object.
 *
 * @author John J. Blum
 * @see java.util.Properties
 * @see org.cp.elements.lang.FluentApiExtension
 * @see org.cp.elements.lang.annotation.FluentApi
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class PropertiesSetter implements FluentApiExtension {

  private Properties properties;

  private final String propertyName;

  /**
   * Factory method to construct an instance of {@link PropertiesSetter} initialized with the given property name.
   *
   * @param propertyName {@link String} indicating the name of the property to set.
   * @return a new instance of {@link PropertiesSetter} initialized with the given property name.
   * @throws IllegalArgumentException if {@code propertyName} was not specified.
   * @see org.cp.elements.lang.annotation.FluentApi
   * @see org.cp.elements.util.PropertiesSetter
   */
  @FluentApi
  public static PropertiesSetter set(String propertyName) {
    return new PropertiesSetter(propertyName) {};
  }

  /**
   * Constructs an instance of the {@link PropertiesSetter} initialized with the given property name.
   *
   * @param propertyName {@link String} indicating the name of the property to set.
   * @throws IllegalArgumentException if {@code propertyName} was not specified.
   */
  protected PropertiesSetter(String propertyName) {
    Assert.hasText(propertyName, "Property name must be specified");
    this.propertyName = propertyName;
  }

  /**
   * Returns a reference to the {@link Properties} object on which the property will be set.
   *
   * @return a reference to the {@link Properties} object on which the property will be set.
   * @throws IllegalStateException if the {@link Properties} reference is {@literal null}.
   * @see java.util.Properties
   */
  protected Properties getProperties() {
    Assert.state(this.properties != null, "Properties were not specified");
    return this.properties;
  }

  /**
   * Returns the name of the property to set.
   *
   * @return a {@link String} indicating the name of the property to set.
   */
  protected String getPropertyName() {
    return this.propertyName;
  }

  /**
   * Sets a reference to the {@link Properties} object on which the property will be set.
   *
   * @param properties {@link Properties} object on which the property will be set.
   * @return this {@link PropertiesSetter}.
   * @throws IllegalArgumentException if {@link Properties} is {@literal null}.
   * @see java.util.Properties
   */
  public PropertiesSetter of(Properties properties) {
    Assert.notNull(properties, "Properties cannot be null");
    this.properties = properties;
    return this;
  }

  /**
   * Sets the identified property to the given value.
   *
   * @param propertyValue {@link String} containing the value to which the property will be set.
   * @throws IllegalStateException if the {@link Properties} reference is {@literal null}.
   * @see java.util.Properties#setProperty(String, String)
   * @see #getProperties()
   * @see #getPropertyName()
   */
  public void to(String propertyValue) {
    getProperties().setProperty(getPropertyName(), propertyValue);
  }

  /**
   * Sets the identified property with name to the value of the property from the given {@link Properties}.
   *
   * @param source {@link Properties} used as the source of the value for the named property to set.
   * @throws IllegalArgumentException if source {@link Properties} are {@literal null}.
   * @throws IllegalStateException if target {@link Properties} are {@literal null}.
   * @see java.util.Properties#getProperty(String)
   * @see java.util.Properties#setProperty(String, String)
   * @see #getProperties()
   * @see #getPropertyName()
   */
  public void with(Properties source) {
    Assert.notNull(source, "Source properties cannot be null");
    String propertyName = getPropertyName();
    getProperties().setProperty(propertyName, source.getProperty(propertyName));
  }
}
