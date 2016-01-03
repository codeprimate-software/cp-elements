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

package org.cp.elements.context.configure;

import java.io.Serializable;

/**
 * The Configuration interface is an abstraction for modeling configuration settings used to configure any application
 * or program.
 *
 * @author John J. Blum
 * @see java.io.Serializable
 * @see java.lang.Iterable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Configuration extends Iterable<String>, Serializable {

  /**
   * Determines whether the configuration property identified by name is present in the configuration settings, which
   * means the configuration property was declared but not necessarily defined.
   *
   * @param propertyName a String value indicating the name of the configuration property.
   * @return a boolean value indicating if the property identified by name is present (declared) in the configuration
   * settings.
   * @see #isSet(String)
   */
  boolean isPresent(String propertyName);

  /**
   * Determines whether the configuration property identified by name is set in the configuration settings, which
   * means the configuration property was both declared and defined (set with a value).
   *
   * @param propertyName a String value indicating the name of the configuration property.
   * @return a boolean value indicating if the property identified by name is defined (set with a value) in the
   * configuration settings.
   * @see #isPresent(String)
   */
  boolean isSet(String propertyName);

  /**
   * Gets the value of the configuration property identified by name.  The property is required to be declared
   * and defined otherwise a ConfigurationException is thrown.
   *
   * @param propertyName a String value indicating the name of the configuration property.
   * @return the value of the configuration property identified by name.
   * @throws ConfigurationException if the property value was undeclared or is undefined.
   */
  String getPropertyValue(String propertyName);

  /**
   * Gets the value of the configuration property identified by name.  The required parameter can be used to indicate
   * the property is not required and that a ConfigurationException should not be thrown if the property is undeclared
   * or undefined.
   *
   * @param propertyName a String value indicating the name of the configuration property.
   * @param required used to indicate whether the configuration property is required to be declared and defined.
   * @return the value of the configuration property identified by name.
   * @throws ConfigurationException if and only if the property is required and the property is either undeclared
   * or undefined.
   */
  String getPropertyValue(String propertyName, boolean required);

  /**
   * Gets the value of the configuration property identified by name.  The defaultPropertyValue parameter effectively
   * overrides the required attribute indicating that the property is not required to be declared or defined.
   *
   * @param propertyName a String value indicating the name of the configuration property.
   * @param defaultPropertyValue the default value for the configuration property when the property is undeclared or
   * undefined.
   * @return the value of the configuration property identified by name, or the default property value if the property
   * was undeclared or undefined.
   */
  String getPropertyValue(String propertyName, String defaultPropertyValue);

  /**
   * Gets the value of the configuration property identified by name as a value of the specified Class type.
   * The property is required to be declared and defined otherwise a ConfigurationException is thrown.
   *
   * @param <T> the return class type of the property value.
   * @param propertyName a String value indicating the name of the configuration property.
   * @param type the expected Class type of the configuration property value.
   * @return the value of the configuration property identified by name.
   * @throws ConfigurationException if the property value was undeclared or is undefined.
   */
  <T> T getPropertyValueAs(String propertyName, Class<T> type);

  /**
   * Gets the value of the configuration property identified by name as a value of the specified Class type.
   * The required parameter can be used to indicate the property is not required and that a ConfigurationException
   * should not be thrown if the property is undeclared or undefined.
   *
   * @param <T> the return class type of the property value.
   * @param propertyName a String value indicating the name of the configuration property.
   * @param required used to indicate whether the configuration property is required to be declared and defined.
   * @param type the expected Class type of the configuration property value.
   * @return the value of the configuration property identified by name.
   * @throws ConfigurationException if and only if the property is required and the property is either undeclared
   * or undefined.
   */
  <T> T getPropertyValueAs(String propertyName, boolean required, Class<T> type);

  /**
   * Gets the value of the configuration property identified by name as a value of the specified Class type.
   * The defaultPropertyValue parameter effectively overrides the required attribute indicating that the property
   * is not required to be declared or defined.
   *
   * @param <T> the return class type of the property value.
   * @param propertyName a String value indicating the name of the configuration property.
   * @param defaultPropertyValue the default value for the configuration property when the property is undeclared or
   * undefined.
   * @param type the expected Class type of the configuration property value.
   * @return the value of the configuration property identified by name, or the default property value if the property
   * was undeclared or undefined.
   */
  <T> T getPropertyValueAs(String propertyName, T defaultPropertyValue, Class<T> type);

}
