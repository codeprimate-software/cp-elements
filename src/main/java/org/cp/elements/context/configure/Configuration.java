/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.context.configure;

import java.io.Serializable;

/**
 * The Configuration interface is an abstraction for modeling configuration settings used to configure any application
 * or program.
 * <p/>
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
   * <p/>
   * @param propertyName a String value indicating the name of the configuration property.
   * @return a boolean value indicating if the property identified by name is present (declared) in the configuration
   * settings.
   * @see #isSet(String)
   */
  public boolean isPresent(String propertyName);

  /**
   * Determines whether the configuration property identified by name is set in the configuration settings, which
   * means the configuration property was both declared and defined (set with a value).
   * <p/>
   * @param propertyName a String value indicating the name of the configuration property.
   * @return a boolean value indicating if the property identified by name is defined (set with a value) in the
   * configuration settings.
   * @see #isPresent(String)
   */
  public boolean isSet(String propertyName);

  /**
   * Gets the value of the configuration property identified by name.  The property is required to be declared
   * and defined otherwise a ConfigurationException is thrown.
   * <p/>
   * @param propertyName a String value indicating the name of the configuration property.
   * @return the value of the configuration property identified by name.
   * @throws ConfigurationException if the property value was undeclared or is undefined.
   */
  public String getPropertyValue(String propertyName);

  /**
   * Gets the value of the configuration property identified by name.  The required parameter can be used to indicate
   * the property is not required and that a ConfigurationException should not be thrown if the property is undeclared
   * or undefined.
   * <p/>
   * @param propertyName a String value indicating the name of the configuration property.
   * @param required used to indicate whether the configuration property is required to be declared and defined.
   * @return the value of the configuration property identified by name.
   * @throws ConfigurationException if and only if the property is required and the property is either undeclared
   * or undefined.
   */
  public String getPropertyValue(String propertyName, boolean required);

  /**
   * Gets the value of the configuration property identified by name.  The defaultPropertyValue parameter effectively
   * overrides the required attribute indicating that the property is not required to be declared or defined.
   * <p/>
   * @param propertyName a String value indicating the name of the configuration property.
   * @param defaultPropertyValue the default value for the configuration property when the property is undeclared or
   * undefined.
   * @return the value of the configuration property identified by name, or the default property value if the property
   * was undeclared or undefined.
   */
  public String getPropertyValue(String propertyName, String defaultPropertyValue);

  /**
   * Gets the value of the configuration property identified by name as a value of the specified Class type.
   * The property is required to be declared and defined otherwise a ConfigurationException is thrown.
   * <p/>
   * @param propertyName a String value indicating the name of the configuration property.
   * @param type the expected Class type of the configuration property value.
   * @return the value of the configuration property identified by name.
   * @throws ConfigurationException if the property value was undeclared or is undefined.
   */
  public <T> T getPropertyValueAs(String propertyName, Class<T> type);

  /**
   * Gets the value of the configuration property identified by name as a value of the specified Class type.
   * The required parameter can be used to indicate the property is not required and that a ConfigurationException
   * should not be thrown if the property is undeclared or undefined.
   * <p/>
   * @param propertyName a String value indicating the name of the configuration property.
   * @param required used to indicate whether the configuration property is required to be declared and defined.
   * @param type the expected Class type of the configuration property value.
   * @return the value of the configuration property identified by name.
   * @throws ConfigurationException if and only if the property is required and the property is either undeclared
   * or undefined.
   */
  public <T> T getPropertyValueAs(String propertyName, boolean required, Class<T> type);

  /**
   * Gets the value of the configuration property identified by name as a value of the specified Class type.
   * The defaultPropertyValue parameter effectively overrides the required attribute indicating that the property
   * is not required to be declared or defined.
   * <p/>
   * @param propertyName a String value indicating the name of the configuration property.
   * @param defaultPropertyValue the default value for the configuration property when the property is undeclared or
   * undefined.
   * @param type the expected Class type of the configuration property value.
   * @return the value of the configuration property identified by name, or the default property value if the property
   * was undeclared or undefined.
   */
  public <T> T getPropertyValueAs(String propertyName, T defaultPropertyValue, Class<T> type);

}
