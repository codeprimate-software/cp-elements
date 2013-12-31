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

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.util.convert.ConversionException;
import org.cp.elements.util.convert.ConversionService;

/**
 * The AbstractConfiguration class is an abstract base class for all Configuration implementations encapsulating common
 * functionality.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.context.configure.Configuration
 * @see org.cp.elements.util.convert.ConversionService
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractConfiguration implements Configuration {

  protected static final boolean DEFAULT_REQUIRED = true;
  protected static final boolean NOT_REQUIRED = false;

  private final Configuration parent;

  // TODO use PropertyEditors instead!?!
  private ConversionService conversionService;

  public AbstractConfiguration() {
    this.parent = null;
  }

  public AbstractConfiguration(final Configuration parent) {
    this.parent = parent;
  }

  protected ConversionService getConversionService() {
    Assert.state(conversionService != null, "The ConversionService was not properly initialized!");
    return conversionService;
  }

  public final void setConversionService(final ConversionService conversionService) {
    Assert.notNull(conversionService, "The ConversionService used to support this Configuration cannot be null!");
    this.conversionService = conversionService;
  }

  protected Configuration getParent() {
    return parent;
  }

  protected <T> T convert(final String value, final Class<T> type) {
    Assert.notNull(type, "The Class type to convert the String value to cannot be null!");

    if (getConversionService().canConvert(String.class, type)) {
      return getConversionService().convert(value, type);
    }

    throw new ConversionException(String.format("Cannot convert String value (%1$s) into a value of type (%2$s)!",
      value, type.getName()));
  }

  /**
   * Determines whether the configuration property identified by name is present in the configuration settings, which
   * means the configuration property was declared but not necessarily defined.
   * <p/>
   * @param propertyName a String value indicating the name of the configuration property.
   * @return a boolean value indicating if the property identified by name is present (declared) in the configuration
   * settings.
   * @see #isSet(String)
   */
  public boolean isPresent(final String propertyName) {
    for (String configurationPropertyName : this) {
      if (configurationPropertyName.equals(propertyName)) {
        return true;
      }
    }

    return false;
  }

  /**
   * Determines whether the configuration property identified by name is set in the configuration settings, which
   * means the configuration property was both declared and defined (set with a value).
   * <p/>
   * @param propertyName a String value indicating the name of the configuration property.
   * @return a boolean value indicating if the property identified by name is defined (set with a value) in the
   * configuration settings.
   * @see #isPresent(String)
   */
  public boolean isSet(final String propertyName) {
    return StringUtils.hasText(doGetPropertyValue(propertyName));
  }

  /**
   * Gets the value of the configuration property identified by name.  The property is required to be declared
   * and defined otherwise a ConfigurationException is thrown.
   * <p/>
   * @param propertyName a String value indicating the name of the configuration property.
   * @return the value of the configuration property identified by name.
   * @throws ConfigurationException if the property value was undeclared or is undefined.
   */
  public String getPropertyValue(final String propertyName) {
    return getPropertyValue(propertyName, DEFAULT_REQUIRED);
  }

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
  public String getPropertyValue(final String propertyName, final boolean required) {
    String propertyValue = doGetPropertyValue(propertyName);

    if (StringUtils.isBlank(propertyValue) && getParent() != null) {
      propertyValue = getParent().getPropertyValue(propertyName, required);
    }

    if (StringUtils.isBlank(propertyValue) && required) {
      throw new ConfigurationException(String.format("The property (%1$s) is required!", propertyName));
    }

    return propertyValue;
  }

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
  public String getPropertyValue(final String propertyName, final String defaultPropertyValue) {
    return ObjectUtils.defaultIfNull(getPropertyValue(propertyName, NOT_REQUIRED), defaultPropertyValue);
  }

  /**
   * Gets the value of the configuration property identified by name as a value of the specified Class type.
   * The property is required to be declared and defined otherwise a ConfigurationException is thrown.
   * <p/>
   * @param propertyName a String value indicating the name of the configuration property.
   * @param type the expected Class type of the configuration property value.
   * @return the value of the configuration property identified by name.
   * @throws ConfigurationException if the property value was undeclared or is undefined.
   */
  public <T> T getPropertyValueAs(final String propertyName, final Class<T> type) {
    return convert(getPropertyValue(propertyName, DEFAULT_REQUIRED), type);
  }

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
  public <T> T getPropertyValueAs(final String propertyName, final boolean required, final Class<T> type) {
    return convert(getPropertyValue(propertyName, required), type);
  }

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
  @SuppressWarnings("unchecked")
  public <T> T getPropertyValueAs(final String propertyName, final T defaultPropertyValue, final Class<T> type) {
    return ObjectUtils.defaultIfNull(convert(getPropertyValue(propertyName, NOT_REQUIRED), type), defaultPropertyValue);
  }

  protected abstract String doGetPropertyValue(String propertyName);

}
