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
import org.cp.elements.lang.ClassUtils;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.util.convert.ConversionException;
import org.cp.elements.util.convert.ConversionService;
import org.cp.elements.util.convert.ConversionServiceAware;

/**
 * The AbstractConfiguration class is an abstract base class encapsulating functionality common to all Configuration
 * implementations.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.context.configure.Configuration
 * @see org.cp.elements.util.convert.ConversionService
 * @see org.cp.elements.util.convert.ConversionServiceAware
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractConfiguration implements Configuration, ConversionServiceAware {

  protected static final boolean DEFAULT_REQUIRED = true;
  protected static final boolean NOT_REQUIRED = false;

  private final Configuration parent;

  // TODO use PropertyEditors in addition to ConversionService!?!
  private ConversionService conversionService;

  /**
   * Constructs an instance of the AbstractConfiguration class with no parent Configuration.
   */
  public AbstractConfiguration() {
    this.parent = null;
  }

  /**
   * Constructs an instance of the AbstractConfiguration class initialized with a parent Configuration.
   * <p/>
   * @param parent the fallback Configuration to retrieve configuration meta-data from when not overridden by this
   * Configuration object.
   * @see org.cp.elements.context.configure.Configuration
   */
  public AbstractConfiguration(final Configuration parent) {
    this.parent = parent;
  }

  /**
   * Gets a reference to the ConversionService used by this Configuration to convert configuration property values into
   * the requested, target type.
   * <p/>
   * @return a reference to a ConversionService to perform property value type conversions.
   * @throws IllegalStateException if a ConversionService reference was not provided.
   * @see org.cp.elements.util.convert.ConversionService
   */
  protected ConversionService getConversionService() {
    Assert.state(conversionService != null, "The ConversionService was not properly initialized!");
    return conversionService;
  }

  /**
   * Sets a reference to a ConversionService used by this Configuration to convert configuration property value into
   * the requested, target type.
   * <p/>
   * @param conversionService a reference to the ConversionService used to perform configuration property value type
   * conversions.
   * @throws NullPointerException if the ConversionService reference is null.
   * @see org.cp.elements.util.convert.ConversionService
   */
  public final void setConversionService(final ConversionService conversionService) {
    Assert.notNull(conversionService, "The ConversionService used to support this Configuration cannot be null!");
    this.conversionService = conversionService;
  }

  /**
   * Gets a reference to the parent Configuration used for fallback to retrieve configuration settings when undefined
   * by this Configuration.
   * <p/>
   * @return the parent Configuration object used for fallback to retrieve configuration settings.
   * @see org.cp.elements.context.configure.Configuration
   */
  protected Configuration getParent() {
    return parent;
  }

  /**
   * Converts the configuration setting property value into a value of the specified type.
   * <p/>
   * @param <T> the type of object the String property value is converted into.
   * @param value the String property value to convert.
   * @param type the Class type to convert the String property value into.
   * @return the String property value into a value of type T.
   * @throws NullPointerException if the specified conversion Class type is null.
   * @throws ConversionException if the String property value cannot be converted into an object of type T.
   * @see #getConversionService()
   * @see org.cp.elements.util.convert.ConversionService
   */
  protected <T> T convert(final String value, final Class<T> type) {
    Assert.notNull(type, "The Class type to convert the String value to cannot be null!");

    if (getConversionService().canConvert(String.class, type)) {
      return getConversionService().convert(value, type);
    }

    throw new ConversionException(String.format("Cannot convert String value (%1$s) into a value of type (%2$s)!",
      value, type.getName()));
  }

  /**
   * Returns value if not blank otherwise returns default value.
   * <p/>
   * @param value the String to evaluate for value (containing text).
   * @param defaultValue the String value to return if the 'value' is blank.
   * @return value if it has text otherwise returns the default value.
   * @see org.cp.elements.lang.StringUtils#hasText(String)
   */
  protected String defaultIfUnset(final String value, final String defaultValue) {
    return (StringUtils.hasText(value) ? value : defaultValue);
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

    return defaultIfUnset(propertyValue, null);
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
    return defaultIfUnset(getPropertyValue(propertyName, NOT_REQUIRED), defaultPropertyValue);
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
    try {
      return convert(getPropertyValue(propertyName, required), type);
    }
    catch (ConversionException e) {
      if (required) {
        throw new ConfigurationException(String.format(
          "Failed to get the value of configuration setting property (%1$s) as type (%2$s)!", propertyName,
            ClassUtils.getName(type)), e);
      }

      return null;
    }
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
    try {
      return ObjectUtils.defaultIfNull(convert(getPropertyValue(propertyName, NOT_REQUIRED), type),
        defaultPropertyValue);
    }
    catch (ConversionException ignore) {
      return defaultPropertyValue;
    }
  }

  /**
   * Abstract method to be implemented by concrete Configuration implementations to handle retrieval
   * of the property value from the property source.
   * <p/>
   * @param propertyName the String name of the property to retrieve.
   * @return a String value of the named property or possibly null if the property is not set or undefined.
   */
  protected abstract String doGetPropertyValue(String propertyName);

}
