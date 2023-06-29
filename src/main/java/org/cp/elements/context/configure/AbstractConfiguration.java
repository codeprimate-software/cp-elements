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
package org.cp.elements.context.configure;

import static org.cp.elements.lang.ElementsExceptionsFactory.newConfigurationException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newConversionException;

import java.io.File;
import java.util.Map;
import java.util.Properties;

import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.data.conversion.ConversionService;
import org.cp.elements.data.conversion.ConversionServiceAware;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ClassUtils;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Abstract base class encapsulating functionality common to all {@link Configuration} implementations.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.util.Map
 * @see java.util.Properties
 * @see org.cp.elements.context.configure.Configuration
 * @see org.cp.elements.context.configure.Configuration.Descriptor
 * @see org.cp.elements.data.conversion.ConversionService
 * @see org.cp.elements.data.conversion.ConversionServiceAware
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractConfiguration implements Configuration, ConversionServiceAware {

  private final Configuration parent;

  private Configuration.Descriptor<?> descriptor;

  // TODO: Use PropertyEditors in addition to ConversionService!?!
  private ConversionService conversionService;

  /**
   * Constructs a new {@link AbstractConfiguration} with no parent {@link Configuration}.
   */
  public AbstractConfiguration() {
    this.parent = null;
  }

  /**
   * Constructs a new {@link AbstractConfiguration} initialized with the given {@literal parent}
   * {@link Configuration}.
   *
   * @param parent {@link Configuration} used as a fallback for retrieving configuration metadata
   * when not overridden by this {@link Configuration} object.
   * @see org.cp.elements.context.configure.Configuration
   */
  public AbstractConfiguration(@Nullable Configuration parent) {
    this.parent = parent;
  }

  /**
   * Sets a reference to the {@link ConversionService} used by this {@link Configuration} to convert configuration
   * property {@link String values} into an {@link Object} of the requested, target {@link Class type}.
   *
   * @param conversionService reference to the {@link ConversionService} used to perform configuration property
   * {@link String value} {@link Class type} conversions.
   * @throws IllegalArgumentException if the {@link ConversionService} is {@literal null}.
   * @see org.cp.elements.data.conversion.ConversionService
   */
  public final void setConversionService(@NotNull ConversionService conversionService) {
    this.conversionService = ObjectUtils.requireObject(conversionService,
      "The ConversionService used by this Configuration is required");
  }

  /**
   * Gets a reference to the {@link ConversionService} used by this {@link Configuration} to convert configuration
   * property {@link String values} into an {@link Object} of the requested, target {@link Class type}.
   *
   * @return a reference to the configured {@link ConversionService} used to perform property {@link String value}
   * {@link Class type} conversions.
   * @throws IllegalStateException if a {@link ConversionService} was not properly configured.
   * @see org.cp.elements.data.conversion.ConversionService
   */
  protected @NotNull ConversionService getConversionService() {
    return ObjectUtils.requireState(this.conversionService,
      "The ConversionService was not properly initialized");
  }

  /**
   * Gets a reference to the parent {@link Configuration} used as a fallback when requesting configuration metadata
   * by {@link String property name} and the configuration property is undeclared or undefined
   * by this {@link Configuration}.
   *
   * @return the configured parent {@link Configuration} object.
   * @see org.cp.elements.context.configure.Configuration
   */
  protected @Nullable Configuration getParent() {
    return this.parent;
  }

  /**
   * Asserts that the {@link String property name} is declared when passed as an argument
   * to the {@link #getPropertyValue(String, boolean)} and {@link #getPropertyValueAs(String, Class, boolean)} methods.
   *
   * @param propertyName {@link String} containing the {@literal name} of the property to assert.
   * @return the given {@link String property name}.
   * @throws IllegalArgumentException if the {@link String property name} was not declared.
   */
  protected @NotNull String assertPropertyName(@NotNull String propertyName) {
    return StringUtils.requireText(propertyName, "Property name [%s] is required");
  }

  /**
   * Converts the configuration property {@link String value} into an {@link Object} of the given,
   * required {@link Class type}.
   *
   * @param <T> {@link Class type} that the property {@link String value} will be converted into.
   * @param value {@link String} containing the property value to convert.
   * @param type {@link Class type} that the property {@link String value} will be converted into;
   * must not be {@literal null}.
   * @return the property {@link String value} converted into an {@link Object} of {@link Class type T}.
   * @throws IllegalArgumentException if the given {@link Class type} is {@literal null}.
   * @throws ConversionException if the property {@link String value} cannot be converted from a {@link String}
   * into a {@link Object} of {@link Class type T}.
   * @see org.cp.elements.data.conversion.ConversionService
   * @see #getConversionService()
   */
  protected <T> T convert(@Nullable String value, @NotNull Class<T> type) {

    Assert.notNull(type, "Class type to convert the String value to is required");

    ConversionService conversionService = getConversionService();

    if (conversionService.canConvert(String.class, type)) {
      return conversionService.convert(value, type);
    }

    throw newConversionException("Cannot convert String value [%1$s] into a value of type [%2$s]",
      value, type.getName());
  }

  /**
   * Determines whether this {@link Configuration} was configured with parent {@link Configuration}.
   *
   * @return a boolean value indicating whether this {@link Configuration} was configured with
   * parent {@link Configuration}.
   * @see #getParent()
   */
  @NullSafe
  protected boolean isParentConfigurationPresent() {
    return getParent() != null;
  }

  /**
   * Determines whether the configuration property identified by the given {@link String name} is present
   * in the configuration metadata.
   * <p>
   * Even if the configuration property was declared (present) does not mean the property is defined (set).
   * To determine if the configuration property has value, call {@link #isSet(String)}.
   *
   * @param propertyName {@link String} containing the {@literal name} of the configuration property.
   * @return a boolean value indicating whether the property identified by the given {@link String name}
   * is present (declared) in the configuration metadata.
   * @see #isSet(String)
   */
  @NullSafe
  public boolean isPresent(@Nullable String propertyName) {

    for (String configurationPropertyName : this) {
      if (configurationPropertyName.equals(propertyName)) {
        return true;
      }
    }

    return false;
  }

  /**
   * Determines whether the configuration property identified by the given {@link String name} is set (defined)
   * in the configuration metadata.
   * <p>
   * If a configuration property is set then it means the property was both declared and defined
   * in Java application (program) {@link Configuration} with a {@link String value}.
   *
   * @param propertyName {@link String} containing the {@literal name} of the configuration property.
   * @return a boolean value indicating whether the configuration property identified by the given {@link String name}
   * is set (defined) in the configuration metadata.
   * @see #getPropertyValue(String, boolean)
   * @see #doGetPropertyValue(String)
   * @see #isPresent(String)
   */
  @NullSafe
  public boolean isSet(@Nullable String propertyName) {
    return StringUtils.hasText(propertyName) && StringUtils.hasText(doGetPropertyValue(propertyName));
  }

  @NullSafe
  private boolean isNotSet(@Nullable String value) {
    return StringUtils.isBlank(value);
  }

  private @Nullable String returnNullWhenNotSet(@Nullable String value) {
    return StringUtils.hasText(value) ? value : null;
  }

  /**
   * Returns the configured {@link Configuration.Descriptor} used to describe this {@link Configuration}.
   *
   * @return the configured {@link Configuration.Descriptor} used to describe this {@link Configuration}.
   * @see org.cp.elements.context.configure.Configuration.Descriptor
   */
  @Override
  public @Nullable Descriptor<?> getDescriptor() {
    return this.descriptor;
  }

  /**
   * Sets (configures) the {@link Configuration.Descriptor} used to describe this {@link Configuration}.
   *
   * @param descriptor {@link Configuration.Descriptor} used to describe this {@link Configuration}.
   * @see org.cp.elements.context.configure.Configuration.Descriptor
   */
  protected void setDescriptor(@Nullable Descriptor<?> descriptor) {
    this.descriptor = descriptor;
  }

  /**
   * Gets the {@link String value} of the configuration property identified by the given {@link String name}.
   * <p>
   * The {@code required} parameter is used to indicate whether the configuration property is {@literal required}
   * or not and whether a {@link ConfigurationException} will be thrown when the property is undeclared or undefined.
   *
   * @param propertyName {@link String} containing the {@literal name} of the configuration property.
   * @param required boolean value used to indicate whether the configuration property is required
   * to be both declared and defined.
   * @return the {@link String value} of the {@link String named} configuration property. May return
   * a {@literal null} {@link String value} for the configuration property if not required.
   * @throws ConfigurationException if the configuration property is required and the property
   * is undeclared or undefined.
   * @see #isParentConfigurationPresent()
   * @see #doGetPropertyValue(String)
   */
  @Override
  public String getPropertyValue(String propertyName, boolean required) {

    String propertyValue = doGetPropertyValue(assertPropertyName(propertyName));

    if (isNotSet(propertyValue) && isParentConfigurationPresent()) {
      propertyValue = getParent().getPropertyValue(propertyName, required);
    }

    if (isNotSet(propertyValue) && required) {
      throw newConfigurationException("Property [%1$s] is required, but was not %2$s", propertyName,
        isPresent(propertyName) ? "defined" : "declared");
    }

    return returnNullWhenNotSet(propertyValue);
  }

  /**
   * Gets the value of the configuration property identified by the given {@link String name} as a {@link T value}
   * of the specified {@link Class type T}.
   * <p>
   * The {@code required} parameter is used to indicate whether the configuration property is {@literal required}
   * or not and whether a {@link ConfigurationException} will be thrown when the property is undeclared or undefined.
   *
   * @param <T> {@link Class type} in which to convert the returned value of the configuration property.
   * @param propertyName {@link String} containing the {@literal name} of the configuration property.
   * @param type expected {@link Class type} of the configuration property value when returned.
   * @param required boolean value used to indicate whether the configuration property is required
   * to be both declared and defined.
   * @return the value of the configuration property identified by the given {@link String name}
   * as a {@link T value} of the specified {@link Class type T}.
   * @throws ConfigurationException if the configuration property is required and the property
   * is undeclared or undefined of a {@link ConversionException} occurs while converting
   * the {@link String property value} into an instance of {@link Class type T}.
   * @see #getPropertyValue(String, boolean)
   * @see #convert(String, Class)
   */
  @Override
  public <T> T getPropertyValueAs(String propertyName, Class<T> type, boolean required) {

    String propertyValue = null;

    try {
      propertyValue = getPropertyValue(propertyName, required);
      return convert(propertyValue, type);
    }
    catch (ConversionException cause) {

      boolean throwConfigurationException = required || StringUtils.hasText(propertyValue);

      if (throwConfigurationException) {
        String message = "Failed to get value [%1$s] of configuration property [%2$s] as an instance of type [%3$s]";
        throw newConfigurationException(cause, message, propertyValue, propertyName, ClassUtils.getName(type));
      }

      return null;
    }
  }

  /**
   * Abstract method to be implemented by concrete {@link Configuration} implementations to handle retrieval
   * of the property value from the property source.
   *
   * @param propertyName {@link String} containing the {@literal name} of the configuration property.
   * @return a {@link String value} for the {@link String named} configuration property,
   * possibly returning {@literal null} if the property was not declared or defined.
   */
  protected abstract String doGetPropertyValue(String propertyName);

  /**
   * Abstract base class for {@link Configuration.Descriptor} implementations.
   *
   * @param <SOURCE> {@link Class type} parameter of the {@link Configuration} {@link Object source}.
   * @see org.cp.elements.context.configure.Configuration.Descriptor
   */
  protected abstract static class AbstractConfigurationDescriptor<SOURCE> implements Configuration.Descriptor<SOURCE> {

    private final SOURCE source;

    /**
     * Constructs a new {@link AbstractConfigurationDescriptor} initialized with the given,
     * required {@link SOURCE}.
     *
     * @param source {@link SOURCE} object used as the {@literal source} of this {@link Configuration}.
     * @throws IllegalArgumentException if the {@link SOURCE} is {@literal null}.
     */
    protected AbstractConfigurationDescriptor(@NotNull SOURCE source) {
      this.source = ObjectUtils.requireObject(source, "Source is required");
    }

    /**
     * Gets the {@link SOURCE} {@link Object} of this {@link Configuration}.
     *
     * @return the {@link SOURCE} {@link Object} of this {@link Configuration}.
     */
    @Override
    public @NotNull SOURCE getSource() {
      return this.source;
    }
  }

  /**
   * {@link Configuration.Descriptor} implementation sourced from a {@link File}.
   *
   * @see java.io.File
   */
  public static class FileConfigurationDescriptor extends AbstractConfigurationDescriptor<File> {

    /**
     * Factory method used to construct a new instance of {@link FileConfigurationDescriptor} initialized with
     * the given, required {@link File} used as the {@literal source} of the {@link Configuration}.
     *
     * @param file {@link File} used as the source of the {@link Configuration}; must not be {@literal null}.
     * @return a new {@link FileConfigurationDescriptor}.
     * @throws IllegalArgumentException if the {@link File} is {@literal null}.
     * @see java.io.File
     */
    public static @NotNull FileConfigurationDescriptor from(@NotNull File file) {
      return new FileConfigurationDescriptor(file);
    }

    /**
     * Constructs a new {@link FileConfigurationDescriptor} initialized with the given,
     * required {@link File} used as the {@literal source} of the {@link Configuration}.
     *
     * @param file {@link File} used as the source of the {@link Configuration}; must not be {@literal null}.
     * @throws IllegalArgumentException if the {@link File} is {@literal null}.
     * @see java.io.File
     */
    public FileConfigurationDescriptor(@NotNull File file) {
      super(file);
    }
  }

  /**
   * {@link Configuration.Descriptor} implementation sourced from a {@link Map}.
   *
   * @see java.util.Map
   */
  public static class MapConfigurationDescriptor extends AbstractConfigurationDescriptor<Map<String, String>> {

    /**
     * Factory method used to construct a new instance of {@link MapConfigurationDescriptor} initialized with
     * the given, required {@link Map} used as the {@literal source} of the {@link Configuration}.
     *
     * @param map {@link Map} used as the source of the {@link Configuration}; must not be {@literal null}.
     * @return a new {@link MapConfigurationDescriptor}.
     * @throws IllegalArgumentException if the {@link Map} is {@literal null}.
     * @see java.util.Map
     */
    public static @NotNull MapConfigurationDescriptor from(@NotNull Map<String, String> map) {
      return new MapConfigurationDescriptor(map);
    }

    /**
     * Constructs a new {@link MapConfigurationDescriptor} initialized with the given, required {@link Map}
     * used as the {@literal source} of the {@link Configuration}.
     *
     * @param map {@link Map} used as the source of the {@link Configuration}; must not be {@literal null}.
     * @throws IllegalArgumentException if the {@link Map} is {@literal null}.
     * @see java.util.Map
     */
    public MapConfigurationDescriptor(@NotNull Map<String, String> map) {
      super(map);
    }
  }

  /**
   * {@link Configuration.Descriptor} implementation sourced from a {@link Properties} object.
   *
   * @see java.util.Properties
   */
  public static class PropertiesConfigurationDescriptor extends AbstractConfigurationDescriptor<Properties> {

    /**
     * Factory method used to construct a new instance of {@link PropertiesConfigurationDescriptor} initialized with
     * the given, required {@link Properties} used as the {@literal source} of the {@link Configuration}.
     *
     * @param properties {@link Properties} used as the source of the {@link Configuration};
     * must not be {@literal null}.
     * @return a new {@link PropertiesConfigurationDescriptor}.
     * @throws IllegalArgumentException if the {@link Properties} are {@literal null}.
     * @see java.util.Properties
     */
    public static @NotNull PropertiesConfigurationDescriptor from(@NotNull Properties properties) {
      return new PropertiesConfigurationDescriptor(properties);
    }

    /**
     * Constructs a new {@link PropertiesConfigurationDescriptor} initialized with
     * the given, required {@link Properties} used as the {@literal source} of the {@link Configuration}.
     *
     * @param properties {@link Properties} used as the source of the {@link Configuration};
     * must not be {@literal null}.
     * @throws IllegalArgumentException if the {@link Properties} are {@literal null}.
     * @see java.util.Properties
     */
    public PropertiesConfigurationDescriptor(@NotNull Properties properties) {
      super(properties);
    }
  }
}
