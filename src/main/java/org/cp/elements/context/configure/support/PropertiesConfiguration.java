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
package org.cp.elements.context.configure.support;

import static org.cp.elements.lang.ElementsExceptionsFactory.newConfigurationException;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.Serial;
import java.util.Collections;
import java.util.Iterator;
import java.util.Properties;

import org.cp.elements.context.configure.AbstractConfiguration;
import org.cp.elements.context.configure.Configuration;
import org.cp.elements.context.configure.ConfigurationException;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * {@link Configuration} implementation for reading configuration metadata backed by {@link Properties}.
 *
 * @author John J. Blum
 * @see java.util.Properties
 * @see org.cp.elements.context.configure.AbstractConfiguration
 * @see org.cp.elements.context.configure.Configuration
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class PropertiesConfiguration extends AbstractConfiguration {

  @Serial
  private static final long serialVersionUID = -6032166841545827263L;

  private final Properties properties;

  /**
   * Constructs a new {@link PropertiesConfiguration} initialized with {@link Properties}
   * contained in the given, required {@link File}.
   *
   * @param propertiesFile {@link File} containing {@link Properties} for this {@link Configuration};
   * must not be {@literal null}.
   * @throws IllegalArgumentException if {@link File} is {@literal null}.
   * @throws ConfigurationException if an error occurs while reading the {@link File properties}.
   * @see java.io.File
   */
  public PropertiesConfiguration(@NotNull File propertiesFile) {
    this(propertiesFile, null);
  }

  /**
   * Constructs a new {@link PropertiesConfiguration} initialized with {@link Properties}
   * contained in the given, required {@link File} and the {@link Configuration parent} as backup.
   *
   * @param propertiesFile {@link File} containing {@link Properties} for this {@link Configuration};
   * must not be {@literal null}.
   * @param parent {@link Configuration} used as the {@literal parent} of this {@link Configuration}.
   * @throws IllegalArgumentException if {@link File properties} is {@literal null}.
   * @throws ConfigurationException if an error occurs while reading the {@link File properties}.
   * @see org.cp.elements.context.configure.AbstractConfiguration.FileConfigurationDescriptor
   * @see org.cp.elements.context.configure.Configuration
   * @see #setDescriptor(Descriptor)
   * @see java.io.File
   */
  public PropertiesConfiguration(@NotNull File propertiesFile, @Nullable Configuration parent) {

    super(parent);

    Assert.notNull(propertiesFile, "The file to load properties from is required");

    setDescriptor(FileConfigurationDescriptor.from(propertiesFile));

    this.properties = new Properties();

    try (FileInputStream fileInputStream = new FileInputStream(propertiesFile)) {
      this.properties.load(fileInputStream);
    }
    catch (IOException cause) {
      throw newConfigurationException(cause, "Failed to initialize configuration from properties file [%s]",
        propertiesFile);
    }
  }

  /**
   * Constructs a new {@link PropertiesConfiguration} initialized with the given,
   * required {@link Properties}.
   *
   * @param properties {@link Properties} used for this {@link Configuration}; must not be {@literal null}.
   * @throws IllegalArgumentException if the {@link Properties} are {@literal null}.
   * @see java.util.Properties
   */
  public PropertiesConfiguration(@NotNull Properties properties) {
    this(properties, null);
  }

  /**
   * Constructs a new {@link PropertiesConfiguration} initialized with the given,
   * required {@link Properties} and {@link Configuration parent} as backup.
   *
   * @param properties {@link Properties} used for this {@link Configuration}; must not be {@literal null}.
   * @param parent {@link Configuration} used as the {@literal parent} of this {@link Configuration}.
   * @throws IllegalArgumentException if the {@link Properties} are {@literal null}.
   * @see org.cp.elements.context.configure.AbstractConfiguration.PropertiesConfigurationDescriptor
   * @see org.cp.elements.context.configure.Configuration
   * @see #setDescriptor(Descriptor)
   * @see java.util.Properties
   */
  public PropertiesConfiguration(@NotNull Properties properties, @Nullable Configuration parent) {

    super(parent);

    this.properties = ObjectUtils.requireObject(properties,
      "The Properties used to back this Configuration is required");

    setDescriptor(PropertiesConfigurationDescriptor.from(this.properties));
  }

  /**
   * Gets the {@link Properties} object used to back this {@link Configuration}.
   *
   * @return the {@link Properties} object used to back this {@link Configuration}.
   * @see java.util.Properties
   */
  protected @NotNull Properties getProperties() {
    return this.properties;
  }

  /**
   * Determines whether the configuration property identified by {@link String name} is present (declared)
   * in this {@link Configuration}.
   * <p>
   * If the configuration property is {@literal present}, then it simply means the configuration property
   * was declared, but not necessarily defined.
   *
   * @param propertyName {@link String} containing the {@literal name} of the configuration property.
   * @return a boolean value indicating whether the configuration property identified by {@link String name}
   * is present (declared) in this {@link Configuration}.
   * @see #isSet(String)
   */
  @Override
  public boolean isPresent(@Nullable String propertyName) {
    return StringUtils.hasText(propertyName) && getProperties().containsKey(propertyName);
  }

  @Override
  protected @Nullable String doGetPropertyValue(@NotNull String propertyName) {
    return getProperties().getProperty(propertyName);
  }

  @Override
  public @NotNull Iterator<String> iterator() {
    return Collections.unmodifiableSet(getProperties().stringPropertyNames()).iterator();
  }
}
