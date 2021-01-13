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

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Collections;
import java.util.Iterator;
import java.util.Properties;

import org.cp.elements.context.configure.AbstractConfiguration;
import org.cp.elements.context.configure.Configuration;
import org.cp.elements.lang.Assert;

/**
 * The PropertiesConfiguration class is a Configuration implementation for reading configuration information
 * backed by a Properties object.
 *
 * @author John J. Blum
 * @see java.util.Properties
 * @see org.cp.elements.context.configure.AbstractConfiguration
 * @see org.cp.elements.context.configure.Configuration
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class PropertiesConfiguration extends AbstractConfiguration {

  private final Properties properties;

  public PropertiesConfiguration(final File propertiesFile) throws IOException {
    this(propertiesFile, null);
  }

  public PropertiesConfiguration(final File propertiesFile, final Configuration parent) throws IOException {
    super(parent);
    Assert.notNull(propertiesFile, "The file to load properties from cannot be null!");
    this.properties = new Properties();
    this.properties.load(new FileInputStream(propertiesFile));
  }

  public PropertiesConfiguration(final Properties properties) {
    this(properties, null);
  }

  public PropertiesConfiguration(final Properties properties, final Configuration parent) {
    super(parent);
    Assert.notNull(properties, "The Properties object used to back this Configuration cannot be null!");
    this.properties = properties;
  }

  protected Properties getProperties() {
    return properties;
  }

  /**
   * Determines whether the configuration property identified by name is present in the configuration settings, which
   * means the configuration property was declared but not necessarily defined.
   *
   * @param propertyName a String value indicating the name of the configuration property.
   * @return a boolean value indicating if the property identified by name is present (declared) in the configuration
   * settings.
   * @see #isSet(String)
   */
  @Override
  public boolean isPresent(final String propertyName) {
    return getProperties().containsKey(propertyName);
  }

  @Override
  protected String doGetPropertyValue(final String propertyName) {
    return getProperties().getProperty(propertyName);
  }

  @Override
  public Iterator<String> iterator() {
    return Collections.unmodifiableSet(getProperties().stringPropertyNames()).iterator();
  }

}
