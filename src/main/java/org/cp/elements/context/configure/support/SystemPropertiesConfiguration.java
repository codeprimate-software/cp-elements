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

package org.cp.elements.context.configure.support;

import java.util.Collections;
import java.util.Iterator;

import org.cp.elements.context.configure.AbstractConfiguration;
import org.cp.elements.context.configure.Configuration;

/**
 * The SystemPropertiesConfiguration class is a Configuration implementation for reading configuration information
 * from System Properties.
 *
 * @author John J. Blum
 * @see java.lang.System
 * @see org.cp.elements.context.configure.AbstractConfiguration
 * @see org.cp.elements.context.configure.Configuration
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class SystemPropertiesConfiguration extends AbstractConfiguration {

  public SystemPropertiesConfiguration() {
  }

  public SystemPropertiesConfiguration(final Configuration parent) {
    super(parent);
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
    return System.getProperties().containsKey(propertyName);
  }

  @Override
  protected String doGetPropertyValue(final String propertyName) {
    return System.getProperty(propertyName);
  }

  @Override
  public Iterator<String> iterator() {
    return Collections.unmodifiableSet(System.getProperties().stringPropertyNames()).iterator();
  }

}
