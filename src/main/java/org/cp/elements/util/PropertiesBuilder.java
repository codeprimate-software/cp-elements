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

package org.cp.elements.util;

import java.util.Properties;

/**
 * The PropertiesBuilder class is a
 *
 * @author John J. Blum
 * @see java.util.Properties
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class PropertiesBuilder {

  private final Properties properties;

  public PropertiesBuilder() {
    properties = new Properties();
  }

  public PropertiesBuilder(final Properties defaults) {
    properties = new Properties(defaults);
  }

  protected Properties getProperties() {
    return properties;
  }

  public PropertiesBuilder setProperty(final String name, final String value) {
    getProperties().setProperty(name, value);
    return this;
  }

  public Properties build() {
    return getProperties();
  }

  @Override
  public String toString() {
    return getProperties().toString();
  }

}
