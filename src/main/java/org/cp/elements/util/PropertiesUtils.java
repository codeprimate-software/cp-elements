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

/**
 * The {@link PropertiesUtils} class is a utility class for working with {@link java.util.Properties}.
 *
 * @author John Blum
 * @see java.util.Properties
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class PropertiesUtils {

  /**
   * Factory method to construct a Singleton {@link Properties} object, a {@link Properties} instance containing
   * only a single property and value.
   *
   * @param propertyName name of the property.
   * @param propertyValue value of the property.
   * @return a Singleton {@link Properties} instance.
   * @see java.util.Properties
   */
  public static Properties singletonProperties(String propertyName, String propertyValue) {
    Properties singleProperty = new Properties();
    singleProperty.setProperty(propertyName, propertyValue);
    return singleProperty;
  }
}
