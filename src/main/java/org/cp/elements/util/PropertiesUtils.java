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

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Abstract utility class for processing Java {@link Properties}.
 *
 * @author John Blum
 * @see java.util.Properties
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class PropertiesUtils {

  /**
   * Utility method used to guard against {@literal null} {@link Properties}.
   *
   * @param properties {@link Properties} to evaluate as {@literal null}.
   * @return the given {@link Properties} if not {@literal null} or a new {@link Properties} object
   * if the given {@link Properties} reference is {@literal null}.
   * @see java.util.Properties
   */
  @NullSafe
  public static @NotNull Properties nullSafeProperties(@Nullable Properties properties) {
    return properties != null ? properties : new Properties();
  }

  /**
   * Factory method used to construct a new instance of {@link Properties} as a {@literal Singleton} {@link Object},
   * or simply a {@link Properties} instance containing only a single {@link String property} and {@link String value}.
   *
   * @param propertyName {@link String} containing the {@literal name} of the property; must not be {@literal null}.
   * @param propertyValue {@link String} containing the {@literal value} for the property; must not be {@literal null}.
   * @return a new {@link Properties} instance as a {@literal Singleton}.
   * @see java.util.Properties
   */
  public static @NotNull Properties singletonProperties(@NotNull String propertyName, @NotNull String propertyValue) {

    Assert.notNull(propertyName, "Property name is required");
    Assert.notNull(propertyValue, "Value for property [%s] is required", propertyName);

    Properties singleProperty = new Properties();

    singleProperty.setProperty(propertyName, propertyValue);

    return singleProperty;
  }
}
