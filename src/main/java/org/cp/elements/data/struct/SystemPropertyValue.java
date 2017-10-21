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

package org.cp.elements.data.struct;

import java.util.Optional;

import org.cp.elements.lang.concurrent.ThreadSafe;

/**
 * The {@link SystemPropertyValue} class is {@link KeyValue} data structure providing access to the value
 * of a given Java System Property.
 *
 * @author John Blum
 * @see java.lang.System#getProperties()
 * @see org.cp.elements.data.struct.StringBasedKeyValue
 * @since 1.0.0
 */
@ThreadSafe
@SuppressWarnings("unused")
public class SystemPropertyValue extends StringBasedKeyValue {

  /**
   * Factory method used to construct a new instance of {@link SystemPropertyValue} initialized with
   * the given system property.
   *
   * @param systemProperty Java system property.
   * @return a new instance of the {@link SystemPropertyValue} initialized with the given system property.
   * @throws IllegalArgumentException if system property is {@literal null}.
   * @see #SystemPropertyValue(String)
   */
  public static SystemPropertyValue newSystemPropertyValue(String systemProperty) {
    return new SystemPropertyValue(systemProperty);
  }

  /**
   * Constructs a new instance of {@link SystemPropertyValue} initialized with the given system property.
   *
   * @param systemProperty Java system property.
   * @throws IllegalArgumentException if system property is {@literal null}.
   */
  public SystemPropertyValue(String systemProperty) {
    super(systemProperty);
  }

  /**
   * Get the value of this system property as a {@literal null-safe} {@link Optional} value.
   *
   * @return the value of this system property as a {@literal null-safe} {@link Optional} value.
   * @see java.lang.System#getProperty(String)
   * @see java.util.Optional
   */
  @Override
  public Optional<String> getValue() {
    return Optional.ofNullable(System.getProperty(getKey()));
  }
}
