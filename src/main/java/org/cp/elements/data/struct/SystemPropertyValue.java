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
package org.cp.elements.data.struct;

import java.util.Optional;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.ThreadSafe;

/**
 * A {@link StringBasedKeyValue} data structure providing access to the {@link String value} of
 * a given Java System Property.
 *
 * @author John Blum
 * @see java.lang.System#getProperties()
 * @see org.cp.elements.data.struct.StringBasedKeyValue
 * @see org.cp.elements.lang.annotation.ThreadSafe
 * @since 1.0.0
 */
@ThreadSafe
@SuppressWarnings("unused")
public class SystemPropertyValue extends StringBasedKeyValue {

  /**
   * Factory method used to construct a new {@link SystemPropertyValue} initialized with the given Java
   * {@link String system property}.
   *
   * @param systemProperty {@link String} containing the {@literal Java system property};
   * must not be {@literal null} or {@literal empty}.
   * @return a new {@link SystemPropertyValue} initialized with the given Java {@link String system property}.
   * @throws IllegalArgumentException if the {@literal Java system property} is {@literal null} or {@literal empty}.
   * @see #SystemPropertyValue(String)
   */
  public static SystemPropertyValue newSystemPropertyValue(@NotNull String systemProperty) {
    return new SystemPropertyValue(systemProperty);
  }

  /**
   * Constructs a new {@link SystemPropertyValue} initialized with the given Java {@link String system property}.
   *
   * @param systemProperty {@link String} containing the {@literal Java system property};
   * must not be {@literal null} or {@literal empty}.
   * @throws IllegalArgumentException if the {@literal Java system property} is {@literal null} or {@literal empty}.
   */
  public SystemPropertyValue(@NotNull String systemProperty) {
    super(systemProperty);
  }

  /**
   * Get the {@link String value} of this {@literal Java system property} as a {@literal null-safe}
   * {@link Optional} value.
   *
   * @return the {@link String value} of this {@literal Java system property} as a {@literal null-safe}
   * {@link Optional} value.
   * @see java.lang.System#getProperty(String)
   * @see java.util.Optional
   */
  @Override
  public Optional<String> getValue() {
    return Optional.ofNullable(System.getProperty(getKey()));
  }
}
