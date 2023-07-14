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
 * a given system environment variable.
 *
 * @author John Blum
 * @see java.lang.System#getenv()
 * @see org.cp.elements.data.struct.StringBasedKeyValue
 * @see org.cp.elements.lang.annotation.ThreadSafe
 * @since 1.0.0
 */
@ThreadSafe
@SuppressWarnings("unused")
public class EnvironmentVariableValue extends StringBasedKeyValue {

  /**
   * Factory method used to construct a new {@link EnvironmentVariableValue} initialized with
   * the given system environment variable.
   *
   * @param environmentVariable System environment variable; must not be {@literal null}.
   * @return a new {@link EnvironmentVariableValue} initialized with the given system environment variable.
   * @throws IllegalArgumentException if the environment variable is {@literal null}.
   * @see #EnvironmentVariableValue(String)
   */
  public static EnvironmentVariableValue newEnvironmentVariableValue(@NotNull String environmentVariable) {
    return new EnvironmentVariableValue(environmentVariable);
  }

  /**
   * Constructs a new {@link EnvironmentVariableValue} initialized with the given system environment variable.
   *
   * @param environmentVariable System environment variable; must not be {@literal null}.
   * @throws IllegalArgumentException if the environment variable is {@literal null}.
   */
  public EnvironmentVariableValue(String environmentVariable) {
    super(environmentVariable);
  }

  /**
   * Get the {@link String value} of this environment variable as a {@literal null-safe} {@link Optional} value.
   *
   * @return the {@link String value} of this environment variable as a {@literal null-safe} {@link Optional} value.
   * @see java.lang.System#getenv(String)
   * @see java.util.Optional
   */
  @Override
  public Optional<String> getValue() {
    return Optional.ofNullable(System.getenv(getKey()));
  }
}
