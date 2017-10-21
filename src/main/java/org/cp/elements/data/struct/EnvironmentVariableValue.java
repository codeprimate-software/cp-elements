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
 * The {@link EnvironmentVariableValue} class is {@link KeyValue} data structure providing access to the value
 * of a given System environment variable.
 *
 * @author John Blum
 * @see java.lang.System#getenv()
 * @see org.cp.elements.data.struct.StringBasedKeyValue
 * @since 1.0.0
 */
@ThreadSafe
@SuppressWarnings("unused")
public class EnvironmentVariableValue extends StringBasedKeyValue {

  /**
   * Factory method used to construct a new instance of {@link EnvironmentVariableValue} initialized with
   * the given system environment variable.
   *
   * @param environmentVariable System environment variable.
   * @return a new instance of {@link EnvironmentVariableValue} initialized with the given system environment variable.
   * @throws IllegalArgumentException if environment variable is {@literal null}.
   * @see #EnvironmentVariableValue(String)
   */
  public static EnvironmentVariableValue newEnvironmentVariableValue(String environmentVariable) {
    return new EnvironmentVariableValue(environmentVariable);
  }

  /**
   * Constructs a new instance of {@link EnvironmentVariableValue} initialized with the given environment variable.
   *
   * @param environmentVariable System environment variable.
   * @throws IllegalArgumentException if environment variable is {@literal null}.
   */
  public EnvironmentVariableValue(String environmentVariable) {
    super(environmentVariable);
  }

  /**
   * Get the value of this environment variable as a {@literal null-safe} {@link Optional} value.
   *
   * @return the value of this environment variable as a {@literal null-safe} {@link Optional} value.
   * @see java.lang.System#getenv(String)
   * @see java.util.Optional
   */
  @Override
  public Optional<String> getValue() {
    return Optional.ofNullable(System.getenv(getKey()));
  }
}
