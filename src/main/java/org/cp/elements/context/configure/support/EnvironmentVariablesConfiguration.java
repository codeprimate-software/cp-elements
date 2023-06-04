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

import java.util.Iterator;

import org.cp.elements.context.configure.AbstractConfiguration;
import org.cp.elements.context.configure.Configuration;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * {@link Configuration} implementation used to read configuration
 * from {@link System#getenv() System Environment Variables}.
 *
 * @author John J. Blum
 * @see java.lang.System#getenv()
 * @see org.cp.elements.context.configure.AbstractConfiguration
 * @see org.cp.elements.context.configure.Configuration
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class EnvironmentVariablesConfiguration extends AbstractConfiguration {

  /**
   * Constructs a new {@link EnvironmentVariablesConfiguration} with
   * initial {@literal environment variables} provided by the {@link System#getenv() System environment}.
   *
   * @see java.lang.System#getenv()
   */
  public EnvironmentVariablesConfiguration() {
    this(null);
  }

  /**
   * Constructs a new {@link EnvironmentVariablesConfiguration} initialized with the given
   * {@link Configuration parent} and initial {@literal environment variables} provided by
   * the {@link System#getenv() System environment}.
   *
   * @param parent {@link Configuration} used as the parent of this {@link Configuration}.
   * @see org.cp.elements.context.configure.AbstractConfiguration.MapConfigurationDescriptor
   * @see org.cp.elements.context.configure.Configuration
   * @see #setDescriptor(Descriptor)
   * @see java.lang.System#getenv()
   * @see java.util.Map
   */
  public EnvironmentVariablesConfiguration(@Nullable Configuration parent) {
    super(parent);
    setDescriptor(MapConfigurationDescriptor.from(System.getenv()));
  }

  /**
   * Determines whether the configuration property identified by {@link String name} is present (declared)
   * in the {@link System#getenv() System Environment Variables}.
   *
   * @param propertyName {@link String} containing the {@literal name} of the configuration property.
   * @return a boolean value indicating whether the configuration property identified by {@link String name}
   * is present (declared) in the {@link System#getenv() System Environment Variables}.
   * @see #isSet(String)
   */
  @Override
  public boolean isPresent(@Nullable String propertyName) {
    return StringUtils.hasText(propertyName) && System.getenv().containsKey(propertyName);
  }

  @Override
  protected @Nullable String doGetPropertyValue(@NotNull String propertyName) {
    return System.getenv(propertyName);
  }

  @Override
  public @NotNull Iterator<String> iterator() {
    return System.getenv().keySet().iterator();
  }
}
