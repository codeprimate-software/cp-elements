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

import java.util.Collections;
import java.util.Iterator;

import org.cp.elements.context.configure.AbstractConfiguration;
import org.cp.elements.context.configure.Configuration;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * {@link Configuration} implementation used to read configuration from
 * Java {@link System#getProperties() System properties}.
 *
 * @author John J. Blum
 * @see java.lang.System#getProperties()
 * @see org.cp.elements.context.configure.AbstractConfiguration
 * @see org.cp.elements.context.configure.Configuration
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class SystemPropertiesConfiguration extends AbstractConfiguration {

  /**
   * Constructs a new instance of {@link SystemPropertiesConfiguration} initialized with
   * Java {@link System#getProperties() System properties}.
   *
   * @see java.lang.System#getProperties()
   */
  public SystemPropertiesConfiguration() { }

  /**
   * Constructs a new instance of {@link SystemPropertiesConfiguration} initialized with
   * the given {@link Configuration parent} and Java {@link System#getProperties() System properties}.
   *
   * @param parent {@link Configuration} used as the parent of this {@link Configuration}.
   * @see org.cp.elements.context.configure.Configuration
   * @see java.lang.System#getProperties()
   */
  public SystemPropertiesConfiguration(@Nullable Configuration parent) {
    super(parent);
  }

  /**
   * Determines whether the configuration property identified by {@link String name} is present (declared)
   * in  the Java {@link System#getProperties() System properties}.
   *
   * @param propertyName {@link String} containing the {@literal name} of the configuration property.
   * @return a boolean value indicating whether the configuration property identified by {@link String name}
   * is present (declared) in the Java {@link System#getProperties() System properties}.
   * @see #isSet(String)
   */
  @Override
  public boolean isPresent(@Nullable String propertyName) {
    return StringUtils.hasText(propertyName) && System.getProperties().containsKey(propertyName);
  }

  @Override
  protected @Nullable String doGetPropertyValue(@NotNull String propertyName) {
    return System.getProperty(propertyName);
  }

  @Override
  public @NotNull Iterator<String> iterator() {
    return Collections.unmodifiableSet(System.getProperties().stringPropertyNames()).iterator();
  }
}
