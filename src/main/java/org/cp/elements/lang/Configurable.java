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
package org.cp.elements.lang;

/**
 * The {@link Configurable} interface defines a contract for {@link Object Objects} that can be configured.
 *
 * This {@link Class interface} allows implementers to define parameters under which their {@link Object Objects}
 * will operate. Configuration metadata can be specific to application context, environment or runtime.
 *
 * @author John J. Blum
 * @param <T> {@link Class} type of configuration metadata and settings.
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Configurable<T> {

  /**
   * Determines whether {@literal this} {@link Object} has been properly configured.
   *
   * An {@link Object} is configured when it's {@link #configure} method is invoked with a configuration parameter.
   *
   * @return a boolean value indicating whether {@literal this} {@link Object} has been properly configured.
   */
  boolean isConfigured();

  /**
   * Gets the configuration metadata of type T used to configure {@literal this} {@link Object}.
   *
   * @return the configuration metadata used to configure {@literal this} {@link Object}.
   */
  T getConfiguration();

  /**
   * Configures {@literal this} {@link Object} with the given configuration metadata.
   *
   * @param configuration configuration metadata of type T used to configure {@literal this} {@link Object}.
   */
  void configure(T configuration);

}
