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

package org.cp.elements.lang;

/**
 * The {@link Configurable} interface defines a contract for configuring an object who's class implements this interface.
 * This interface allows implementers to define parameters under which their objects will operate.  Configuration
 * meta-data can be specific to environment, runtime or other contextual information.
 *
 * @author John J. Blum
 * @param <T> the type of configuration meta-data and settings.
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Configurable<T> {

  /**
   * Determines whether this object has been properly configured.  An object is configured when it's configure method
   * is invoked with configuration parameter.
   *
   * @return a boolean value indicating whether this object has been properly configured.
   */
  boolean isConfigured();

  /**
   * Gets the configuration meta-data of type T used to configure this object.
   *
   * @return the configuration meta-data used to configure this object.
   */
  T getConfiguration();

  /**
   * Configures this object with the given configuration meta-data of type T.
   *
   * @param configuration the configuration meta-data of type T used to configure this object.
   */
  void configure(T configuration);

}
