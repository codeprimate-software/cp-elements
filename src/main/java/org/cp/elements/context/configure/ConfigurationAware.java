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

package org.cp.elements.context.configure;

/**
 * The ConfigurationAware interface specifies a contract for objects that require a reference to a
 * Configuration instance.
 *
 * @author John J. Blum
 * @see org.cp.elements.context.configure.Configuration
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface ConfigurationAware {

  /**
   * Sets a reference to an instance of the application Configuration in use.
   *
   * @param configuration the Configuration reference in use by the application.
   */
  void setConfiguration(Configuration configuration);

}
