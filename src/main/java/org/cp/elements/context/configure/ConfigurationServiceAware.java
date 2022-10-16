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
package org.cp.elements.context.configure;

/**
 * {@link FunctionalInterface} defining a contract for Java program (application) components
 * that need to be aware of the {@link ConfigurationService}.
 *
 * @author John Blum
 * @see java.lang.FunctionalInterface
 * @see org.cp.elements.context.configure.ConfigurationService
 * @since 1.0.0
 */
@FunctionalInterface
@SuppressWarnings("unused")
public interface ConfigurationServiceAware {

  /**
   * Sets a reference to the Java application (program) {@link ConfigurationService}.
   *
   * @param configurationService {@link ConfigurationService} used by the Java application (program)
   * to acquire configuration metadata.
   * @see org.cp.elements.context.configure.ConfigurationService
   */
  void setConfigurationService(ConfigurationService configurationService);

}
