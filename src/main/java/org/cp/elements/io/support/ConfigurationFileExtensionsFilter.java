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

package org.cp.elements.io.support;

import org.cp.elements.io.FileExtensionFilter;

/**
 * The ConfigurationFileExtensionsFilter class is a {@link FileExtensionFilter} implementation
 * that filters {@link java.io.File}s by configuration file types.
 *
 * @author John J. Blum
 * @see org.cp.elements.io.FileExtensionFilter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ConfigurationFileExtensionsFilter extends FileExtensionFilter {

  protected static final String[] CONFIGURATION_FILE_EXTENSIONS = {
    "cfg",
    "cnf",
    "conf",
    "config",
    "ini",
    "json",
    "prop",
    "properties",
    "xml",
    "yaml"
  };

  /**
   * Constructs an instance of the {@link ConfigurationFileExtensionsFilter} class to filter {@link java.io.File}s
   * by configuration file types.
   *
   * @see org.cp.elements.io.FileExtensionFilter#FileExtensionFilter(String...)
   */
  public ConfigurationFileExtensionsFilter() {
    super(CONFIGURATION_FILE_EXTENSIONS);
  }
}
