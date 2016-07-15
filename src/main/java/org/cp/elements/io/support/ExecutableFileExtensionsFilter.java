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

package org.cp.elements.io.support;

import org.cp.elements.io.FileExtensionFilter;

/**
 * The ExecutableFileExtensionsFilter class is a {@link FileExtensionFilter} implementation
 * that filters {@link java.io.File}s by executable file types.
 *
 * @author John J. Blum
 * @see org.cp.elements.io.FileExtensionFilter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ExecutableFileExtensionsFilter extends FileExtensionFilter {

  protected static final String[] EXECUTABLE_FILE_EXTENSIONS = {
    "bat",
    "bin",
    "cmd",
    "com",
    "csh",
    "exe",
    "groovy",
    "jar",
    "js",
    "ksh",
    "out",
    "py",
    "rb",
    "script",
    "sh"
  };

  /**
   * Constructs an instance of the {@link ExecutableFileExtensionsFilter} class to filter {@link java.io.File}s
   * by executable file types.
   *
   * @see org.cp.elements.io.FileExtensionFilter#FileExtensionFilter(String...)
   */
  public ExecutableFileExtensionsFilter() {
    super(EXECUTABLE_FILE_EXTENSIONS);
  }
}
