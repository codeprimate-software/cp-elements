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

import java.io.File;

import org.cp.elements.io.FileExtensionFilter;

/**
 * {@link FileExtensionFilter} implementation that filters {@link File Files} by text file types.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see org.cp.elements.io.FileExtensionFilter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class TextFileExtensionsFilter extends FileExtensionFilter {

  static final String[] TEXT_FILE_EXTENSIONS = {
    "dbg",
    "log",
    "rtf",
    "text",
    "txt"
  };

  /**
   * Constructs a new {@link TextFileExtensionsFilter} to filter {@link File Files} by text file types.
   *
   * @see org.cp.elements.io.FileExtensionFilter#FileExtensionFilter(String...)
   */
  public TextFileExtensionsFilter() {
    super(TEXT_FILE_EXTENSIONS);
  }
}
