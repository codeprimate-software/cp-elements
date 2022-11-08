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
 * {@link FileExtensionFilter} implementation that filters {@link File Files} by data file types.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see org.cp.elements.io.FileExtensionFilter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class DataFileExtensionsFilter extends FileExtensionFilter {

  protected static final String[] DATA_FILE_EXTENSIONS = {
    "csv",
    "dat",
    "data",
    "db",
    "dbf",
    "json",
    "mdb",
    "mf",
    "tab",
    "xls",
    "xml"
  };

  /**
   * Constructs a new instance of {@link DataFileExtensionsFilter} to filter {@link File Files}
   * by data file types.
   *
   * @see org.cp.elements.io.FileExtensionFilter#FileExtensionFilter(String...)
   */
  public DataFileExtensionsFilter() {
    super(DATA_FILE_EXTENSIONS);
  }
}
