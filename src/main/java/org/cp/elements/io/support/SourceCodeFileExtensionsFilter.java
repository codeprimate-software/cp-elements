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
 * {@link FileExtensionFilter} implementation that filters {@link File Files} by source code file types.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see org.cp.elements.io.FileExtensionFilter
 * @see <a href="http://www.file-extensions.org/filetype/extension/name/source-code-and-script-files">file-extensions.org</a>
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class SourceCodeFileExtensionsFilter extends FileExtensionFilter {

  static final String[] SOURCE_CODE_FILE_EXTENSIONS = {
    "ada",
    "asp",
    "bin",
    "c",
    "cpp",
    "csharp",
    "ddl",
    "dml",
    "dtd",
    "groovy",
    "htm",
    "html",
    "html5",
    "java",
    "js",
    "json",
    "jsp",
    "kt",
    "perl",
    "phl",
    "php2",
    "rb",
    "rpy",
    "sql",
    "tcl",
    "vb",
    "vbscript",
    "xml",
    "xsd",
  };

  /**
   * Constructs a new {@link SourceCodeFileExtensionsFilter} to filter {@link File Files} by source code file types.
   *
   * @see org.cp.elements.io.FileExtensionFilter#FileExtensionFilter(String...)
   */
  public SourceCodeFileExtensionsFilter() {
    super(SOURCE_CODE_FILE_EXTENSIONS);
  }
}
