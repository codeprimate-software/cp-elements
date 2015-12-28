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
 * The SourceCodeFileExtensionFilter is a FileExtensionFilter implementation...
 *
 * @author John J. Blum
 * @see org.cp.elements.io.FileExtensionFilter
 * @link http://www.file-extensions.org/filetype/extension/name/source-code-and-script-files
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class SourceCodeFileExtensionFilter extends FileExtensionFilter {

  protected static final String[] SOURCE_CODE_FILE_EXTENSIONS = {
    "ada",
    "bin",
    "c",
    "cpp",
    "csharp",
    "ddl",
    "dtd",
    "htm",
    "html",
    "html5",
    "java",
    "js",
    "json",
    "jsp",
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

  public SourceCodeFileExtensionFilter() {
    super(SOURCE_CODE_FILE_EXTENSIONS);
  }

}
