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
 * The CompressionOnlyFileExtensionFilter class is a {@link FileExtensionFilter} implementation
 * that filters files by compression file types.
 *
 * @author John J. Blum
 * @see org.cp.elements.io.FileExtensionFilter
 * @see <a href="http://en.wikipedia.org/wiki/List_of_archive_formats">List of archive formats</a>
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class CompressionOnlyFileExtensionFilter extends FileExtensionFilter {

  protected static final String[] COMPRESSION_ONLY_FILE_EXTENSIONS = {
    "bz",
    "bz2",
    "bzip",
    "bzip2",
    "gz",
    "gzip",
    "lz",
    "lzma",
    "lzo",
    "rz",
    "rzip",
    "z"
  };

  public CompressionOnlyFileExtensionFilter() {
    super(COMPRESSION_ONLY_FILE_EXTENSIONS);
  }

}
