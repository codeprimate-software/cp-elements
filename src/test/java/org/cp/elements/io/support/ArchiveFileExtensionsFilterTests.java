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
 * Test suite of test cases testing the contract and functionality of the {@link ArchiveFileExtensionsFilter} class.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see org.junit.Test
 * @see org.cp.elements.io.support.AbstractFileExtensionFilterTests
 * @see org.cp.elements.io.support.ArchiveFileExtensionsFilter
 * @since 1.0.0
 */
public class ArchiveFileExtensionsFilterTests extends AbstractFileExtensionFilterTests {

  private ArchiveFileExtensionsFilter fileExtensionFilter = new ArchiveFileExtensionsFilter();

  @Override
  protected String[] expectedFileExtensions() {
    return ArchiveFileExtensionsFilter.ARCHIVE_FILE_EXTENSIONS;
  }

  @Override
  protected FileExtensionFilter fileExtensionFilter() {
    return fileExtensionFilter;
  }

  @Override
  protected String[] unexpectedFileExtensions() {
    return new String[] {
      "/path/to/a/file/archive.zip",
      "absolute/path/to/a/file/archive.war",
      "/path/to/a/file/archive.jar",
      "/path/to/a/file/archive.jar",
      "/path/to/a/source.java",
      "absolute/path/to/a/binary.class",
      "/path/to/a/file.ext",
      "absolute/path/to/a/file"
    };
  }
}
