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
import org.cp.elements.test.annotation.SubjectUnderTest;

/**
 * Unit Tests for {@link SourceCodeFileExtensionsFilter}.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.io.support.AbstractFileExtensionFilterTests
 * @see org.cp.elements.io.support.SourceCodeFileExtensionsFilter
 * @since 1.0.0
 */
public class SourceCodeFileExtensionsFilterUnitTests extends AbstractFileExtensionFilterTests {

  @SubjectUnderTest
  private final SourceCodeFileExtensionsFilter fileExtensionFilter = new SourceCodeFileExtensionsFilter();

  @Override
  protected String[] expectedFileExtensions() {
    return SourceCodeFileExtensionsFilter.SOURCE_CODE_FILE_EXTENSIONS;
  }

  @Override
  protected FileExtensionFilter fileExtensionFilter() {
    return this.fileExtensionFilter;
  }

  @Override
  protected String[] unexpectedFileExtensions() {

    return new String[] {
      "/path/to/a/file.binary",
      "absolute/path/to/a/file.exe",
      "/path/to/a.out",
      "/path/to/a/file.ext",
      "absolute/path/to/a/file"
    };
  }
}
