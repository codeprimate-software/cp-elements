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

import static org.assertj.core.api.Assertions.assertThat;

import java.io.File;
import java.util.Set;

import org.junit.Test;

import org.cp.elements.io.FileExtensionFilter;
import org.cp.elements.lang.annotation.NotNull;

/**
 * Abstract base class containing test case methods common to all {@link FileExtensionFilter} tests
 * in the {@link org.cp.elements.io.support} package.
 *
 * @author John Blum
 * @see java.io.File
 * @see org.junit.Test
 * @see org.cp.elements.io.FileExtensionFilter
 * @since 1.0.0
 */
public abstract class AbstractFileExtensionFilterTests {

  protected abstract String[] expectedFileExtensions();

  protected int expectedSize() {
    return expectedFileExtensions().length;
  }

  protected abstract FileExtensionFilter fileExtensionFilter();

  protected @NotNull File newFile(@NotNull String pathname) {
    return new File(pathname);
  }

  protected abstract String[] unexpectedFileExtensions();

  @Test
  public void acceptsExpectedFileExtensions() {

    Set<String> fileExtensions = fileExtensionFilter().getFileExtensions();

    assertThat(fileExtensions).isNotNull();
    assertThat(fileExtensions).hasSize(expectedSize());
    assertThat(fileExtensions).containsExactlyInAnyOrder(expectedFileExtensions());

    for (String fileExtension : fileExtensions) {
      assertThat(fileExtensionFilter().accept(newFile(String.format("file.%s", fileExtension)))).isTrue();
    }
  }

  @Test
  public void acceptWithNullIsNullSafeReturnsFalse() {
    assertThat(fileExtensionFilter().accept(null)).isFalse();
  }

  @Test
  public void rejectsUnexpectedFileExtensions() {

    for (String fileExtension : unexpectedFileExtensions()) {
      assertThat(fileExtensionFilter().accept(newFile(fileExtension))).isFalse();
    }
  }
}
