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
package org.cp.elements.io;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.File;

import org.cp.elements.test.AbstractBaseTestSuite;
import org.junit.Test;

/**
 * Unit Tests for {@link FilesOnlyFilter}.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see org.junit.Test
 * @see org.cp.elements.io.FilesOnlyFilter
 * @see org.cp.elements.test.AbstractBaseTestSuite
 * @since 1.0.0
 */
public class FilesOnlyFilterTests extends AbstractBaseTestSuite {

  private FilesOnlyFilter filesOnlyFilter = FilesOnlyFilter.INSTANCE;

  private File newFile(File parent, String pathname) {
    return new File(parent, pathname);
  }

  @Test
  public void acceptsFile() {

    File filesOnlyFilterClass = getLocation(FilesOnlyFilter.class);

    assertThat(filesOnlyFilterClass).isNotNull();
    assertThat(filesOnlyFilterClass.isFile()).isTrue();
    assertThat(filesOnlyFilter.accept(filesOnlyFilterClass)).isTrue();
  }

  @Test
  public void rejectsDirectories() {

    assertThat(filesOnlyFilter.accept(TEMPORARY_DIRECTORY)).isFalse();
    assertThat(filesOnlyFilter.accept(USER_HOME)).isFalse();
    assertThat(filesOnlyFilter.accept(WORKING_DIRECTORY)).isFalse();
  }

  @Test
  public void rejectsNonExistingDirectory() {

    File nonExistingDirectory = newFile(USER_HOME, "relative/path/to/non/existing/directory/");

    assertThat(nonExistingDirectory).isNotNull();
    assertThat(nonExistingDirectory.exists()).isFalse();
    assertThat(filesOnlyFilter.accept(nonExistingDirectory)).isFalse();
  }

  @Test
  public void rejectsNonExistingFile() {

    File nonExistingFile = newFile(WORKING_DIRECTORY, "relative/path/to/non/existing/file.ext");

    assertThat(nonExistingFile).isNotNull();
    assertThat(nonExistingFile.exists()).isFalse();
    assertThat(filesOnlyFilter.accept(nonExistingFile)).isFalse();
  }

  @Test
  public void rejectsNull() {
    assertThat(filesOnlyFilter.accept(null)).isFalse();
  }
}
