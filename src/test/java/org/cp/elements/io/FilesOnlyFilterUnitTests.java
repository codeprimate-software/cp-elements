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
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.withSettings;

import java.io.File;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.test.AbstractBaseTestSuite;
import org.cp.elements.test.annotation.IntegrationTest;
import org.cp.elements.test.annotation.SubjectUnderTest;

import org.mockito.quality.Strictness;

/**
 * Unit Tests for {@link FilesOnlyFilter}.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.io.FilesOnlyFilter
 * @see org.cp.elements.test.AbstractBaseTestSuite
 * @since 1.0.0
 */
public class FilesOnlyFilterUnitTests extends AbstractBaseTestSuite {

  @SubjectUnderTest
  private final FilesOnlyFilter filesOnlyFilter = FilesOnlyFilter.INSTANCE;

  private @NotNull File newFile(@Nullable File parent, @NotNull String pathname) {
    return new File(parent, pathname);
  }

  @Test
  public void acceptsDirectoryReturnsFalse() {

    File mockFile = mock(File.class, withSettings().strictness(Strictness.LENIENT));

    doReturn(true).when(mockFile).isDirectory();
    doReturn(false).when(mockFile).isFile();
    doReturn(true).when(mockFile).exists();

    assertThat(this.filesOnlyFilter.accept(mockFile)).isFalse();

    verify(mockFile, times(1)).isFile();
    verify(mockFile, never()).isDirectory();
    verify(mockFile, never()).exists();
    verifyNoMoreInteractions(mockFile);
  }

  @Test
  public void acceptsFileReturnsTrue() {

    File mockFile = mock(File.class, withSettings().strictness(Strictness.LENIENT));

    doReturn(false).when(mockFile).isDirectory();
    doReturn(true).when(mockFile).isFile();
    doReturn(false).when(mockFile).exists();

    assertThat(this.filesOnlyFilter.accept(mockFile)).isTrue();

    verify(mockFile, times(1)).isFile();
    verify(mockFile, never()).isDirectory();
    verify(mockFile, never()).exists();
    verifyNoMoreInteractions(mockFile);
  }

  @Test
  public void acceptsNonFileReturnsFalse() {

    File mockFile = mock(File.class, withSettings().strictness(Strictness.LENIENT));

    doReturn(false).when(mockFile).isDirectory();
    doReturn(false).when(mockFile).isFile();
    doReturn(true).when(mockFile).exists();

    assertThat(this.filesOnlyFilter.accept(mockFile)).isFalse();

    verify(mockFile, times(1)).isFile();
    verify(mockFile, never()).isDirectory();
    verify(mockFile, never()).exists();
    verifyNoMoreInteractions(mockFile);
  }

  @Test
  public void acceptsNullIsNullSafeReturnsFalse() {
    assertThat(this.filesOnlyFilter.accept(null)).isFalse();
  }

  @Test
  @IntegrationTest
  public void acceptsRealFileReturnsTrue() {

    File filesOnlyFilterClass = getLocation(FilesOnlyFilter.class);

    assertThat(filesOnlyFilterClass).isNotNull();
    assertThat(filesOnlyFilterClass).isFile();
    assertThat(this.filesOnlyFilter.accept(filesOnlyFilterClass)).isTrue();
  }

  @Test
  @IntegrationTest
  public void rejectsDirectories() {

    assertThat(this.filesOnlyFilter.accept(TEMPORARY_DIRECTORY)).isFalse();
    assertThat(this.filesOnlyFilter.accept(USER_HOME)).isFalse();
    assertThat(this.filesOnlyFilter.accept(WORKING_DIRECTORY)).isFalse();
  }

  @Test
  public void rejectsNonExistingDirectory() {

    File nonExistingDirectory = newFile(USER_HOME, "relative/path/to/non/existing/directory/");

    assertThat(nonExistingDirectory).isNotNull();
    assertThat(nonExistingDirectory).doesNotExist();
    assertThat(this.filesOnlyFilter.accept(nonExistingDirectory)).isFalse();
  }

  @Test
  public void rejectsNonExistingFile() {

    File nonExistingFile = newFile(WORKING_DIRECTORY, "relative/path/to/non/existing/file.ext");

    assertThat(nonExistingFile).isNotNull();
    assertThat(nonExistingFile).doesNotExist();
    assertThat(this.filesOnlyFilter.accept(nonExistingFile)).isFalse();
  }
}
