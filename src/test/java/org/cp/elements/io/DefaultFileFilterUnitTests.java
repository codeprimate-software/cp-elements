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
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.withSettings;

import java.io.File;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.test.AbstractBaseTestSuite;

import org.mockito.quality.Strictness;

/**
 * Unit Tests for {@link DefaultFileFilter}.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.io.DefaultFileFilter
 * @see org.cp.elements.test.AbstractBaseTestSuite
 * @since 1.0.0
 */
public class DefaultFileFilterUnitTests extends AbstractBaseTestSuite {

  private @NotNull File newFile(@NotNull String pathname) {
    return new File(pathname);
  }

  @Test
  public void getInstanceWithFalseReturnsDefaultReject() {
    assertThat(DefaultFileFilter.getInstance(false)).isSameAs(DefaultFileFilter.DEFAULT_REJECT);
  }

  @Test
  public void getInstanceWithTrueReturnsDefaultAccept() {
    assertThat(DefaultFileFilter.getInstance(true)).isSameAs(DefaultFileFilter.DEFAULT_ACCEPT);
  }

  @Test
  public void acceptsAllFiles() {

    assertThat(DefaultFileFilter.DEFAULT_ACCEPT.accept(WORKING_DIRECTORY)).isTrue();
    assertThat(DefaultFileFilter.DEFAULT_ACCEPT.accept(USER_HOME)).isTrue();
    assertThat(DefaultFileFilter.DEFAULT_ACCEPT.accept(TEMPORARY_DIRECTORY)).isTrue();
    assertThat(DefaultFileFilter.DEFAULT_ACCEPT.accept(getLocation(DefaultFileFilter.class))).isTrue();
    assertThat(DefaultFileFilter.DEFAULT_ACCEPT.accept(newFile("/absolute/path/to/non/existing/directory"))).isTrue();
    assertThat(DefaultFileFilter.DEFAULT_ACCEPT.accept(newFile("relative/path/to/non/existing/file.ext"))).isTrue();
  }

  @Test
  public void acceptsMockFileReturnsTrue() {

    File mockFile = mock(File.class, withSettings().strictness(Strictness.LENIENT));

    doReturn(false).when(mockFile).isDirectory();
    doReturn(false).when(mockFile).isFile();
    doReturn(false).when(mockFile).exists();

    assertThat(DefaultFileFilter.DEFAULT_ACCEPT.accept(mockFile)).isTrue();

    verifyNoInteractions(mockFile);
  }

  @Test
  public void acceptsNullIsNullSafeReturnsTrue() {
    assertThat(DefaultFileFilter.DEFAULT_ACCEPT.accept(null)).isTrue();
  }

  @Test
  public void rejectsAllFiles() {

    assertThat(DefaultFileFilter.DEFAULT_REJECT.accept(WORKING_DIRECTORY)).isFalse();
    assertThat(DefaultFileFilter.DEFAULT_REJECT.accept(USER_HOME)).isFalse();
    assertThat(DefaultFileFilter.DEFAULT_REJECT.accept(TEMPORARY_DIRECTORY)).isFalse();
    assertThat(DefaultFileFilter.DEFAULT_REJECT.accept(getLocation(DefaultFileFilter.class))).isFalse();
    assertThat(DefaultFileFilter.DEFAULT_REJECT.accept(newFile("/absolute/path/to/non/existing/directory"))).isFalse();
    assertThat(DefaultFileFilter.DEFAULT_REJECT.accept(newFile("relative/path/to/non/existing/file.ext"))).isFalse();
  }

  @Test
  public void rejectsMockFileReturnsFalse() {

    File mockFile = mock(File.class, withSettings().strictness(Strictness.LENIENT));

    doReturn(false).when(mockFile).isDirectory();
    doReturn(true).when(mockFile).isFile();
    doReturn(true).when(mockFile).exists();

    assertThat(DefaultFileFilter.DEFAULT_REJECT.accept(mockFile)).isFalse();

    verifyNoInteractions(mockFile);
  }

  @Test
  public void rejectsNullIsNullSafeReturnsFalse() {
    assertThat(DefaultFileFilter.DEFAULT_REJECT.accept(null)).isFalse();
  }
}
