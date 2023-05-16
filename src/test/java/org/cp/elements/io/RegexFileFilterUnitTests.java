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
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.cp.elements.lang.CheckedExceptionsFactory.newIOException;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.regex.Pattern;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.annotation.NotNull;

/**
 * Unit Tests for {@link RegexFileFilter}.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.io.RegexFileFilter
 * @since 1.0.0
 */
public class RegexFileFilterUnitTests {

  private @NotNull File mockFile(@NotNull String name) {
    return mock(File.class, name);
  }

  @Test
  public void constructRegexFileFilterWithRegularExpressionPattern() {

    RegexFileFilter fileFilter = new RegexFileFilter("[a-zA-Z0-9_*/]+[a-zA-Z0-9_]+\\.dat");

    assertThat(fileFilter).isNotNull();
    assertThat(fileFilter.getPattern()).isNotNull();
    assertThat(fileFilter.getRegularExpression()).isEqualTo("[a-zA-Z0-9_*/]+[a-zA-Z0-9_]+\\.dat");
  }

  @Test
  public void constructRegexFileFilterWithPattern() {

    Pattern expectedPattern = Pattern.compile("[a-zA-Z0-9_]+");

    RegexFileFilter fileFilter = new RegexFileFilter(expectedPattern);

    assertThat(fileFilter).isNotNull();
    assertThat(fileFilter.getPattern()).isSameAs(expectedPattern);
    assertThat(fileFilter.getRegularExpression()).isEqualTo(expectedPattern.pattern());
  }

  @Test
  public void constructRegexFileFilterWithIllegalRegularExpressionPatterns() {

    Arrays.asList("  ", "", null).forEach(illegalPattern ->
      assertThatIllegalArgumentException()
        .isThrownBy(() -> new RegexFileFilter(illegalPattern))
        .withMessage("Regular Expression Pattern [%s] is required", illegalPattern)
        .withNoCause());
  }

  @Test
  public void constructRegexFileFilterWithNullPatternObject() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new RegexFileFilter((Pattern) null))
      .withMessage("Regular Expression Pattern is required")
      .withNoCause();
  }

  @Test
  public void acceptsMatchingFiles() throws IOException {

    File mockFileOne = mockFile("MockFileOne");
    File mockFileTwo = mockFile("MockFileTwo");
    File mockFileThree = mockFile("MockFileThree");

    doReturn("./db.dat").when(mockFileOne).getCanonicalPath();
    doReturn("/absolute/path/to/db.dat").when(mockFileTwo).getCanonicalPath();
    doThrow(newIOException("test")).when(mockFileThree).getCanonicalPath();
    doReturn("relative/path/to/db.dat").when(mockFileThree).getAbsolutePath();

    RegexFileFilter fileFilter = new RegexFileFilter("[a-zA-Z0-9_/\\.]+.*\\.dat");

    assertThat(fileFilter.accept(mockFileOne)).isTrue();
    assertThat(fileFilter.accept(mockFileTwo)).isTrue();
    assertThat(fileFilter.accept(mockFileThree)).isTrue();

    verify(mockFileOne, times(1)).getCanonicalPath();
    verify(mockFileTwo, times(1)).getCanonicalPath();
    verify(mockFileThree, times(1)).getCanonicalPath();
    verify(mockFileThree, times(1)).getAbsolutePath();
    verifyNoMoreInteractions(mockFileOne, mockFileTwo, mockFileThree);
  }

  @Test
  public void rejectsNonMatchingFiles() throws IOException {

    File mockFile= mockFile("MockFile");

    doReturn("/path/to/some/junk.data").when(mockFile).getCanonicalPath();

    RegexFileFilter fileFilter = new RegexFileFilter("[.*/]+.*\\.dat");

    assertThat(fileFilter.accept(mockFile)).isFalse();

    verify(mockFile, times(1)).getCanonicalPath();
    verifyNoMoreInteractions(mockFile);
  }

  @Test
  public void rejectsNullFilesIsNullSafe() {
    assertThat(new RegexFileFilter(".").accept(null)).isFalse();
  }
}
