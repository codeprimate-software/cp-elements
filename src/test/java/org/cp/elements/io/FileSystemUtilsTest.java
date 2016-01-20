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

package org.cp.elements.io;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import static org.junit.Assume.assumeThat;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;

import org.cp.elements.lang.Constants;
import org.cp.elements.lang.SystemUtils;
import org.cp.elements.test.AbstractBaseTestSuite;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

/**
 * The FileSystemUtilsTest class is a test suite of test cases testing the contract and functionality
 * of the FileSystemUtilsTest class.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.runners.MockitoJUnitRunner
 * @see org.cp.elements.io.FileSystemUtils
 * @see org.cp.elements.test.AbstractBaseTestSuite
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class FileSystemUtilsTest extends AbstractBaseTestSuite {

  @Rule
  public ExpectedException expectedException = ExpectedException.none();

  @Mock
  private File mockFile;

  @Test
  public void appendToPathWithNonNullBasePathnameAndNonEmptyPathElements() {
    assertThat(FileSystemUtils.appendToPath("/absolute/path/to", "a", "relative", "location", "containing", "file.ext"),
      is(equalTo("/absolute/path/to/a/relative/location/containing/file.ext")));
  }

  @Test
  public void appendToPathWithNonNullBasePathnameAndEmptyPathElements() {
    assertThat(FileSystemUtils.appendToPath("/absolute/path/to/nothing"), is(equalTo("/absolute/path/to/nothing")));
  }

  @Test
  public void appendToPathWithBasePathnameAndPathElementsContainingFileSeparators() {
    assumeThat(SystemUtils.isUnixBasedOperatingSystem(), is(equalTo(true)));
    assertThat(FileSystemUtils.appendToPath("  ///base/pathname//////to// ", "/some////   ", " relative/",
        "     /location// ", " containing ", "//file.ext"),
      is(equalTo("/base/pathname/to/some/relative/location/containing/file.ext")));
  }

  @Test
  public void appendToPathWithBasePathnameAndEmptyBlankPathElements() {
    assertThat(FileSystemUtils.appendToPath("/absolute/path/to", "", "  ", "null"),
      is(equalTo("/absolute/path/to/null")));
  }

  @Test
  public void appendToPathWithNullBasePathname() {
    expectedException.expect(NullPointerException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("basePathname cannot be null");

    FileSystemUtils.appendToPath(null, "relative", "path", "to", "file.ext");
  }

  @Test
  public void appendToPathWithEmptyBasePathnameAndNonEmptyPathElements() {
    assertThat(FileSystemUtils.appendToPath("", "relative", "path", "to", "file.ext"),
      is(equalTo("/relative/path/to/file.ext")));
  }

  @Test
  public void appendToPathWithBlankBasePathNameAndNullPathElements() {
    assertThat(FileSystemUtils.appendToPath("  ", (String[]) null), is(equalTo("  ")));
  }

  @Test
  public void createPathFromNonNullNonEmptyPathElements() {
    assertThat(FileSystemUtils.createPath("absolute", "path", "to", "a", "file.ext"),
      is(equalTo("/absolute/path/to/a/file.ext")));
    assertThat(FileSystemUtils.createPath("relative", "path", "to", "a", "file.ext"),
      is(equalTo("/relative/path/to/a/file.ext")));
  }

  @Test
  public void createPathFromNonNullEmptyPathElements() {
    assertThat(FileSystemUtils.createPath("", "null", " ", "nil", "  "), is(equalTo("/null/nil")));
    assertThat(FileSystemUtils.createPath("", "  "), is(equalTo("")));
    assertThat(FileSystemUtils.createPath(), is(equalTo("")));
  }

  @Test
  public void createPathFromNullPathElements() {
    assertThat(FileSystemUtils.createPath((String[]) null), is(equalTo("")));
  }

  @Test
  public void createPathFromPathElementsContainingFileSeparators() {
    assumeThat(SystemUtils.isUnixBasedOperatingSystem(), is(true));
    assertThat(FileSystemUtils.createPath("  ///absolute/", " /path////   ", "to  ", "/file.ext "),
      is(equalTo("/absolute/path/to/file.ext")));
  }

  @Test
  public void count() {
    fail(Constants.NOT_IMPLEMENTED);
  }

  @Test
  public void deleteRecursive() {
    fail(Constants.NOT_IMPLEMENTED);
  }

  @Test
  @SuppressWarnings("all")
  public void isEmptyDirectoryWithNonEmptyDirectoryIsFalse() {
    when(mockFile.isDirectory()).thenReturn(true);
    when(mockFile.listFiles()).thenReturn(new File[1]);

    assertThat(FileSystemUtils.isEmptyDirectory(mockFile), is(false));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).listFiles();
  }

  @Test
  @SuppressWarnings("all")
  public void isEmptyDirectoryWithEmptyDirectoryIsTrue() {
    when(mockFile.isDirectory()).thenReturn(true);
    when(mockFile.listFiles()).thenReturn(FileSystemUtils.NO_FILES);

    assertThat(FileSystemUtils.isEmptyDirectory(mockFile), is(true));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).listFiles();
  }

  @Test
  @SuppressWarnings("all")
  public void isEmptyDirectoryWithFileIsFalse() {
    when(mockFile.isDirectory()).thenReturn(false);
    assertThat(FileSystemUtils.isEmptyDirectory(mockFile), is(false));
    verify(mockFile, times(1)).isDirectory();
  }

  @Test
  public void isEmptyDirectoryWithNullIsFalse() {
    assertThat(FileSystemUtils.isEmptyDirectory(null), is(false));
  }

  @Test
  public void isRelativeToWorkingDirectoryWithWorkingDirectoryIsTrue() {
    assertThat(FileSystemUtils.isRelativeToWorkingDirectory(WORKING_DIRECTORY), is(true));
  }

  @Test
  public void isRelativeToWorkingDirectoryWithRelativeFileIsTrue() {
    File relativeFile = new File(WORKING_DIRECTORY, "/relative/path/to/file.ext");
    assertThat(FileSystemUtils.isRelativeToWorkingDirectory(relativeFile), is(true));
  }

  @Test
  public void isRelativeToWorkingDirectoryWithNonRelativeFileIsFalse() {
    File nonRelativeFile = new File("/non/relative/path/to/working/directory");
    assertThat(FileSystemUtils.isRelativeToWorkingDirectory(nonRelativeFile), is(false));
  }

  @Test
  public void isRelativeToWorkingDirectoryWithParentOfWorkingDirectoryIsFalse() {
    assertThat(FileSystemUtils.isRelativeToWorkingDirectory(WORKING_DIRECTORY.getParentFile()), is(false));
  }

  @Test
  public void isRelativeToWorkingDirectoryWithNullIsFalse() {
    assertThat(FileSystemUtils.isRelativeToWorkingDirectory(null), is(false));
  }

}
