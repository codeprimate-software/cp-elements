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
import static org.hamcrest.Matchers.greaterThanOrEqualTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import static org.junit.Assume.assumeThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.isA;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.FileFilter;

import org.cp.elements.lang.SystemUtils;
import org.cp.elements.test.AbstractBaseTestSuite;
import org.cp.elements.test.annotation.IntegrationTest;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.runners.MockitoJUnitRunner;
import org.mockito.stubbing.Answer;

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

  @Mock
  private FileFilter mockFileFilter;

  protected File[] asArray(final File... files) {
    return files;
  }

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
    assertThat(FileSystemUtils.createPath(null, "", "  "), is(equalTo("")));
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
  @SuppressWarnings("all")
  public void countFilteredDirectoryIsAccepted() {
    when(mockFile.isDirectory()).thenReturn(true);
    when(mockFile.isFile()).thenReturn(false);
    when(mockFile.listFiles(any(FileFilter.class))).thenReturn(FileSystemUtils.NO_FILES);
    when(mockFileFilter.accept(any(File.class))).thenReturn(true);

    assertThat(FileSystemUtils.count(mockFile, mockFileFilter), is(equalTo(1)));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, never()).isFile();
    verify(mockFile, times(1)).listFiles(isA(AcceptingAllNonNullFilesFilter.class));
    verify(mockFileFilter, times(1)).accept(eq(mockFile));
  }

  @Test
  @SuppressWarnings("all")
  public void countFilteredDirectoryIsRejected() {
    when(mockFile.isDirectory()).thenReturn(true);
    when(mockFile.isFile()).thenReturn(false);
    when(mockFile.listFiles(any(FileFilter.class))).thenReturn(FileSystemUtils.NO_FILES);
    when(mockFileFilter.accept(any(File.class))).thenReturn(false);

    assertThat(FileSystemUtils.count(mockFile, mockFileFilter), is(equalTo(0)));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, never()).isFile();
    verify(mockFile, times(1)).listFiles(isA(AcceptingAllNonNullFilesFilter.class));
    verify(mockFileFilter, times(1)).accept(eq(mockFile));
  }

  @Test
  @SuppressWarnings("all")
  public void countFilteredFileIsAccepted() {
    when(mockFile.isDirectory()).thenReturn(false);
    when(mockFile.isFile()).thenReturn(true);
    when(mockFileFilter.accept(eq(mockFile))).thenReturn(true);

    assertThat(FileSystemUtils.count(mockFile, mockFileFilter), is(equalTo(1)));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, never()).isFile();
    verify(mockFile, never()).listFiles(any(FileFilter.class));
    verify(mockFileFilter, times(1)).accept(eq(mockFile));
  }

  @Test
  @SuppressWarnings("all")
  public void countFilteredFileIsRejected() {
    when(mockFile.isDirectory()).thenReturn(false);
    when(mockFile.isFile()).thenReturn(true);
    when(mockFileFilter.accept(any(File.class))).thenReturn(false);

    assertThat(FileSystemUtils.count(mockFile, mockFileFilter), is(equalTo(0)));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, never()).isFile();
    verify(mockFile, never()).listFiles(any(FileFilter.class));
    verify(mockFileFilter, times(1)).accept(eq(mockFile));
  }

  @Test
  @SuppressWarnings("all")
  public void countNonFilteredFileIsAccepted() {
    when(mockFile.isDirectory()).thenReturn(false);
    when(mockFile.isFile()).thenReturn(true);
    assertThat(FileSystemUtils.count(mockFile), is(equalTo(1)));
    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).isFile();
    verify(mockFile, never()).listFiles(any(FileFilter.class));
  }

  @Test
  @SuppressWarnings("all")
  public void countNonFilteredDirectoryIsRejected() {
    when(mockFile.isDirectory()).thenReturn(true);
    when(mockFile.isFile()).thenReturn(false);
    when(mockFile.listFiles(any(FileFilter.class))).thenReturn(FileSystemUtils.NO_FILES);
    assertThat(FileSystemUtils.count(mockFile), is(equalTo(0)));
    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).isFile();
    verify(mockFile, times(1)).listFiles(isA(AcceptingAllNonNullFilesFilter.class));
  }

  @Test
  public void countNullFileIsRejected() {
    assertThat(FileSystemUtils.count(null), is(equalTo(0)));
  }

  @Test
  @SuppressWarnings("all")
  public void countNonFilteredNonEmptyDirectory() {
    File mockSubDirectoryOne = mock(File.class, "MockSubDirectoryOne");
    File mockSubDirectoryTwo = mock(File.class, "MockSubDirectoryTwo");
    File mockFileOne = mock(File.class, "MockFileOne");
    File mockFileTwo = mock(File.class, "MockFileTwo");
    File mockFileThree = mock(File.class, "MockFileThree");
    File mockFileFour = mock(File.class, "MockFileFour");

    when(mockFile.isDirectory()).thenReturn(true);
    when(mockFile.isFile()).thenReturn(false);
    when(mockFile.listFiles(any(FileFilter.class))).thenReturn(asArray(mockSubDirectoryOne, mockSubDirectoryTwo,
      mockFileOne, mockFileTwo, mockFileThree));
    when(mockSubDirectoryOne.isDirectory()).thenReturn(true);
    when(mockSubDirectoryOne.isFile()).thenReturn(false);
    when(mockSubDirectoryOne.listFiles(any(FileFilter.class))).thenReturn(asArray(mockFileFour));
    when(mockSubDirectoryTwo.isDirectory()).thenReturn(true);
    when(mockSubDirectoryTwo.isFile()).thenReturn(false);
    when(mockSubDirectoryTwo.listFiles(any(FileFilter.class))).thenReturn(FileSystemUtils.NO_FILES);
    when(mockFileOne.isDirectory()).thenReturn(false);
    when(mockFileOne.isFile()).thenReturn(true);
    when(mockFileTwo.isDirectory()).thenReturn(false);
    when(mockFileTwo.isFile()).thenReturn(true);
    when(mockFileThree.isDirectory()).thenReturn(false);
    when(mockFileThree.isFile()).thenReturn(true);
    when(mockFileFour.isDirectory()).thenReturn(false);
    when(mockFileFour.isFile()).thenReturn(true);

    assertThat(FileSystemUtils.count(mockFile), is(equalTo(4)));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).isFile();
    verify(mockFile, times(1)).listFiles(isA(AcceptingAllNonNullFilesFilter.class));
    verify(mockSubDirectoryOne, times(2)).isDirectory();
    verify(mockSubDirectoryOne, times(1)).isFile();
    verify(mockSubDirectoryOne, times(1)).listFiles(isA(AcceptingAllNonNullFilesFilter.class));
    verify(mockSubDirectoryTwo, times(2)).isDirectory();
    verify(mockSubDirectoryTwo, times(1)).isFile();
    verify(mockSubDirectoryTwo, times(1)).listFiles(isA(AcceptingAllNonNullFilesFilter.class));
    verify(mockFileOne, times(1)).isDirectory();
    verify(mockFileOne, times(1)).isFile();
    verify(mockFileOne, never()).listFiles(any(FileFilter.class));
    verify(mockFileTwo, times(1)).isDirectory();
    verify(mockFileTwo, times(1)).isFile();
    verify(mockFileTwo, never()).listFiles(any(FileFilter.class));
    verify(mockFileThree, times(1)).isDirectory();
    verify(mockFileThree, times(1)).isFile();
    verify(mockFileThree, never()).listFiles(any(FileFilter.class));
    verify(mockFileFour, times(1)).isDirectory();
    verify(mockFileFour, times(1)).isFile();
    verify(mockFileFour, never()).listFiles(any(FileFilter.class));
  }

  @Test
  @SuppressWarnings("all")
  public void countFilteredNonEmptyDirectory() {
    File mockSubDirectoryOne = mock(File.class, "MockSubDirectoryOne");
    File mockSubDirectoryTwo = mock(File.class, "MockSubDirectoryTwo");
    File mockSubDirectoryThree = mock(File.class, "MockSubDirectoryThree");
    File mockFileOne = mock(File.class, "MockFileOne");
    File mockFileTwo = mock(File.class, "MockFileTwo");
    File mockFileThree = mock(File.class, "MockFileThree");
    File mockFileFour = mock(File.class, "MockFileFour");
    File mockFileFive = mock(File.class, "MockFileFive");
    File mockFileSix = mock(File.class, "MockFileFive");

    when(mockFile.isDirectory()).thenReturn(true);
    when(mockFile.isFile()).thenReturn(false);
    when(mockFile.listFiles(any(FileFilter.class))).thenReturn(asArray(mockSubDirectoryOne, mockSubDirectoryTwo,
      mockFileOne, mockFileTwo, mockFileThree));
    when(mockSubDirectoryOne.isDirectory()).thenReturn(true);
    when(mockSubDirectoryOne.isFile()).thenReturn(false);
    when(mockSubDirectoryOne.listFiles(any(FileFilter.class))).thenReturn(asArray(mockFileFour, mockFileFive));
    when(mockSubDirectoryTwo.isDirectory()).thenReturn(true);
    when(mockSubDirectoryTwo.isFile()).thenReturn(false);
    when(mockSubDirectoryTwo.listFiles(any(FileFilter.class))).thenReturn(asArray(mockSubDirectoryThree, mockFileSix));
    when(mockSubDirectoryThree.isDirectory()).thenReturn(true);
    when(mockSubDirectoryThree.isFile()).thenReturn(false);
    when(mockSubDirectoryThree.listFiles(any(FileFilter.class))).thenReturn(asArray());
    when(mockFileOne.isDirectory()).thenReturn(false);
    when(mockFileOne.isFile()).thenReturn(true);
    when(mockFileOne.isHidden()).thenReturn(false);
    when(mockFileTwo.isDirectory()).thenReturn(false);
    when(mockFileTwo.isFile()).thenReturn(true);
    when(mockFileTwo.isHidden()).thenReturn(true);
    when(mockFileThree.isDirectory()).thenReturn(false);
    when(mockFileThree.isFile()).thenReturn(true);
    when(mockFileThree.isHidden()).thenReturn(false);
    when(mockFileFour.isDirectory()).thenReturn(false);
    when(mockFileFour.isFile()).thenReturn(true);
    when(mockFileFour.isHidden()).thenReturn(true);
    when(mockFileFive.isDirectory()).thenReturn(false);
    when(mockFileFive.isFile()).thenReturn(true);
    when(mockFileFive.isHidden()).thenReturn(false);
    when(mockFileSix.isDirectory()).thenReturn(false);
    when(mockFileSix.isFile()).thenReturn(true);
    when(mockFileSix.isHidden()).thenReturn(true);

    when(mockFileFilter.accept(any(File.class))).thenAnswer(new Answer<Boolean>() {
      @Override public Boolean answer(final InvocationOnMock invocationOnMock) throws Throwable {
        File file = invocationOnMock.getArgumentAt(0, File.class);
        return !file.isHidden();
      }
    });

    assertThat(FileSystemUtils.count(mockFile, ComposableFileFilter.and(FileOnlyFilter.INSTANCE, mockFileFilter)),
      is(equalTo(3)));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).isFile();
    verify(mockFile, times(1)).isHidden();
    verify(mockFile, times(1)).listFiles(isA(AcceptingAllNonNullFilesFilter.class));
    verify(mockSubDirectoryOne, times(2)).isDirectory();
    verify(mockSubDirectoryOne, times(1)).isFile();
    verify(mockSubDirectoryOne, times(1)).isHidden();
    verify(mockSubDirectoryOne, times(1)).listFiles(isA(AcceptingAllNonNullFilesFilter.class));
    verify(mockSubDirectoryTwo, times(2)).isDirectory();
    verify(mockSubDirectoryTwo, times(1)).isFile();
    verify(mockSubDirectoryTwo, times(1)).isHidden();
    verify(mockSubDirectoryTwo, times(1)).listFiles(isA(AcceptingAllNonNullFilesFilter.class));
    verify(mockSubDirectoryThree, times(2)).isDirectory();
    verify(mockSubDirectoryThree, times(1)).isFile();
    verify(mockSubDirectoryThree, times(1)).isHidden();
    verify(mockSubDirectoryThree, times(1)).listFiles(isA(AcceptingAllNonNullFilesFilter.class));
    verify(mockFileOne, times(1)).isDirectory();
    verify(mockFileOne, times(1)).isFile();
    verify(mockFileOne, times(1)).isHidden();
    verify(mockFileOne, never()).listFiles(any(FileFilter.class));
    verify(mockFileTwo, times(1)).isDirectory();
    verify(mockFileTwo, times(1)).isFile();
    verify(mockFileTwo, times(1)).isHidden();
    verify(mockFileTwo, never()).listFiles(any(FileFilter.class));
    verify(mockFileThree, times(1)).isDirectory();
    verify(mockFileThree, times(1)).isFile();
    verify(mockFileThree, times(1)).isHidden();
    verify(mockFileThree, never()).listFiles(any(FileFilter.class));
    verify(mockFileFour, times(1)).isDirectory();
    verify(mockFileFour, times(1)).isFile();
    verify(mockFileFour, times(1)).isHidden();
    verify(mockFileFour, never()).listFiles(any(FileFilter.class));
    verify(mockFileFive, times(1)).isDirectory();
    verify(mockFileFive, times(1)).isFile();
    verify(mockFileFive, times(1)).isHidden();
    verify(mockFileFive, never()).listFiles(any(FileFilter.class));
    verify(mockFileSix, times(1)).isDirectory();
    verify(mockFileSix, times(1)).isFile();
    verify(mockFileSix, times(1)).isHidden();
    verify(mockFileSix, never()).listFiles(any(FileFilter.class));
  }

  @Test
  @IntegrationTest
  public void countJavaSourceFilesInProjectReturnsNonZeroCount() {
    assertThat(FileSystemUtils.count(getSourceDirectory(), new FileExtensionFilter("java")),
      is(greaterThanOrEqualTo(386)));
  }

  @Test
  @IntegrationTest
  public void countGroovySourceFilesInProjectReturnsZero() {
    assertThat(FileSystemUtils.count(getSourceDirectory(), new FileExtensionFilter("groovy")), is(equalTo(0)));
  }

  @Test
  @SuppressWarnings("all")
  public void deleteRecursiveWithExistingNonEmptyDirectoryIsTrue() {
    File mockSubDirectoryOne = mock(File.class, "MockSubDirectoryOne");
    File mockSubDirectoryTwo = mock(File.class, "MockSubDirectoryTwo");
    File mockFileOne = mock(File.class, "MockFileOne");
    File mockFileTwo = mock(File.class, "MockFileTwo");
    File mockFileThree = mock(File.class, "MockFileThree");

    when(mockFile.isDirectory()).thenReturn(true);
    when(mockFile.exists()).thenReturn(true);
    when(mockFile.listFiles(any(FileFilter.class))).thenReturn(asArray(mockSubDirectoryOne, mockSubDirectoryTwo,
      mockFileOne, mockFileTwo));
    when(mockFile.delete()).thenReturn(true);
    when(mockSubDirectoryOne.isDirectory()).thenReturn(true);
    when(mockSubDirectoryOne.exists()).thenReturn(true);
    when(mockSubDirectoryOne.listFiles(any(FileFilter.class))).thenReturn(asArray(mockFileThree));
    when(mockSubDirectoryOne.delete()).thenReturn(true);
    when(mockSubDirectoryTwo.isDirectory()).thenReturn(true);
    when(mockSubDirectoryTwo.exists()).thenReturn(true);
    when(mockSubDirectoryTwo.listFiles(any(FileFilter.class))).thenReturn(asArray());
    when(mockSubDirectoryTwo.delete()).thenReturn(true);
    when(mockFileOne.isDirectory()).thenReturn(false);
    when(mockFileOne.exists()).thenReturn(true);
    when(mockFileOne.delete()).thenReturn(true);
    when(mockFileTwo.isDirectory()).thenReturn(false);
    when(mockFileTwo.exists()).thenReturn(true);
    when(mockFileTwo.delete()).thenReturn(true);
    when(mockFileThree.isDirectory()).thenReturn(false);
    when(mockFileThree.exists()).thenReturn(true);
    when(mockFileThree.delete()).thenReturn(true);

    assertThat(FileSystemUtils.deleteRecursive(mockFile), is(true));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).exists();
    verify(mockFile, times(1)).listFiles(isA(AcceptingAllNonNullFilesFilter.class));
    verify(mockFile, times(1)).delete();
    verify(mockSubDirectoryOne, times(2)).isDirectory();
    verify(mockSubDirectoryOne, times(1)).exists();
    verify(mockSubDirectoryOne, times(1)).listFiles(isA(AcceptingAllNonNullFilesFilter.class));
    verify(mockSubDirectoryOne, times(1)).delete();
    verify(mockSubDirectoryTwo, times(2)).isDirectory();
    verify(mockSubDirectoryTwo, times(1)).exists();
    verify(mockSubDirectoryTwo, times(1)).listFiles(isA(AcceptingAllNonNullFilesFilter.class));
    verify(mockSubDirectoryTwo, times(1)).delete();
    verify(mockFileOne, times(1)).isDirectory();
    verify(mockFileOne, times(1)).exists();
    verify(mockFileOne, never()).listFiles(any(FileFilter.class));
    verify(mockFileOne, times(1)).delete();
    verify(mockFileTwo, times(1)).isDirectory();
    verify(mockFileTwo, times(1)).exists();
    verify(mockFileTwo, never()).listFiles(any(FileFilter.class));
    verify(mockFileTwo, times(1)).delete();
    verify(mockFileThree, times(1)).isDirectory();
    verify(mockFileThree, times(1)).exists();
    verify(mockFileThree, never()).listFiles(any(FileFilter.class));
    verify(mockFileThree, times(1)).delete();
  }

  @Test
  @SuppressWarnings("all")
  public void deleteRecursiveWithExistingEmptyDirectoryIsTrue() {
    when(mockFile.delete()).thenReturn(true);
    when(mockFile.exists()).thenReturn(true);
    when(mockFile.isDirectory()).thenReturn(true);
    when(mockFile.listFiles(any(FileFilter.class))).thenReturn(asArray());

    assertThat(FileSystemUtils.deleteRecursive(mockFile), is(true));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).listFiles(isA(AcceptingAllNonNullFilesFilter.class));
    verify(mockFile, times(1)).exists();
    verify(mockFile, times(1)).delete();
  }

  @Test
  @SuppressWarnings("all")
  public void deleteRecursiveWithNonExistingDirectoryIsFalse() {
    when(mockFile.exists()).thenReturn(false);
    when(mockFile.isDirectory()).thenReturn(true);
    when(mockFile.listFiles(any(FileFilter.class))).thenReturn(asArray());

    assertThat(FileSystemUtils.deleteRecursive(mockFile), is(false));

    verify(mockFile, never()).delete();
    verify(mockFile, times(1)).exists();
    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).listFiles(isA(AcceptingAllNonNullFilesFilter.class));
  }

  @Test
  @SuppressWarnings("all")
  public void deleteRecursiveWithExistingFileIsTrue() {
    when(mockFile.delete()).thenReturn(true);
    when(mockFile.exists()).thenReturn(true);
    when(mockFile.isDirectory()).thenReturn(false);

    assertThat(FileSystemUtils.deleteRecursive(mockFile), is(true));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, never()).listFiles(any(FileFilter.class));
    verify(mockFile, times(1)).exists();
    verify(mockFile, times(1)).delete();
  }

  @Test
  @SuppressWarnings("all")
  public void deleteRecursiveWithNonExistingFileIsFalse() {
    when(mockFile.exists()).thenReturn(false);
    when(mockFile.isDirectory()).thenReturn(false);

    assertThat(FileSystemUtils.deleteRecursive(mockFile), is(false));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, never()).listFiles(any(FileFilter.class));
    verify(mockFile, times(1)).exists();
    verify(mockFile, never()).delete();
  }

  @Test
  public void deleteRecursiveWithNullFileIsFalse() {
    assertThat(FileSystemUtils.deleteRecursive(null), is(false));
  }

  @Test
  public void test() {
    fail("Not Implemented");
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
