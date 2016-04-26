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
import static org.junit.Assume.assumeThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.isA;
import static org.mockito.Matchers.isNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.FileFilter;
import java.io.FilenameFilter;

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
 * The FileSystemUtilsTests class is a test suite of test cases testing the contract and functionality
 * of the FileSystemUtilsTests class.
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
public class FileSystemUtilsTests extends AbstractBaseTestSuite {

  @Rule
  public ExpectedException exception = ExpectedException.none();

  @Mock
  private File mockFile;

  @Mock
  private FileFilter mockFileFilter;

  protected File[] asArray(final File... files) {
    return files;
  }

  @Test
  public void appendToPathWithBasePathAndPathElements() {
    assumeThat(SystemUtils.isUnixBasedOperatingSystem(), is(equalTo(true)));
    assertThat(FileSystemUtils.appendToPath("/absolute/path/to", "a", "relative", "location", "containing", "file.ext"),
      is(equalTo("/absolute/path/to/a/relative/location/containing/file.ext")));
  }

  @Test
  public void appendToPathWithBasePathAndPathElementsContainingFileSeparators() {
    assumeThat(SystemUtils.isUnixBasedOperatingSystem(), is(equalTo(true)));
    assertThat(FileSystemUtils.appendToPath("  ///base/pathname/////to// ", "//a////   ", " /relative/",
      "     /location ", " containing ", "//file.ext"),
        is(equalTo("/base/pathname/to/a/relative/location/containing/file.ext")));
  }

  @Test
  public void appendToPathWithBasePathAndNoPathElements() {
    assumeThat(SystemUtils.isUnixBasedOperatingSystem(), is(equalTo(true)));
    assertThat(FileSystemUtils.appendToPath("/absolute/path/to/nothing"), is(equalTo("/absolute/path/to/nothing")));
  }

  @Test
  public void appendToPathWithBasePathAndEmptyPathElements() {
    assumeThat(SystemUtils.isUnixBasedOperatingSystem(), is(equalTo(true)));
    assertThat(FileSystemUtils.appendToPath("/absolute/path/to", "", "  ", "\t"),
      is(equalTo("/absolute/path/to")));
  }

  @Test
  public void appendToPathWithNullBasePath() {
    exception.expect(NullPointerException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("basePath cannot be null");

    FileSystemUtils.appendToPath(null, "relative", "path", "to", "file.ext");
  }

  @Test
  public void appendToPathWithEmptyBasePathAndPathElements() {
    assumeThat(SystemUtils.isUnixBasedOperatingSystem(), is(equalTo(true)));
    assertThat(FileSystemUtils.appendToPath("", "relative", "path", "to", "file.ext"),
      is(equalTo("/relative/path/to/file.ext")));
  }

  @Test
  public void appendToPathWithBlankBasePathAndNullPathElements() {
    assertThat(FileSystemUtils.appendToPath("  ", (String[]) null), is(equalTo("")));
  }

  @Test
  public void createPathFromPathElements() {
    assumeThat(SystemUtils.isUnixBasedOperatingSystem(), is(equalTo(true)));
    assertThat(FileSystemUtils.createPath("absolute", "path", "to", "a", "file.ext"),
      is(equalTo("/absolute/path/to/a/file.ext")));
    assertThat(FileSystemUtils.createPath("relative", "path", "to", "a", "file.ext"),
      is(equalTo("/relative/path/to/a/file.ext")));
  }

  @Test
  public void createPathFromPathElementsContainingFileSeparators() {
    assumeThat(SystemUtils.isUnixBasedOperatingSystem(), is(true));
    assertThat(FileSystemUtils.createPath("  ///absolute/ ", " /path////   ", "to  ", "/file.ext "),
      is(equalTo("/absolute/path/to/file.ext")));
  }

  @Test
  public void createPathFromEmptyPathElements() {
    assumeThat(SystemUtils.isUnixBasedOperatingSystem(), is(equalTo(true)));
    assertThat(FileSystemUtils.createPath("", "null", " ", "nil", "  "), is(equalTo("/null/nil")));
    assertThat(FileSystemUtils.createPath(null, "", "  ", "\t"), is(equalTo("/")));
    assertThat(FileSystemUtils.createPath(), is(equalTo("/")));
  }

  @Test
  public void createPathFromNullPathElements() {
    assumeThat(SystemUtils.isUnixBasedOperatingSystem(), is(equalTo(true)));
    assertThat(FileSystemUtils.createPath((String[]) null), is(equalTo("/")));
  }

  void whenDirectory(File mockDirectory, File... files) {
    when(mockDirectory.isDirectory()).thenReturn(true);
    when(mockDirectory.isFile()).thenReturn(false);
    when(mockDirectory.listFiles()).thenReturn(files);
    when(mockDirectory.listFiles(any(FileFilter.class))).thenReturn(files);
    when(mockDirectory.listFiles(any(FilenameFilter.class))).thenReturn(files);
  }

  void whenFile(File mockFile) {
    when(mockFile.isDirectory()).thenReturn(false);
    when(mockFile.isFile()).thenReturn(true);
  }

  void whenFile(File mockFile, boolean hidden) {
    whenFile(mockFile);
    when(mockFile.isHidden()).thenReturn(hidden);
  }

  @SuppressWarnings("all")
  void verifyDirectory(File mockDirectory) {
    verify(mockDirectory, times(1)).isDirectory();
    verify(mockDirectory, times(1)).isFile();
    verify(mockDirectory, times(1)).listFiles(isA(FileFilter.class));
  }

  @SuppressWarnings("all")
  void verifySubDirectory(File mockSubDirectory) {
    verify(mockSubDirectory, times(2)).isDirectory();
    verify(mockSubDirectory, times(1)).isFile();
    verify(mockSubDirectory, times(1)).listFiles(isA(FileFilter.class));
  }

  @SuppressWarnings("all")
  void verifyFile(File mockFile) {
    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).isFile();
    verify(mockFile, never()).listFiles(any(FileFilter.class));
  }

  @SuppressWarnings("all")
  void verifyHiddenDirectory(File mockDirectory) {
    verifyDirectory(mockDirectory);
    verify(mockDirectory, times(1)).isHidden();
  }

  @SuppressWarnings("all")
  void verifyHiddenSubDirectory(File mockDirectory) {
    verifySubDirectory(mockDirectory);
    verify(mockDirectory, times(1)).isHidden();
  }

  @SuppressWarnings("all")
  void verifyHiddenFile(File mockFile) {
    verifyFile(mockFile);
    verify(mockFile, times(1)).isHidden();
  }

  @Test
  @SuppressWarnings("all")
  public void countNonFilteredEmptyDirectoryReturnsZero() {
    whenDirectory(mockFile);
    assertThat(FileSystemUtils.count(mockFile), is(equalTo(0)));
    verifyDirectory(mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void countNonFilteredNonEmptyDirectoryReturnsOne() {
    File mockFileOne = mock(File.class, "MockFileOne");

    whenDirectory(mockFile, mockFileOne);
    whenFile(mockFileOne);

    assertThat(FileSystemUtils.count(mockFile), is(equalTo(1)));

    verifyDirectory(mockFile);
    verifyFile(mockFileOne);
  }

  @Test
  @SuppressWarnings("all")
  public void countNonFilteredNonEmptyDirectoryReturnsTwo() {
    File mockFileOne = mock(File.class, "MockFileOne");
    File mockFileTwo = mock(File.class, "MockFileTwo");

    whenDirectory(mockFile, mockFileOne, mockFileTwo);
    whenFile(mockFileOne);
    whenFile(mockFileTwo);

    assertThat(FileSystemUtils.count(mockFile), is(equalTo(2)));

    verifyDirectory(mockFile);
    verifyFile(mockFileOne);
    verifyFile(mockFileTwo);
  }

  @Test
  @SuppressWarnings("all")
  public void countNonFilteredFileReturnsOne() {
    whenFile(mockFile);
    assertThat(FileSystemUtils.count(mockFile), is(equalTo(1)));
    verifyFile(mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void countFilteredDirectoryReturnsOne() {
    FileFilter mockFileFilter = mock(FileFilter.class);

    when(mockFileFilter.accept(any(File.class))).thenReturn(true);
    whenDirectory(mockFile);

    assertThat(FileSystemUtils.count(mockFile, mockFileFilter), is(equalTo(1)));

    verify(mockFileFilter, times(1)).accept(eq(mockFile));
    verify(mockFile, times(1)).isDirectory();
  }

  @Test
  @SuppressWarnings("all")
  public void countFilteredFileReturnsZero() {
    FileFilter mockFileFilter = mock(FileFilter.class);

    when(mockFileFilter.accept(any(File.class))).thenReturn(false);
    whenFile(mockFile);

    assertThat(FileSystemUtils.count(mockFile, mockFileFilter), is(equalTo(0)));

    verify(mockFileFilter, times(1)).accept(eq(mockFile));
    verify(mockFile, times(1)).isDirectory();
  }

  @Test
  public void countNullReturnsZero() {
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

    whenDirectory(mockFile, mockSubDirectoryOne, mockSubDirectoryTwo, mockFileOne, mockFileTwo, mockFileThree);
    whenDirectory(mockSubDirectoryOne, mockFileFour);
    whenDirectory(mockSubDirectoryTwo);
    whenFile(mockFileOne);
    whenFile(mockFileTwo);
    whenFile(mockFileThree);
    whenFile(mockFileFour);

    assertThat(FileSystemUtils.count(mockFile), is(equalTo(4)));

    verifyDirectory(mockFile);
    verifySubDirectory(mockSubDirectoryOne);
    verifySubDirectory(mockSubDirectoryTwo);
    verifyFile(mockFileOne);
    verifyFile(mockFileTwo);
    verifyFile(mockFileThree);
    verifyFile(mockFileFour);
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
    File mockFileSix = mock(File.class, "MockFileSix");
    File mockFileSeven = mock(File.class, "MockFileSeven");

    whenDirectory(mockFile, mockSubDirectoryOne, mockSubDirectoryTwo, mockFileOne, mockFileTwo, mockFileThree);
    whenDirectory(mockSubDirectoryOne, mockFileFour, mockFileFive);
    whenDirectory(mockSubDirectoryTwo, mockSubDirectoryThree, mockFileSix);
    whenDirectory(mockSubDirectoryThree, mockFileSeven);
    whenFile(mockFileOne, false);
    whenFile(mockFileTwo, true);
    whenFile(mockFileThree, false);
    whenFile(mockFileFour, true);
    whenFile(mockFileFive, false);
    whenFile(mockFileSix, true);
    whenFile(mockFileSeven, false);

    when(mockFileFilter.accept(any(File.class))).thenAnswer(new Answer<Boolean>() {
      public Boolean answer(InvocationOnMock invocationOnMock) throws Throwable {
        File file = invocationOnMock.getArgumentAt(0, File.class);
        return !file.isHidden();
      }
    });

    assertThat(FileSystemUtils.count(mockFile, ComposableFileFilter.and(FileOnlyFilter.INSTANCE, mockFileFilter)),
      is(equalTo(4)));

    verifyHiddenDirectory(mockFile);
    verifyHiddenSubDirectory(mockSubDirectoryOne);
    verifyHiddenSubDirectory(mockSubDirectoryTwo);
    verifyHiddenSubDirectory(mockSubDirectoryThree);
    verifyHiddenFile(mockFileOne);
    verifyHiddenFile(mockFileTwo);
    verifyHiddenFile(mockFileThree);
    verifyHiddenFile(mockFileFour);
    verifyHiddenFile(mockFileFive);
    verifyHiddenFile(mockFileSix);
    verifyHiddenFile(mockFileSeven);
  }

  @Test
  @IntegrationTest
  public void countJavaSourceFilesInProjectReturnsNonZeroCount() {
    assertThat(FileSystemUtils.count(getSourceDirectory(), new FileExtensionFilter("java")),
      is(greaterThanOrEqualTo(398)));
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

    when(mockFile.delete()).thenReturn(true);
    when(mockFile.exists()).thenReturn(true);
    when(mockFile.isDirectory()).thenReturn(true);
    when(mockFile.listFiles(any(FileFilter.class))).thenReturn(asArray(mockSubDirectoryOne, mockSubDirectoryTwo,
      mockFileOne, mockFileTwo));
    when(mockSubDirectoryOne.delete()).thenReturn(true);
    when(mockSubDirectoryOne.exists()).thenReturn(true);
    when(mockSubDirectoryOne.isDirectory()).thenReturn(true);
    when(mockSubDirectoryOne.listFiles(any(FileFilter.class))).thenReturn(asArray(mockFileThree));
    when(mockSubDirectoryTwo.delete()).thenReturn(true);
    when(mockSubDirectoryTwo.exists()).thenReturn(true);
    when(mockSubDirectoryTwo.isDirectory()).thenReturn(true);
    when(mockSubDirectoryTwo.listFiles(any(FileFilter.class))).thenReturn(asArray());
    when(mockFileOne.delete()).thenReturn(true);
    when(mockFileOne.exists()).thenReturn(true);
    when(mockFileOne.isDirectory()).thenReturn(false);
    when(mockFileTwo.delete()).thenReturn(true);
    when(mockFileTwo.exists()).thenReturn(true);
    when(mockFileTwo.isDirectory()).thenReturn(false);
    when(mockFileThree.delete()).thenReturn(true);
    when(mockFileThree.exists()).thenReturn(true);
    when(mockFileThree.isDirectory()).thenReturn(false);

    assertThat(FileSystemUtils.deleteRecursive(mockFile), is(true));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(2)).exists();
    verify(mockFile, times(1)).listFiles(isA(FileFilter.class));
    verify(mockFile, times(1)).delete();
    verify(mockSubDirectoryOne, times(2)).isDirectory();
    verify(mockSubDirectoryOne, times(1)).listFiles(isA(FileFilter.class));
    verify(mockSubDirectoryOne, times(2)).exists();
    verify(mockSubDirectoryOne, times(1)).delete();
    verify(mockSubDirectoryTwo, times(2)).isDirectory();
    verify(mockSubDirectoryTwo, times(1)).listFiles(isA(FileFilter.class));
    verify(mockSubDirectoryTwo, times(2)).exists();
    verify(mockSubDirectoryTwo, times(1)).delete();
    verify(mockFileOne, times(1)).isDirectory();
    verify(mockFileOne, never()).listFiles(any(FileFilter.class));
    verify(mockFileOne, times(2)).exists();
    verify(mockFileOne, times(1)).delete();
    verify(mockFileTwo, times(1)).isDirectory();
    verify(mockFileTwo, never()).listFiles(any(FileFilter.class));
    verify(mockFileTwo, times(2)).exists();
    verify(mockFileTwo, times(1)).delete();
    verify(mockFileThree, times(1)).isDirectory();
    verify(mockFileThree, never()).listFiles(any(FileFilter.class));
    verify(mockFileThree, times(2)).exists();
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
    verify(mockFile, times(1)).listFiles(isA(FileFilter.class));
    verify(mockFile, times(2)).exists();
    verify(mockFile, times(1)).delete();
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
    verify(mockFile, times(2)).exists();
    verify(mockFile, times(1)).delete();
  }

  @Test
  @SuppressWarnings("all")
  public void deleteRecursiveWithNonExistingDirectoryIsFalse() {
    when(mockFile.exists()).thenReturn(false);
    when(mockFile.isDirectory()).thenReturn(false);

    assertThat(FileSystemUtils.deleteRecursive(mockFile), is(false));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, never()).listFiles(any(FileFilter.class));
    verify(mockFile, times(1)).exists();
    verify(mockFile, never()).delete();
  }

  @Test
  public void deleteRecursiveWithNullIsFalse() {
    assertThat(FileSystemUtils.deleteRecursive(null), is(false));
  }

  @Test
  public void deletedRecursiveWithNullAndAcceptingFileFilterIsFalse() {
    when(mockFileFilter.accept(any(File.class))).thenReturn(true);
    assertThat(FileSystemUtils.deleteRecursive(null, mockFileFilter), is(false));
    verify(mockFileFilter, times(1)).accept(isNull(File.class));
  }

  // TODO add additional deleteRecursive test cases

  @Test
  @SuppressWarnings("all")
  public void isEmptyDirectoryWithEmptyDirectoryIsTrue() {
    whenDirectory(mockFile);

    assertThat(FileSystemUtils.isEmptyDirectory(mockFile), is(equalTo(true)));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).listFiles();
  }

  @Test
  @SuppressWarnings("all")
  public void isEmptyDirectoryWithNonEmptyDirectoryIsFalse() {
    whenDirectory(mockFile, new File[10]);

    assertThat(FileSystemUtils.isEmptyDirectory(mockFile), is(equalTo(false)));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).listFiles();
  }

  @Test
  @SuppressWarnings("all")
  public void isEmptyDirectoryWithFileIsFalse() {
    whenFile(mockFile);

    assertThat(FileSystemUtils.isEmptyDirectory(mockFile), is(false));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, never()).isFile();
    verify(mockFile, never()).listFiles();
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
