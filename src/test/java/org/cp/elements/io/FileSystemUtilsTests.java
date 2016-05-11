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
import java.util.Arrays;
import java.util.List;

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

  protected File[] asArray(File... files) {
    return files;
  }

  protected List<File> asList(File... files) {
    return Arrays.asList(files);
  }

  protected File mockFile(String name) {
    return mock(File.class, name);
  }

  protected File newFile(File parent, File child) {
    return new File(parent, child.getAbsolutePath());
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

  File whenDirectory(File mockDirectory, File... files) {
    when(mockDirectory.exists()).thenReturn(true);
    when(mockDirectory.isDirectory()).thenReturn(true);
    when(mockDirectory.isFile()).thenReturn(false);
    when(mockDirectory.listFiles()).thenReturn(files);
    when(mockDirectory.listFiles(any(FileFilter.class))).thenReturn(files);
    when(mockDirectory.listFiles(any(FilenameFilter.class))).thenReturn(files);
    return mockDirectory;
  }

  File whenFile(File mockFile) {
    when(mockFile.exists()).thenReturn(true);
    when(mockFile.isDirectory()).thenReturn(false);
    when(mockFile.isFile()).thenReturn(true);
    return mockFile;
  }

  File whenFile(File mockFile, boolean hidden) {
    whenFile(mockFile);
    when(mockFile.isHidden()).thenReturn(hidden);
    return mockFile;
  }

  File whenFile(File mockFile, long length) {
    whenFile(mockFile);
    when(mockFile.length()).thenReturn(length);
    return mockFile;
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
    File mockFileOne = mockFile("MockFileOne");

    whenDirectory(mockFile, mockFileOne);
    whenFile(mockFileOne);

    assertThat(FileSystemUtils.count(mockFile), is(equalTo(1)));

    verifyDirectory(mockFile);
    verifyFile(mockFileOne);
  }

  @Test
  @SuppressWarnings("all")
  public void countNonFilteredNonEmptyDirectoryReturnsTwo() {
    File mockFileOne = mockFile("MockFileOne");
    File mockFileTwo = mockFile("MockFileTwo");

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
    File mockSubDirectoryOne = mockFile("MockSubDirectoryOne");
    File mockSubDirectoryTwo = mockFile("MockSubDirectoryTwo");
    File mockFileOne = mockFile("MockFileOne");
    File mockFileTwo = mockFile("MockFileTwo");
    File mockFileThree = mockFile("MockFileThree");
    File mockFileFour = mockFile("MockFileFour");

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
    File mockSubDirectoryOne = mockFile("MockSubDirectoryOne");
    File mockSubDirectoryTwo = mockFile("MockSubDirectoryTwo");
    File mockSubDirectoryThree = mockFile("MockSubDirectoryThree");
    File mockFileOne = mockFile("MockFileOne");
    File mockFileTwo = mockFile("MockFileTwo");
    File mockFileThree = mockFile("MockFileThree");
    File mockFileFour = mockFile("MockFileFour");
    File mockFileFive = mockFile("MockFileFive");
    File mockFileSix = mockFile("MockFileSix");
    File mockFileSeven = mockFile("MockFileSeven");

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
  public void deleteRecursive() {
    File mockFileOne = whenFile(mockFile("MockFileOne"));
    File mockFileTwo = whenFile(mockFile("MockFileTwo"));
    File mockFileThree = whenFile(mockFile("MockFileThree"));
    File mockFileFour = whenFile(mockFile("MockFileFour"));
    File mockFileFive = whenFile(mockFile("MockFileFive"));
    File mockFileSix = whenFile(mockFile("MockFileSix"));
    File mockFileSeven = whenFile(mockFile("MockFileSeven"));
    File mockSubDirectoryOne = whenDirectory(mockFile("MockSubDirectoryOne"), mockFileSix);
    File mockSubDirectoryTwo = whenDirectory(mockFile("MockSubDirectoryTwo"), mockSubDirectoryOne,
      mockFileFour, mockFileFive);
    File mockSubDirectoryThree = whenDirectory(mockFile("MockSubDirectoryThree"), mockFileSeven);

    when(mockFileOne.delete()).thenReturn(true);
    when(mockFileTwo.delete()).thenReturn(true);
    when(mockFileThree.delete()).thenReturn(true);
    when(mockFileFour.delete()).thenReturn(true);
    when(mockFileFive.delete()).thenReturn(true);
    when(mockFileSix.delete()).thenReturn(true);
    when(mockFileSeven.delete()).thenReturn(true);
    when(mockSubDirectoryOne.delete()).thenReturn(true);
    when(mockSubDirectoryTwo.delete()).thenReturn(true);
    when(mockSubDirectoryThree.delete()).thenReturn(true);
    when(mockFileFilter.accept(any(File.class))).thenReturn(true);
    when(whenDirectory(mockFile, mockSubDirectoryTwo, mockSubDirectoryThree, mockFileOne, mockFileTwo,
      mockFileThree).delete()).thenReturn(true);

    assertThat(FileSystemUtils.deleteRecursive(mockFile, mockFileFilter), is(true));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).listFiles(eq(mockFileFilter));
    verify(mockFile, times(1)).exists();
    verify(mockFile, times(1)).delete();
    verify(mockSubDirectoryOne, times(2)).isDirectory();
    verify(mockSubDirectoryOne, times(1)).listFiles(eq(mockFileFilter));
    verify(mockSubDirectoryOne, times(1)).exists();
    verify(mockSubDirectoryOne, times(1)).delete();
    verify(mockSubDirectoryTwo, times(2)).isDirectory();
    verify(mockSubDirectoryTwo, times(1)).listFiles(eq(mockFileFilter));
    verify(mockSubDirectoryTwo, times(1)).exists();
    verify(mockSubDirectoryTwo, times(1)).delete();
    verify(mockSubDirectoryThree, times(2)).isDirectory();
    verify(mockSubDirectoryThree, times(1)).listFiles(eq(mockFileFilter));
    verify(mockSubDirectoryThree, times(1)).exists();
    verify(mockSubDirectoryThree, times(1)).delete();
    verify(mockFileOne, times(1)).isDirectory();
    verify(mockFileOne, never()).listFiles(any(FileFilter.class));
    verify(mockFileOne, times(1)).exists();
    verify(mockFileOne, times(1)).delete();
    verify(mockFileTwo, times(1)).isDirectory();
    verify(mockFileTwo, never()).listFiles(any(FileFilter.class));
    verify(mockFileTwo, times(1)).exists();
    verify(mockFileTwo, times(1)).delete();
    verify(mockFileThree, times(1)).isDirectory();
    verify(mockFileThree, never()).listFiles(any(FileFilter.class));
    verify(mockFileThree, times(1)).exists();
    verify(mockFileThree, times(1)).delete();
    verify(mockFileFour, times(1)).isDirectory();
    verify(mockFileFour, never()).listFiles(any(FileFilter.class));
    verify(mockFileFour, times(1)).exists();
    verify(mockFileFour, times(1)).delete();
    verify(mockFileFive, times(1)).isDirectory();
    verify(mockFileFive, never()).listFiles(any(FileFilter.class));
    verify(mockFileFive, times(1)).exists();
    verify(mockFileFive, times(1)).delete();
    verify(mockFileSix, times(1)).isDirectory();
    verify(mockFileSix, never()).listFiles(any(FileFilter.class));
    verify(mockFileSix, times(1)).exists();
    verify(mockFileSix, times(1)).delete();
    verify(mockFileSeven, times(1)).isDirectory();
    verify(mockFileSeven, never()).listFiles(any(FileFilter.class));
    verify(mockFileSeven, times(1)).exists();
    verify(mockFileSeven, times(1)).delete();
    verify(mockFileFilter, times(1)).accept(eq(mockFile));
    verify(mockFileFilter, times(1)).accept(eq(mockSubDirectoryOne));
    verify(mockFileFilter, times(1)).accept(eq(mockSubDirectoryTwo));
    verify(mockFileFilter, times(1)).accept(eq(mockSubDirectoryThree));
    verify(mockFileFilter, never()).accept(eq(mockFileOne));
    verify(mockFileFilter, never()).accept(eq(mockFileTwo));
    verify(mockFileFilter, never()).accept(eq(mockFileThree));
    verify(mockFileFilter, never()).accept(eq(mockFileFour));
    verify(mockFileFilter, never()).accept(eq(mockFileFive));
    verify(mockFileFilter, never()).accept(eq(mockFileSix));
    verify(mockFileFilter, never()).accept(eq(mockFileSeven));
  }

  @Test
  @SuppressWarnings("all")
  public void deleteRecursiveWithNonEmptyDirectoryIsTrue() {
    File mockFileOne = whenFile(mockFile("MockFileOne"));
    File mockFileTwo = whenFile(mockFile("MockFileTwo"));

    when(mockFileOne.delete()).thenReturn(true);
    when(mockFileTwo.delete()).thenReturn(true);
    when(whenDirectory(mockFile, mockFileOne, mockFileTwo).delete()).thenReturn(true);

    assertThat(FileSystemUtils.deleteRecursive(mockFile), is(true));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).listFiles(isA(FileFilter.class));
    verify(mockFile, times(2)).exists();
    verify(mockFile, times(1)).delete();
    verify(mockFileOne, times(1)).isDirectory();
    verify(mockFileOne, times(1)).exists();
    verify(mockFileOne, times(1)).delete();
    verify(mockFileTwo, times(1)).isDirectory();
    verify(mockFileTwo, times(1)).exists();
    verify(mockFileTwo, times(1)).delete();
  }

  @Test
  @SuppressWarnings("all")
  public void deleteRecursiveWithFilteredNonEmptyDirectoryFailsToDeleteReturnsFalse() {
    File mockFileOne = whenFile(mockFile("MockFileOne"));
    File mockFileTwo = whenFile(mockFile("MockFileTwo"));

    when(mockFileOne.delete()).thenReturn(true);
    when(mockFileTwo.delete()).thenReturn(true);
    when(mockFileFilter.accept(any(File.class))).thenReturn(true);
    when(whenDirectory(mockFile, mockFileOne, mockFileTwo).delete()).thenReturn(false);

    assertThat(FileSystemUtils.deleteRecursive(mockFile, mockFileFilter), is(false));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).listFiles(eq(mockFileFilter));
    verify(mockFile, times(1)).exists();
    verify(mockFile, times(1)).delete();
    verify(mockFileFilter, times(1)).accept(eq(mockFile));
    verify(mockFileOne, times(1)).isDirectory();
    verify(mockFileOne, times(1)).exists();
    verify(mockFileOne, times(1)).delete();
    verify(mockFileTwo, times(1)).isDirectory();
    verify(mockFileTwo, times(1)).exists();
    verify(mockFileTwo, times(1)).delete();
  }

  @Test
  @SuppressWarnings("all")
  public void deleteRecursiveWithFilteredNonEmptyDirectoryShortCircuitsAndReturnsFalse() {
    File mockFileOne = whenFile(mockFile("MockFileOne"));
    File mockFileTwo = whenFile(mockFile("MockFileTwo"));

    when(mockFileOne.delete()).thenReturn(false);
    when(mockFileTwo.delete()).thenReturn(false);
    when(mockFileFilter.accept(any(File.class))).thenReturn(true);
    when(whenDirectory(mockFile, mockFileOne, mockFileTwo).delete()).thenReturn(true);

    assertThat(FileSystemUtils.deleteRecursive(mockFile, mockFileFilter), is(false));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).listFiles(eq(mockFileFilter));
    verify(mockFile, never()).exists();
    verify(mockFile, never()).delete();
    verify(mockFileFilter, never()).accept(eq(mockFile));
    verify(mockFileOne, times(1)).isDirectory();
    verify(mockFileOne, times(1)).exists();
    verify(mockFileOne, times(1)).delete();
    verify(mockFileTwo, times(1)).isDirectory();
    verify(mockFileTwo, times(1)).exists();
    verify(mockFileTwo, times(1)).delete();
  }

  @Test
  @SuppressWarnings("all")
  public void deleteRecursiveWithEmptyDirectoryIsTrue() {
    when(whenDirectory(mockFile).delete()).thenReturn(true);

    assertThat(FileSystemUtils.deleteRecursive(mockFile), is(true));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).listFiles(isA(FileFilter.class));
    verify(mockFile, times(2)).exists();
    verify(mockFile, times(1)).delete();
  }

  @Test
  @SuppressWarnings("all")
  public void deleteRecursiveWithFilteredEmptyDirectoryIsFalse() {
    when(mockFileFilter.accept(any(File.class))).thenReturn(false);
    whenDirectory(mockFile);

    assertThat(FileSystemUtils.deleteRecursive(mockFile, mockFileFilter), is(false));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).listFiles(isA(FileFilter.class));
    verify(mockFile, never()).delete();
    verify(mockFileFilter, times(1)).accept(eq(mockFile));
  }

  @Test
  @SuppressWarnings("all")
  public void deleteRecursiveWithFileReturnsTrue() {
    when(whenFile(mockFile).delete()).thenReturn(true);

    assertThat(FileSystemUtils.deleteRecursive(mockFile), is(true));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, never()).listFiles(any(FileFilter.class));
    verify(mockFile, times(2)).exists();
    verify(mockFile, times(1)).delete();
  }

  @Test
  @SuppressWarnings("all")
  public void deleteResusiveWithFilteredFileReturnsFalse() {
    when(mockFileFilter.accept(any(File.class))).thenReturn(false);
    whenFile(mockFile);

    assertThat(FileSystemUtils.deleteRecursive(mockFile, mockFileFilter), is(false));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, never()).listFiles(any(FileFilter.class));
    verify(mockFile, never()).delete();
    verify(mockFileFilter, times(1)).accept(eq(mockFile));
  }

  @Test
  public void deleteRecursiveWithNullReturnsFalse() {
    assertThat(FileSystemUtils.deleteRecursive(null), is(false));
  }

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
  public void isRelativeToWorkingDirectoryWithParentOfWorkingDirectoryIsFalse() {
    assertThat(FileSystemUtils.isRelativeToWorkingDirectory(WORKING_DIRECTORY.getParentFile()), is(false));
  }

  @Test
  public void isRelativeToWorkingDirectoryWithNonRelativeFileIsFalse() {
    File nonRelativeFile = new File("/non/relative/path/to/working/directory");
    assertThat(FileSystemUtils.isRelativeToWorkingDirectory(nonRelativeFile), is(false));
  }

  @Test
  @SuppressWarnings("all")
  public void isRelativeToWorkingDirectoryWithChildOfWorkingDirectoryIsTrue() {
    assertThat(FileSystemUtils.isRelativeToWorkingDirectory(newFile(WORKING_DIRECTORY,
      WORKING_DIRECTORY.listFiles()[0])), is(true));
  }

  @Test
  public void isRelativeToWorkingDirectoryWithFileSystemUtilsClassIsTrue() {
    assertThat(FileSystemUtils.isRelativeToWorkingDirectory(getLocation(FileSystemUtils.class)), is(true));
  }

  @Test
  public void isRelativeToWorkingDirectoryWithNullIsFalse() {
    assertThat(FileSystemUtils.isRelativeToWorkingDirectory(null), is(false));
  }

  @Test
  @SuppressWarnings("all")
  public void listFilesWithNonEmptyFilteredDirectory() {
    File mockFileOne = whenFile(mockFile("MockFileOne"));
    File mockFileTwo = whenFile(mockFile("MockFileTwo"));
    File mockFileThree = whenFile(mockFile("MockFileThree"));
    File mockSubDirectory = whenDirectory(mockFile("MockSubDirectory"), mockFileOne, mockFileTwo);

    mockFile = whenDirectory(mockFile, mockSubDirectory, mockFileThree);

    assertThat(FileSystemUtils.listFiles(mockFile, mockFileFilter),
      is(equalTo(asArray(mockFileOne, mockFileTwo, mockFileThree))));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).listFiles(eq(mockFileFilter));
    verify(mockSubDirectory, times(2)).isDirectory();
    verify(mockSubDirectory, times(1)).listFiles(eq(mockFileFilter));
    verify(mockFileOne, times(1)).isDirectory();
    verify(mockFileOne, never()).listFiles(any(FileFilter.class));
    verify(mockFileTwo, times(1)).isDirectory();
    verify(mockFileTwo, never()).listFiles(any(FileFilter.class));
    verify(mockFileThree, times(1)).isDirectory();
    verify(mockFileThree, never()).listFiles(any(FileFilter.class));
  }

  @Test
  @SuppressWarnings("all")
  public void listFilesWithNonEmptyDirectory() {
    File mockFileOne = whenFile(mockFile("MockFileOne"));
    File mockFileTwo = whenFile(mockFile("MockFileTwo"));
    File mockFileThree = whenFile(mockFile("MockFileThree"));
    File mockSubDirectory = whenDirectory(mockFile("MockSubDirectory"), mockFileOne, mockFileTwo);

    mockFile = whenDirectory(mockFile, mockSubDirectory, mockFileThree);

    assertThat(FileSystemUtils.listFiles(mockFile), is(equalTo(asArray(mockFileOne, mockFileTwo, mockFileThree))));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).listFiles(isA(FileFilter.class));
    verify(mockSubDirectory, times(2)).isDirectory();
    verify(mockSubDirectory, times(1)).listFiles(isA(FileFilter.class));
    verify(mockFileOne, times(1)).isDirectory();
    verify(mockFileOne, never()).listFiles(any(FileFilter.class));
    verify(mockFileTwo, times(1)).isDirectory();
    verify(mockFileTwo, never()).listFiles(any(FileFilter.class));
    verify(mockFileThree, times(1)).isDirectory();
    verify(mockFileThree, never()).listFiles(any(FileFilter.class));
  }

  @Test
  @SuppressWarnings("all")
  public void listFilesWithNonEmptyDirectoryAndNullFileFilter() {
    File mockFileOne = whenFile(mockFile("MockFileOne"));
    File mockFileTwo = whenFile(mockFile("MockfileTwo"));

    mockFile = whenDirectory(mockFile, mockFileOne, mockFileTwo);

    assertThat(FileSystemUtils.listFiles(mockFile, null), is(equalTo(asArray(mockFileOne, mockFileTwo))));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).listFiles(isNull(FileFilter.class));
    verify(mockFileOne, times(1)).isDirectory();
    verify(mockFileOne, never()).listFiles(any(FileFilter.class));
    verify(mockFileTwo, times(1)).isDirectory();
    verify(mockFileTwo, never()).listFiles(any(FileFilter.class));
  }

  @Test
  @SuppressWarnings("all")
  public void listFilesWithEmptyDirectory() {
    mockFile = whenDirectory(mockFile);

    assertThat(FileSystemUtils.listFiles(mockFile), is(equalTo(FileSystemUtils.NO_FILES)));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).listFiles(isA(FileFilter.class));
  }

  @Test
  @SuppressWarnings("all")
  public void listFilesWithFile() {
    mockFile = whenFile(mockFile);

    assertThat(FileSystemUtils.listFiles(mockFile, null), is(equalTo(FileSystemUtils.NO_FILES)));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, never()).listFiles(any(FileFilter.class));
  }

  @Test
  public void listFilesWithNull() {
    assertThat(FileSystemUtils.listFiles(null, mockFileFilter), is(equalTo(FileSystemUtils.NO_FILES)));
  }

  @Test
  @SuppressWarnings("all")
  public void safeListFilesWithDirectory() {
    File mockFileOne = mockFile("MockFileOne");
    File mockFileTwo = mockFile("MockFileTwo");

    mockFile = whenDirectory(mockFile, mockFileOne, mockFileTwo);

    assertThat(FileSystemUtils.safeListFiles(mockFile), is(equalTo(asArray(mockFileOne, mockFileTwo))));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).listFiles(isA(FileFilter.class));
  }

  @Test
  @SuppressWarnings("all")
  public void safeListFilesWithDirectoryAndFileFilter() {
    File mockFileOne = mockFile("MockFileOne");
    File mockFileTwo = mockFile("MockFileTwo");

    mockFile = whenDirectory(mockFile, mockFileOne, mockFileTwo);

    assertThat(FileSystemUtils.safeListFiles(mockFile, mockFileFilter), is(equalTo(asArray(mockFileOne, mockFileTwo))));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).listFiles(eq(mockFileFilter));
  }

  @Test
  @SuppressWarnings("all")
  public void safeListFilesWithFile() {
    mockFile = whenFile(mockFile);

    assertThat(FileSystemUtils.safeListFiles(mockFile), is(equalTo(FileSystemUtils.NO_FILES)));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, never()).listFiles(any(FileFilter.class));
  }

  @Test
  public void safeListFilesWithNull() {
    assertThat(FileSystemUtils.safeListFiles(null), is(equalTo(FileSystemUtils.NO_FILES)));
  }

  @Test
  @SuppressWarnings("all")
  public void sizeOfFile() {
    whenFile(mockFile);
    when(mockFile.length()).thenReturn(1024000l);

    assertThat(FileSystemUtils.size(mockFile), is(equalTo(1024000l)));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(2)).isFile();
    verify(mockFile, times(1)).length();
    verify(mockFile, never()).listFiles(any(FileFilter.class));
  }

  @Test
  @SuppressWarnings("all")
  public void sizeOfEmptyDirectory() {
    whenDirectory(mockFile);
    when(mockFile.length()).thenReturn(1l);

    assertThat(FileSystemUtils.size(mockFile), is(equalTo(0l)));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(2)).isFile();
    verify(mockFile, never()).length();
    verify(mockFile, times(1)).listFiles(isA(FileFilter.class));
  }

  @Test
  @SuppressWarnings("all")
  public void sizeOfDirectoryWithOneFile() {
    File mockFileOne = mockFile("MockFileOne");

    whenDirectory(mockFile, mockFileOne);
    whenFile(mockFileOne);
    when(mockFileOne.length()).thenReturn(512l);

    assertThat(FileSystemUtils.size(mockFile), is(equalTo(512l)));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(2)).isFile();
    verify(mockFile, never()).length();
    verify(mockFile, times(1)).listFiles(isA(FileFilter.class));
    verify(mockFileOne, times(1)).isDirectory();
    verify(mockFileOne, times(2)).isFile();
    verify(mockFileOne, times(1)).length();
    verify(mockFileOne, never()).listFiles(any(FileFilter.class));
  }

  @Test
  @SuppressWarnings("all")
  public void sizeOfDirectoryWithTwoFiles() {
    File mockFileOne = mockFile("MockFileOne");
    File mockFileTwo = mockFile("MockFileTwo");

    whenDirectory(mockFile, mockFileOne, mockFileTwo);
    whenFile(mockFileOne);
    when(mockFileOne.length()).thenReturn(512l);
    whenFile(mockFileTwo);
    when(mockFileTwo.length()).thenReturn(512l);

    assertThat(FileSystemUtils.size(mockFile), is(equalTo(1024l)));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(2)).isFile();
    verify(mockFile, never()).length();
    verify(mockFile, times(1)).listFiles(isA(FileFilter.class));
    verify(mockFileOne, times(1)).isDirectory();
    verify(mockFileOne, times(2)).isFile();
    verify(mockFileOne, times(1)).length();
    verify(mockFileOne, never()).listFiles(any(FileFilter.class));
    verify(mockFileTwo, times(1)).isDirectory();
    verify(mockFileTwo, times(2)).isFile();
    verify(mockFileTwo, times(1)).length();
    verify(mockFileTwo, never()).listFiles(any(FileFilter.class));
  }

  @Test
  @SuppressWarnings("all")
  public void sizeOfDirectoryAndSubDirectoriesWithFiles() {
    File mockSubDirectoryOne = mockFile("MockSubDirectoryOne");
    File mockSubDirectoryTwo = mockFile("MockSubDirectoryTwo");
    File mockSubDirectoryThree = mockFile("MockSubDirectoryThree");
    File mockFileOne = mockFile("MockFileOne");
    File mockFileTwo = mockFile("MockFileTwo");
    File mockFileThree = mockFile("MockFileThree");
    File mockFileFour = mockFile("MockFileFour");
    File mockFileFive = mockFile("MockFileFive");

    whenDirectory(mockFile, mockSubDirectoryOne, mockSubDirectoryTwo, mockFileOne);
    whenDirectory(mockSubDirectoryOne, mockSubDirectoryThree, mockFileTwo);
    whenDirectory(mockSubDirectoryTwo, mockFileThree);
    whenDirectory(mockSubDirectoryThree, mockFileFour, mockFileFive);
    whenFile(mockFileOne, 256l);
    whenFile(mockFileTwo, 512l);
    whenFile(mockFileThree, 1024l);
    whenFile(mockFileFour, 256l);
    whenFile(mockFileFive, 2048l);

    assertThat(FileSystemUtils.size(mockFile), is(equalTo(4096l)));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(2)).isFile();
    verify(mockFile, never()).length();
    verify(mockFile, times(1)).listFiles(isA(FileFilter.class));
    verify(mockSubDirectoryOne, times(1)).isDirectory();
    verify(mockSubDirectoryOne, times(2)).isFile();
    verify(mockSubDirectoryOne, never()).length();
    verify(mockSubDirectoryOne, times(1)).listFiles(isA(FileFilter.class));
    verify(mockSubDirectoryTwo, times(1)).isDirectory();
    verify(mockSubDirectoryTwo, times(2)).isFile();
    verify(mockSubDirectoryTwo, never()).length();
    verify(mockSubDirectoryTwo, times(1)).listFiles(isA(FileFilter.class));
    verify(mockSubDirectoryThree, times(1)).isDirectory();
    verify(mockSubDirectoryThree, times(2)).isFile();
    verify(mockSubDirectoryThree, never()).length();
    verify(mockSubDirectoryThree, times(1)).listFiles(isA(FileFilter.class));
    verify(mockFileOne, times(1)).isDirectory();
    verify(mockFileOne, times(2)).isFile();
    verify(mockFileOne, times(1)).length();
    verify(mockFileOne, never()).listFiles(any(FileFilter.class));
    verify(mockFileTwo, times(1)).isDirectory();
    verify(mockFileTwo, times(2)).isFile();
    verify(mockFileTwo, times(1)).length();
    verify(mockFileTwo, never()).listFiles(any(FileFilter.class));
    verify(mockFileThree, times(1)).isDirectory();
    verify(mockFileThree, times(2)).isFile();
    verify(mockFileThree, times(1)).length();
    verify(mockFileThree, never()).listFiles(any(FileFilter.class));
    verify(mockFileFour, times(1)).isDirectory();
    verify(mockFileFour, times(2)).isFile();
    verify(mockFileFour, times(1)).length();
    verify(mockFileFour, never()).listFiles(any(FileFilter.class));
    verify(mockFileFive, times(1)).isDirectory();
    verify(mockFileFive, times(2)).isFile();
    verify(mockFileFive, times(1)).length();
    verify(mockFileFive, never()).listFiles(any(FileFilter.class));
  }

  @Test
  @SuppressWarnings("all")
  public void sizeOfDirectoryAndSubDirectoriesWithFilesWhenFiltered() {
    File mockSubDirectory = mockFile("MockSubDirectory");
    File mockFileOne = mockFile("MockFileOne");
    File mockFileTwo = mockFile("MockFileTwo");
    File mockFileThree = mockFile("MockFileThree");

    whenDirectory(mockFile, mockSubDirectory, mockFileOne);
    whenDirectory(mockSubDirectory, mockFileTwo, mockFileThree);
    whenFile(mockFileOne, 1024l);
    whenFile(mockFileTwo, 2048l);
    whenFile(mockFileThree, 4096l);
    when(mockFileOne.isHidden()).thenReturn(true);
    when(mockFileTwo.isHidden()).thenReturn(false);
    when(mockFileThree.isHidden()).thenReturn(true);

    assertThat(FileSystemUtils.size(mockFile, (file) -> !file.isHidden()), is(equalTo(2048l)));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).isFile();
    verify(mockFile, times(1)).isHidden();
    verify(mockFile, never()).length();
    verify(mockFile, times(1)).listFiles(isA(FileFilter.class));
    verify(mockSubDirectory, times(1)).isDirectory();
    verify(mockSubDirectory, times(1)).isFile();
    verify(mockSubDirectory, times(1)).isHidden();
    verify(mockSubDirectory, never()).length();
    verify(mockSubDirectory, times(1)).listFiles(isA(FileFilter.class));
    verify(mockFileOne, times(1)).isDirectory();
    verify(mockFileOne, times(1)).isFile();
    verify(mockFileOne, times(1)).isHidden();
    verify(mockFileOne, never()).length();
    verify(mockFileOne, never()).listFiles(any(FileFilter.class));
    verify(mockFileTwo, times(1)).isDirectory();
    verify(mockFileTwo, times(1)).isFile();
    verify(mockFileTwo, times(1)).isHidden();
    verify(mockFileTwo, times(1)).length();
    verify(mockFileTwo, never()).listFiles(any(FileFilter.class));
    verify(mockFileThree, times(1)).isDirectory();
    verify(mockFileThree, times(1)).isFile();
    verify(mockFileThree, times(1)).isHidden();
    verify(mockFileThree, never()).length();
    verify(mockFileThree, never()).listFiles(any(FileFilter.class));
  }

}
