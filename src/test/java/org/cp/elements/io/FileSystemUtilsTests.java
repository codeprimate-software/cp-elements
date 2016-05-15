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
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.FileFilter;
import java.io.FilenameFilter;
import java.util.ArrayList;
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
 * of the {@link FileSystemUtils} class.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see java.io.FilenameFilter
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.rules.ExpectedException
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

  @SafeVarargs
  protected static <T> T[] asArray(T... array) {
    return array;
  }

  @SafeVarargs
  protected static <T> List<T> asList(T... array) {
    return Arrays.asList(array);
  }

  protected File mockFile(String name) {
    return mock(File.class, name);
  }

  protected File newFile(File parent, File child) {
    return new File(parent, child.getAbsolutePath());
  }

  protected File whenDirectory(File mockDirectory, File... files) {
    when(mockDirectory.exists()).thenReturn(true);
    when(mockDirectory.isDirectory()).thenReturn(true);
    when(mockDirectory.isFile()).thenReturn(false);
    when(mockDirectory.listFiles()).thenReturn(files);
    when(mockDirectory.listFiles(any(FileFilter.class))).thenReturn(files);
    when(mockDirectory.listFiles(any(FilenameFilter.class))).thenReturn(files);
    return mockDirectory;
  }

  protected File whenFile(File mockFile) {
    when(mockFile.exists()).thenReturn(true);
    when(mockFile.isDirectory()).thenReturn(false);
    when(mockFile.isFile()).thenReturn(true);
    return mockFile;
  }

  protected File whenFile(File mockFile, boolean hidden) {
    whenFile(mockFile);
    when(mockFile.isHidden()).thenReturn(hidden);
    return mockFile;
  }

  protected File whenFile(File mockFile, long length) {
    whenFile(mockFile);
    when(mockFile.length()).thenReturn(length);
    return mockFile;
  }

  protected File whenFile(File mockFile, boolean hidden, long length) {
    whenFile(mockFile);
    when(mockFile.isHidden()).thenReturn(hidden);
    when(mockFile.length()).thenReturn(length);
    return mockFile;
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
    exception.expect(IllegalArgumentException.class);
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

  @Test
  @SuppressWarnings("all")
  public void countNonFilteredEmptyDirectoryReturnsZero() {
    assertThat(FileSystemUtils.count(whenDirectory(mockFile)), is(equalTo(0)));

    verify(mockFile, never()).exists();
    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).isFile();
    verify(mockFile, times(1)).listFiles(isA(FileFilter.class));
  }

  @Test
  @SuppressWarnings("all")
  public void countNonFilteredNonEmptyDirectoryReturnsOne() {
    File mockFileOne = whenFile(mockFile("MockFileOne"));

    assertThat(FileSystemUtils.count(whenDirectory(mockFile, mockFileOne)), is(equalTo(1)));

    verify(mockFile, never()).exists();
    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).isFile();
    verify(mockFile, times(1)).listFiles(isA(FileFilter.class));
    verify(mockFileOne, times(1)).isDirectory();
    verifyNoMoreInteractions(mockFileOne);
  }

  @Test
  @SuppressWarnings("all")
  public void countNonFilteredNonEmptyDirectoryReturnsTwo() {
    File mockFileOne = whenFile(mockFile("MockFileOne"));
    File mockFileTwo = whenFile(mockFile("MockFileTwo"));

    assertThat(FileSystemUtils.count(whenDirectory(mockFile, mockFileOne, mockFileTwo), null), is(equalTo(2)));

    verify(mockFile, never()).exists();
    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).isFile();
    verify(mockFile, times(1)).listFiles(isNull(FileFilter.class));
    verify(mockFileOne, times(1)).isDirectory();
    verifyNoMoreInteractions(mockFileOne);
    verify(mockFileTwo, times(1)).isDirectory();
    verifyNoMoreInteractions(mockFileTwo);
  }

  @Test
  @SuppressWarnings("all")
  public void countFilteredEmptyDirectoryReturnsZero() {
    when(mockFileFilter.accept(any(File.class))).thenReturn(true);

    assertThat(FileSystemUtils.count(whenDirectory(mockFile), mockFileFilter), is(equalTo(0)));

    verify(mockFile, never()).exists();
    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).isFile();
    verify(mockFile, times(1)).listFiles(eq(mockFileFilter));
    verifyZeroInteractions(mockFileFilter);
  }

  @Test
  @SuppressWarnings("all")
  public void countFilteredNonEmptyDirectoryReturnsOne() {
    File mockFileOne = whenFile(mockFile("MockFileOne"));

    assertThat(FileSystemUtils.count(whenDirectory(mockFile, mockFileOne), mockFileFilter), is(equalTo(1)));

    verify(mockFile, never()).exists();
    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).isFile();
    verify(mockFile, times(1)).listFiles(eq(mockFileFilter));
    verify(mockFileOne, times(1)).isDirectory();
    verify(mockFileOne, never()).listFiles(any(FileFilter.class));
    verifyZeroInteractions(mockFileFilter);
  }

  @Test
  @SuppressWarnings("all")
  public void countNonFilteredFileReturnsOne() {
    assertThat(FileSystemUtils.count(whenFile(mockFile)), is(equalTo(1)));

    verify(mockFile, times(1)).exists();
    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).isFile();
    verify(mockFile, never()).listFiles(any(FileFilter.class));
  }

  @Test
  @SuppressWarnings("all")
  public void countFilteredFileReturnsZero() {
    when(mockFileFilter.accept(any(File.class))).thenReturn(false);

    assertThat(FileSystemUtils.count(whenFile(mockFile), mockFileFilter), is(equalTo(0)));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).isFile();
    verify(mockFileFilter, times(1)).accept(eq(mockFile));
  }

  @Test
  public void countNullReturnsZero() {
    assertThat(FileSystemUtils.count(null), is(equalTo(0)));
  }

  @Test
  @SuppressWarnings("all")
  public void countNonFilteredNonEmptyDirectoryWithSubDirectoriesAndFiles() {
    File mockSubDirectoryOne = mockFile("MockSubDirectoryOne");
    File mockSubDirectoryTwo = mockFile("MockSubDirectoryTwo");
    File mockFileOne = mockFile("MockFileOne");
    File mockFileTwo = mockFile("MockFileTwo");
    File mockFileThree = mockFile("MockFileThree");
    File mockFileFour = mockFile("MockFileFour");

    whenDirectory(mockFile, whenDirectory(mockSubDirectoryOne, whenFile(mockFileFour)),
      whenDirectory(mockSubDirectoryTwo), whenFile(mockFileOne), whenFile(mockFileTwo), whenFile(mockFileThree));

    assertThat(FileSystemUtils.count(mockFile), is(equalTo(4)));

    verify(mockFile, never()).exists();
    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).isFile();
    verify(mockFile, times(1)).listFiles(isA(FileFilter.class));
    verify(mockSubDirectoryOne, never()).exists();
    verify(mockSubDirectoryOne, times(2)).isDirectory();
    verify(mockSubDirectoryOne, times(1)).isFile();
    verify(mockSubDirectoryOne, times(1)).listFiles(isA(FileFilter.class));
    verify(mockSubDirectoryTwo, never()).exists();
    verify(mockSubDirectoryTwo, times(2)).isDirectory();
    verify(mockSubDirectoryTwo, times(1)).isFile();
    verify(mockSubDirectoryTwo, times(1)).listFiles(isA(FileFilter.class));
    verify(mockFileOne, times(1)).isDirectory();
    verify(mockFileOne, never()).listFiles(any(FileFilter.class));
    verify(mockFileTwo, times(1)).isDirectory();
    verify(mockFileTwo, never()).listFiles(any(FileFilter.class));
    verify(mockFileThree, times(1)).isDirectory();
    verify(mockFileThree, never()).listFiles(any(FileFilter.class));
    verify(mockFileFour, times(1)).isDirectory();
    verify(mockFileFour, never()).listFiles(any(FileFilter.class));
    verifyZeroInteractions(mockFileFilter);
  }

  @Test
  @SuppressWarnings("all")
  public void countFilteredNonEmptyDirectoryWithSubDirectoriesAndFiles() {
    File mockFileOne = whenFile(mockFile("MockFileOne"), false);
    File mockFileTwo = whenFile(mockFile("MockFileTwo"), true);
    File mockFileThree = whenFile(mockFile("MockFileThree"), false);
    File mockFileFour = whenFile(mockFile("MockFileFour"), true);
    File mockFileFive = whenFile(mockFile("MockFileFive"), false);
    File mockFileSix = whenFile(mockFile("MockFileSix"), true);
    File mockFileSeven = whenFile(mockFile("MockFileSeven"), false);
    File mockSubDirectoryFour = whenDirectory(mockFile("MockSubDirectoryFour"));
    File mockSubDirectoryThree = whenDirectory(mockFile("MockSubDirectoryThree"), mockSubDirectoryFour, mockFileSeven);
    File mockSubDirectoryTwo = whenDirectory(mockFile("MockSubDirectoryTwo"), mockSubDirectoryThree, mockFileSix);
    File mockSubDirectoryOne = whenDirectory(mockFile("MockSubDirectoryOne"), mockFileFour, mockFileFive);

    Answer<File[]> listFilesWithFilterAnswer = new Answer<File[]>() {
      @Override public File[] answer(InvocationOnMock invocationOnMock) throws Throwable {
        File directory = (File) invocationOnMock.getMock();
        FileFilter fileFilter = invocationOnMock.getArgumentAt(0, FileFilter.class);

        List<File> files = new ArrayList<>();

        for (File file : directory.listFiles()) {
          if (fileFilter.accept(file)) {
            files.add(file);
          }
        }

        return files.toArray(new File[files.size()]);
      }
    };

    whenDirectory(mockFile, mockSubDirectoryOne, mockSubDirectoryTwo, mockFileOne, mockFileTwo, mockFileThree);
    when(mockFile.listFiles(any(FileFilter.class))).then(listFilesWithFilterAnswer);
    when(mockSubDirectoryOne.listFiles(any(FileFilter.class))).then(listFilesWithFilterAnswer);
    when(mockSubDirectoryTwo.listFiles(any(FileFilter.class))).then(listFilesWithFilterAnswer);
    when(mockSubDirectoryThree.listFiles(any(FileFilter.class))).then(listFilesWithFilterAnswer);
    when(mockSubDirectoryFour.listFiles(any(FileFilter.class))).then(listFilesWithFilterAnswer);

    FileFilter expectedFileFilter = ComposableFileFilter.or(DirectoryOnlyFilter.INSTANCE, (file) -> !file.isHidden());

    assertThat(FileSystemUtils.count(mockFile, expectedFileFilter), is(equalTo(4)));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).isFile();
    verify(mockFile, times(1)).listFiles(eq(expectedFileFilter));
    verify(mockSubDirectoryOne, times(3)).isDirectory();
    verify(mockSubDirectoryOne, times(1)).isFile();
    verify(mockSubDirectoryOne, times(1)).listFiles(eq(expectedFileFilter));
    verify(mockSubDirectoryTwo, times(3)).isDirectory();
    verify(mockSubDirectoryTwo, times(1)).isFile();
    verify(mockSubDirectoryTwo, times(1)).listFiles(eq(expectedFileFilter));
    verify(mockSubDirectoryThree, times(3)).isDirectory();
    verify(mockSubDirectoryThree, times(1)).isFile();
    verify(mockSubDirectoryThree, times(1)).listFiles(eq(expectedFileFilter));
    verify(mockSubDirectoryFour, times(3)).isDirectory();
    verify(mockSubDirectoryFour, times(1)).isFile();
    verify(mockSubDirectoryFour, times(1)).listFiles(eq(expectedFileFilter));
    verify(mockFileOne, times(2)).isDirectory();
    verify(mockFileOne, times(1)).isHidden();
    verifyNoMoreInteractions(mockFileOne);
    verify(mockFileTwo, times(1)).isDirectory();
    verify(mockFileTwo, times(1)).isHidden();
    verifyNoMoreInteractions(mockFileTwo);
    verify(mockFileThree, times(2)).isDirectory();
    verify(mockFileThree, times(1)).isHidden();
    verifyNoMoreInteractions(mockFileThree);
    verify(mockFileFour, times(1)).isDirectory();
    verify(mockFileFour, times(1)).isHidden();
    verifyNoMoreInteractions(mockFileFour);
    verify(mockFileFive, times(2)).isDirectory();
    verify(mockFileFive, times(1)).isHidden();
    verifyNoMoreInteractions(mockFileFive);
    verify(mockFileSix, times(1)).isDirectory();
    verify(mockFileSix, times(1)).isHidden();
    verifyNoMoreInteractions(mockFileSix);
    verify(mockFileSeven, times(2)).isDirectory();
    verify(mockFileSeven, times(1)).isHidden();
    verifyNoMoreInteractions(mockFileSeven);
  }

  @Test
  @IntegrationTest
  public void countJavaSourceFilesInProjectReturnsNonZeroCount() {
    FileFilter directoryAndJavaFileFilter = ComposableFileFilter.or(DirectoryOnlyFilter.INSTANCE,
      new FileExtensionFilter("java"));

    assertThat(FileSystemUtils.count(getSourceDirectory(), directoryAndJavaFileFilter),
      is(greaterThanOrEqualTo(398)));
  }

  @Test
  @IntegrationTest
  public void countGroovySourceFilesInProjectReturnsZero() {
    FileFilter directoryAndGroovyFileFilter = ComposableFileFilter.or(DirectoryOnlyFilter.INSTANCE,
      new FileExtensionFilter("groovy"));

    assertThat(FileSystemUtils.count(getSourceDirectory(), directoryAndGroovyFileFilter), is(equalTo(0)));
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

    assertThat(FileSystemUtils.deleteRecursive(whenDirectory(mockFile), mockFileFilter), is(false));

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

    assertThat(FileSystemUtils.deleteRecursive(whenFile(mockFile), mockFileFilter), is(false));

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
    assertThat(FileSystemUtils.isEmptyDirectory(whenDirectory(mockFile)), is(equalTo(true)));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).listFiles();
  }

  @Test
  @SuppressWarnings("all")
  public void isEmptyDirectoryWithNonEmptyDirectoryIsFalse() {
    assertThat(FileSystemUtils.isEmptyDirectory(whenDirectory(mockFile, new File[10])), is(equalTo(false)));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).listFiles();
  }

  @Test
  @SuppressWarnings("all")
  public void isEmptyDirectoryWithFileIsFalse() {
    assertThat(FileSystemUtils.isEmptyDirectory(whenFile(mockFile)), is(false));

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

    assertThat(FileSystemUtils.listFiles(whenDirectory(mockFile, mockSubDirectory, mockFileThree), mockFileFilter),
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

    assertThat(FileSystemUtils.listFiles(whenDirectory(mockFile, mockSubDirectory, mockFileThree)),
      is(equalTo(asArray(mockFileOne, mockFileTwo, mockFileThree))));

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

    assertThat(FileSystemUtils.listFiles(whenDirectory(mockFile, mockFileOne, mockFileTwo), null),
      is(equalTo(asArray(mockFileOne, mockFileTwo))));

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
    assertThat(FileSystemUtils.listFiles(whenDirectory(mockFile)), is(equalTo(FileSystemUtils.NO_FILES)));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).listFiles(isA(FileFilter.class));
  }

  @Test
  @SuppressWarnings("all")
  public void listFilesWithFile() {
    assertThat(FileSystemUtils.listFiles(whenFile(mockFile), null), is(equalTo(FileSystemUtils.NO_FILES)));

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

    assertThat(FileSystemUtils.safeListFiles(whenDirectory(mockFile, mockFileOne, mockFileTwo)),
      is(equalTo(asArray(mockFileOne, mockFileTwo))));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).listFiles(isA(FileFilter.class));
  }

  @Test
  @SuppressWarnings("all")
  public void safeListFilesWithDirectoryAndFileFilter() {
    File mockFileOne = mockFile("MockFileOne");
    File mockFileTwo = mockFile("MockFileTwo");

    assertThat(FileSystemUtils.safeListFiles(whenDirectory(mockFile, mockFileOne, mockFileTwo), mockFileFilter),
      is(equalTo(asArray(mockFileOne, mockFileTwo))));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).listFiles(eq(mockFileFilter));
  }

  @Test
  @SuppressWarnings("all")
  public void safeListFilesWithFile() {
    assertThat(FileSystemUtils.safeListFiles(whenFile(mockFile)), is(equalTo(FileSystemUtils.NO_FILES)));

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
    assertThat(FileSystemUtils.size(whenFile(mockFile, 1024000l)), is(equalTo(1024000l)));

    verify(mockFile, times(1)).exists();
    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).isFile();
    verify(mockFile, times(1)).length();
    verify(mockFile, never()).listFiles(any(FileFilter.class));
  }

  @Test
  @SuppressWarnings("all")
  public void sizeOfFilteredFile() {
    when(mockFileFilter.accept(any(File.class))).thenReturn(false);

    assertThat(FileSystemUtils.size(whenFile(mockFile, 1024l), mockFileFilter), is(equalTo(0l)));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).isFile();
    verify(mockFile, never()).length();
    verify(mockFile, never()).listFiles(any(FileFilter.class));
    verify(mockFileFilter, times(1)).accept(eq(mockFile));
  }

  @Test
  @SuppressWarnings("all")
  public void sizeOfEmptyDirectory() {
    when(whenDirectory(mockFile).length()).thenReturn(1l);

    assertThat(FileSystemUtils.size(mockFile), is(equalTo(0l)));

    verify(mockFile, never()).exists();
    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).isFile();
    verify(mockFile, never()).length();
    verify(mockFile, times(1)).listFiles(isA(FileFilter.class));
  }

  @Test
  @SuppressWarnings("all")
  public void sizeOfDirectoryWithOneFile() {
    File mockFileOne = whenFile(mockFile("MockFileOne"), 512l);

    assertThat(FileSystemUtils.size(whenDirectory(mockFile, mockFileOne)), is(equalTo(512l)));

    verify(mockFile, never()).exists();
    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).isFile();
    verify(mockFile, never()).length();
    verify(mockFile, times(1)).listFiles(isA(FileFilter.class));
    verify(mockFileOne, times(1)).isDirectory();
    verify(mockFileOne, times(1)).length();
    verify(mockFileOne, never()).listFiles(any(FileFilter.class));
  }

  @Test
  @SuppressWarnings("all")
  public void sizeOfDirectoryWithTwoFiles() {
    File mockFileOne = whenFile(mockFile("MockFileOne"), 512l);
    File mockFileTwo = whenFile(mockFile("MockFileTwo"), 512l);

    assertThat(FileSystemUtils.size(whenDirectory(mockFile, mockFileOne, mockFileTwo)), is(equalTo(1024l)));

    verify(mockFile, never()).exists();
    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).isFile();
    verify(mockFile, never()).length();
    verify(mockFile, times(1)).listFiles(isA(FileFilter.class));
    verify(mockFileOne, times(1)).isDirectory();
    verify(mockFileOne, times(1)).length();
    verify(mockFileOne, never()).listFiles(any(FileFilter.class));
    verify(mockFileTwo, times(1)).isDirectory();
    verify(mockFileTwo, times(1)).length();
    verify(mockFileTwo, never()).listFiles(any(FileFilter.class));
  }

  @Test
  @SuppressWarnings("all")
  public void sizeOfDirectoryWithSubDirectoriesAndFiles() {
    File mockFileOne = whenFile(mockFile("MockFileOne"), 256l);
    File mockFileTwo = whenFile(mockFile("MockFileTwo"), 256l);
    File mockFileThree = whenFile(mockFile("MockFileThree"), 1024l);
    File mockFileFour = whenFile(mockFile("MockFileFour"), 512l);
    File mockFileFive = whenFile(mockFile("MockFileFive"), 2048l);
    File mockSubDirectoryThree = whenDirectory(mockFile("MockSubDirectoryThree"), mockFileFour, mockFileFive);
    File mockSubDirectoryTwo = whenDirectory(mockFile("MockSubDirectoryTwo"), mockFileThree);
    File mockSubDirectoryOne = whenDirectory(mockFile("MockSubDirectoryOne"), mockSubDirectoryThree, mockFileTwo);

    assertThat(FileSystemUtils.size(whenDirectory(mockFile, mockSubDirectoryOne, mockSubDirectoryTwo, mockFileOne)),
      is(equalTo(4096l)));

    verify(mockFile, never()).exists();
    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).isFile();
    verify(mockFile, never()).length();
    verify(mockFile, times(1)).listFiles(isA(FileFilter.class));
    verify(mockSubDirectoryOne, never()).exists();
    verify(mockSubDirectoryOne, times(2)).isDirectory();
    verify(mockSubDirectoryOne, times(1)).isFile();
    verify(mockSubDirectoryOne, never()).length();
    verify(mockSubDirectoryOne, times(1)).listFiles(isA(FileFilter.class));
    verify(mockSubDirectoryTwo, never()).exists();
    verify(mockSubDirectoryTwo, times(2)).isDirectory();
    verify(mockSubDirectoryTwo, times(1)).isFile();
    verify(mockSubDirectoryTwo, never()).length();
    verify(mockSubDirectoryTwo, times(1)).listFiles(isA(FileFilter.class));
    verify(mockSubDirectoryThree, never()).exists();
    verify(mockSubDirectoryThree, times(2)).isDirectory();
    verify(mockSubDirectoryThree, times(1)).isFile();
    verify(mockSubDirectoryThree, never()).length();
    verify(mockSubDirectoryThree, times(1)).listFiles(isA(FileFilter.class));
    verify(mockFileOne, times(1)).isDirectory();
    verify(mockFileOne, times(1)).length();
    verify(mockFileOne, never()).listFiles(any(FileFilter.class));
    verify(mockFileTwo, times(1)).isDirectory();
    verify(mockFileTwo, times(1)).length();
    verify(mockFileTwo, never()).listFiles(any(FileFilter.class));
    verify(mockFileThree, times(1)).isDirectory();
    verify(mockFileThree, times(1)).length();
    verify(mockFileThree, never()).listFiles(any(FileFilter.class));
    verify(mockFileFour, times(1)).isDirectory();
    verify(mockFileFour, times(1)).length();
    verify(mockFileFour, never()).listFiles(any(FileFilter.class));
    verify(mockFileFive, times(1)).isDirectory();
    verify(mockFileFive, times(1)).length();
    verify(mockFileFive, never()).listFiles(any(FileFilter.class));
  }

  @Test
  @SuppressWarnings("all")
  public void sizeOfDirectoryWithSubDirectoriesAndFilesWhenFiltered() {
    File mockFileOne = whenFile(mockFile("MockFileOne"), true, 1024l);
    File mockFileTwo = whenFile(mockFile("MockFileTwo"), false, 2048l);
    File mockFileThree = whenFile(mockFile("MockFileThree"), true, 4096l);
    File mockSubDirectory = whenDirectory(mockFile("MockSubDirectory"), mockFileTwo, mockFileThree);

    Answer<File[]> listFilesWithFilterAnswer = new Answer<File[]>() {
      @Override public File[] answer(InvocationOnMock invocationOnMock) throws Throwable {
        File directory = (File) invocationOnMock.getMock();
        FileFilter fileFilter = invocationOnMock.getArgumentAt(0, FileFilter.class);

        List<File> files = new ArrayList<>();

        for (File file : directory.listFiles()) {
          if (fileFilter.accept(file)) {
            files.add(file);
          }
        }

        return files.toArray(new File[files.size()]);
      }
    };

    whenDirectory(mockFile, mockSubDirectory, mockFileOne);
    when(mockFile.listFiles(any(FileFilter.class))).then(listFilesWithFilterAnswer);
    when(mockSubDirectory.listFiles(any(FileFilter.class))).then(listFilesWithFilterAnswer);

    FileFilter expectedFileFilter = ComposableFileFilter.or(DirectoryOnlyFilter.INSTANCE, (file) -> !file.isHidden());

    assertThat(FileSystemUtils.size(mockFile, expectedFileFilter),
      is(equalTo(2048l)));

    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).isFile();
    verify(mockFile, never()).isHidden();
    verify(mockFile, never()).length();
    verify(mockFile, times(1)).listFiles(eq(expectedFileFilter));
    verify(mockSubDirectory, times(3)).isDirectory();
    verify(mockSubDirectory, times(1)).isFile();
    verify(mockSubDirectory, never()).isHidden();
    verify(mockSubDirectory, never()).length();
    verify(mockSubDirectory, times(1)).listFiles(eq(expectedFileFilter));
    verify(mockFileOne, times(1)).isDirectory();
    verify(mockFileOne, never()).isFile();
    verify(mockFileOne, times(1)).isHidden();
    verify(mockFileOne, never()).length();
    verify(mockFileOne, never()).listFiles(any(FileFilter.class));
    verify(mockFileTwo, times(2)).isDirectory();
    verify(mockFileTwo, never()).isFile();
    verify(mockFileTwo, times(1)).isHidden();
    verify(mockFileTwo, times(1)).length();
    verify(mockFileTwo, never()).listFiles(any(FileFilter.class));
    verify(mockFileThree, times(1)).isDirectory();
    verify(mockFileThree, never()).isFile();
    verify(mockFileThree, times(1)).isHidden();
    verify(mockFileThree, never()).length();
    verify(mockFileThree, never()).listFiles(any(FileFilter.class));
  }
}
