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
import static org.assertj.core.api.Assumptions.assumeThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.Test;
import org.junit.runner.RunWith;

import org.cp.elements.lang.SystemUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.test.AbstractBaseTestSuite;
import org.cp.elements.test.annotation.IntegrationTest;

import org.mockito.ArgumentMatchers;
import org.mockito.Mock;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.junit.MockitoJUnitRunner;
import org.mockito.stubbing.Answer;

/**
 * Unit Tests for {@link FileSystemUtils}.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see java.io.FilenameFilter
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.cp.elements.io.FileSystemUtils
 * @see org.cp.elements.test.AbstractBaseTestSuite
 * @see org.cp.elements.test.TestUtils
 * @see org.cp.elements.test.annotation.IntegrationTest
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class FileSystemUtilsUnitTests extends AbstractBaseTestSuite {

  @Mock
  private File mockFile;

  @Mock
  private FileFilter mockFileFilter;

  private @NotNull File mockFile(String name) {
    return mock(File.class, name);
  }

  private @NotNull File newFile(File parent, File child) {
    return new File(parent, child.getAbsolutePath());
  }

  private @NotNull File whenDirectory(@NotNull File mockDirectory, File... files) {

    doReturn(true).when(mockDirectory).exists();
    doReturn(true).when(mockDirectory).isDirectory();
    doReturn(false).when(mockDirectory).isFile();
    doReturn(files).when(mockDirectory).listFiles();
    doReturn(files).when(mockDirectory).listFiles(ArgumentMatchers.<FileFilter>isNull());
    doReturn(files).when(mockDirectory).listFiles(any(FileFilter.class));

    return mockDirectory;
  }

  private @NotNull File whenFile(@NotNull File mockFile) {

    doReturn(true).when(mockFile).exists();
    doReturn(false).when(mockFile).isDirectory();
    doReturn(true).when(mockFile).isFile();

    return mockFile;
  }

  private @NotNull File whenFile(@NotNull File mockFile, boolean hidden) {

    whenFile(mockFile);
    doReturn(hidden).when(mockFile).isHidden();

    return mockFile;
  }

  private @NotNull File whenFile(@NotNull File mockFile, long length) {

    whenFile(mockFile);
    doReturn(length).when(mockFile).length();

    return mockFile;
  }

  private @NotNull File whenFile(@NotNull File mockFile, boolean hidden, long length) {

    whenFile(mockFile);
    doReturn(hidden).when(mockFile).isHidden();
    doReturn(length).when(mockFile).length();

    return mockFile;
  }

  @Test
  public void appendToPathWithBasePathAndPathElements() {

    assumeThat(SystemUtils.isUnixBasedOperatingSystem()).isTrue();

    assertThat(FileSystemUtils.appendToPath("/absolute/path/to",
      "a", "relative", "location", "containing", "file.ext"))
      .isEqualTo("/absolute/path/to/a/relative/location/containing/file.ext");
  }

  @Test
  public void appendToPathWithBasePathAndPathElementsContainingFileSeparators() {

    assumeThat(SystemUtils.isUnixBasedOperatingSystem()).isTrue();

    assertThat(FileSystemUtils.appendToPath("  ///base/pathname/////to// ", "//a////   ", " /relative/",
      "     /location ", " containing ", "//file.ext"))
      .isEqualTo("/base/pathname/to/a/relative/location/containing/file.ext");
  }

  @Test
  public void appendToPathWithBasePathAndEmptyPathElements() {

    assumeThat(SystemUtils.isUnixBasedOperatingSystem()).isTrue();

    assertThat(FileSystemUtils.appendToPath("/absolute/path/to", "  ", "", null, "\t", "\n"))
      .isEqualTo("/absolute/path/to");
  }

  @Test
  public void appendToPathWithBasePathAndNoPathElements() {

    assumeThat(SystemUtils.isUnixBasedOperatingSystem()).isTrue();

    assertThat(FileSystemUtils.appendToPath("/absolute/path/to/nothing")).isEqualTo("/absolute/path/to/nothing");
  }

  @Test
  public void appendToPathWithBasePathAndNullPathElementsIsNullSafe() {

    assumeThat(SystemUtils.isUnixBasedOperatingSystem()).isTrue();

    assertThat(FileSystemUtils.appendToPath("/absolute/path/to", (String[]) null))
      .isEqualTo("/absolute/path/to");
  }

  @Test
  public void appendToPathWithNullBasePath() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> FileSystemUtils.appendToPath(null, "relative", "path", "to", "file.ext"))
      .withMessage("Base path is required")
      .withNoCause();
  }

  @Test
  public void appendToPathWithEmptyBasePathAndPathElements() {

    assumeThat(SystemUtils.isUnixBasedOperatingSystem()).isTrue();

    assertThat(FileSystemUtils.appendToPath("", "relative", "path", "to", "file.ext"))
      .isEqualTo("/relative/path/to/file.ext");
  }

  @Test
  public void appendToPathWithBlankBasePathAndNullPathElements() {
    assertThat(FileSystemUtils.appendToPath("  ", (String[]) null))
      .isEqualTo(FileSystemUtils.resolveFileSeparator());
  }

  @Test
  public void createPathFromPathElements() {

    assumeThat(SystemUtils.isUnixBasedOperatingSystem()).isTrue();

    assertThat(FileSystemUtils.createPath("/absolute", "path", "to", "a", "file.ext"))
      .isEqualTo("/absolute/path/to/a/file.ext");

    assertThat(FileSystemUtils.createPath("relative", "path", "to", "a", "file.ext"))
      .isEqualTo("/relative/path/to/a/file.ext");
  }

  @Test
  public void createPathFromPathElementsContainingFileSeparators() {

    assumeThat(SystemUtils.isUnixBasedOperatingSystem()).isTrue();

    assertThat(FileSystemUtils.createPath("  ///absolute/", " /path////   ", "to  ", "/file.ext "))
      .isEqualTo("/absolute/path/to/file.ext");
  }

  @Test
  public void createPathFromEmptyPathElements() {

    assumeThat(SystemUtils.isUnixBasedOperatingSystem()).isTrue();

    assertThat(FileSystemUtils.createPath("", "null", " ", "nil", "  ")).isEqualTo("/null/nil");
    assertThat(FileSystemUtils.createPath(null, "", "  ", "\t", "\n")).isEqualTo("/");
    assertThat(FileSystemUtils.createPath()).isEqualTo("/");
  }

  @Test
  public void createPathFromNullPathElements() {

    assumeThat(SystemUtils.isUnixBasedOperatingSystem()).isTrue();

    assertThat(FileSystemUtils.createPath((String[]) null)).isEqualTo("/");
  }

  @Test
  @SuppressWarnings("all")
  public void countNonFilteredEmptyDirectoryReturnsZero() {

    assertThat(FileSystemUtils.count(whenDirectory(this.mockFile))).isZero();

    verify(this.mockFile, never()).exists();
    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, times(1)).listFiles(isA(FileFilter.class));
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void countNonFilteredNonEmptyDirectoryReturnsOne() {

    File mockFile = whenFile(mockFile("MockFileOne"));

    assertThat(FileSystemUtils.count(whenDirectory(this.mockFile, mockFile))).isOne();

    verify(this.mockFile, never()).exists();
    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, times(1)).listFiles(isA(FileFilter.class));
    verify(mockFile, times(1)).isDirectory();
    verifyNoMoreInteractions(this.mockFile, mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void countNonFilteredNonEmptyDirectoryReturnsTwo() {

    File mockFileOne = whenFile(mockFile("MockFileOne"));
    File mockFileTwo = whenFile(mockFile("MockFileTwo"));

    assertThat(FileSystemUtils.count(whenDirectory(this.mockFile, mockFileOne, mockFileTwo), null))
      .isEqualTo(2);

    verify(this.mockFile, never()).exists();
    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, times(1)).listFiles(ArgumentMatchers.<FileFilter>isNull());
    verify(mockFileOne, times(1)).isDirectory();
    verify(mockFileTwo, times(1)).isDirectory();
    verifyNoMoreInteractions(this.mockFile, mockFileOne, mockFileTwo);
  }

  @Test
  @SuppressWarnings("all")
  public void countFilteredEmptyDirectoryReturnsZero() {

    assertThat(FileSystemUtils.count(whenDirectory(this.mockFile), this.mockFileFilter)).isZero();

    verify(this.mockFile, never()).exists();
    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, times(1)).listFiles(eq(this.mockFileFilter));
    verifyNoInteractions(this.mockFileFilter);
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void countFilteredNonEmptyDirectoryReturnsOne() {

    File mockFile = whenFile(mockFile("MockFileOne"));

    assertThat(FileSystemUtils.count(whenDirectory(this.mockFile, mockFile), this.mockFileFilter)).isOne();

    verify(this.mockFile, never()).exists();
    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, times(1)).listFiles(eq(this.mockFileFilter));
    verify(mockFile, times(1)).isDirectory();
    verifyNoMoreInteractions(this.mockFile, mockFile);
    verifyNoInteractions(this.mockFileFilter);
  }

  @Test
  @SuppressWarnings("all")
  public void countFilteredNonEmptyDirectoryReturnsTwo() {

    File mockFileOne = whenFile(mockFile("MockFileOne"));
    File mockFileTwo = whenFile(mockFile("MockFileTwo"));

    assertThat(FileSystemUtils.count(whenDirectory(this.mockFile, mockFileOne, mockFileTwo), this.mockFileFilter))
      .isEqualTo(2);

    verify(this.mockFile, never()).exists();
    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, times(1)).listFiles(eq(this.mockFileFilter));
    verify(mockFileOne, times(1)).isDirectory();
    verify(mockFileTwo, times(1)).isDirectory();
    verifyNoMoreInteractions(this.mockFile, mockFileOne, mockFileTwo);
    verifyNoInteractions(this.mockFileFilter);
  }

  @Test
  @SuppressWarnings("all")
  public void countNonFilteredFileReturnsOne() {

    assertThat(FileSystemUtils.count(whenFile(this.mockFile))).isOne();

    verify(this.mockFile, times(1)).exists();
    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, never()).listFiles(any(FileFilter.class));
  }

  @Test
  @SuppressWarnings("all")
  public void countFilteredFileReturnsZero() {

    doReturn(false).when(this.mockFileFilter).accept(any());

    assertThat(FileSystemUtils.count(whenFile(this.mockFile), this.mockFileFilter)).isZero();

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFileFilter, times(1)).accept(eq(this.mockFile));
    verifyNoMoreInteractions(this.mockFile, this.mockFileFilter);
  }

  @Test
  public void countNullFileIsNullSafeReturnsZero() {
    assertThat(FileSystemUtils.count(null)).isZero();
  }

  @Test
  public void countNullFileWithNullFileFilterIsNullSafeReturnsZero() {
    assertThat(FileSystemUtils.count(null, null)).isZero();
  }

  @Test
  @SuppressWarnings("all")
  public void countNonFilteredNonEmptyDirectoryWithSubDirectoriesAndFiles() {

    File mockFileOne = mockFile("MockFileOne");
    File mockFileTwo = mockFile("MockFileTwo");
    File mockFileThree = mockFile("MockFileThree");
    File mockFileFour = mockFile("MockFileFour");
    File mockSubdirectoryOne = mockFile("MockSubdirectoryOne");
    File mockSubdirectoryTwo = mockFile("MockSubdirectoryTwo");

    whenDirectory(this.mockFile, whenDirectory(mockSubdirectoryOne, whenFile(mockFileFour)),
      whenDirectory(mockSubdirectoryTwo), whenFile(mockFileOne), whenFile(mockFileTwo), whenFile(mockFileThree));

    assertThat(FileSystemUtils.count(this.mockFile)).isEqualTo(4);

    verify(this.mockFile, never()).exists();
    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, times(1)).listFiles(isA(FileFilter.class));
    verify(mockSubdirectoryOne, never()).exists();
    verify(mockSubdirectoryOne, times(2)).isDirectory();
    verify(mockSubdirectoryOne, times(1)).isFile();
    verify(mockSubdirectoryOne, times(1)).listFiles(isA(FileFilter.class));
    verify(mockSubdirectoryTwo, never()).exists();
    verify(mockSubdirectoryTwo, times(2)).isDirectory();
    verify(mockSubdirectoryTwo, times(1)).isFile();
    verify(mockSubdirectoryTwo, times(1)).listFiles(isA(FileFilter.class));
    verify(mockFileOne, times(1)).isDirectory();
    verify(mockFileTwo, times(1)).isDirectory();
    verify(mockFileThree, times(1)).isDirectory();
    verify(mockFileFour, times(1)).isDirectory();
    verifyNoMoreInteractions(mockFileOne, mockFileTwo, mockFileThree, mockFileFour);
    verifyNoMoreInteractions(mockSubdirectoryOne, mockSubdirectoryTwo);
    verifyNoMoreInteractions(this.mockFile);
    verifyNoInteractions(this.mockFileFilter);
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
    File mockSubdirectoryFour = whenDirectory(mockFile("MockSubdirectoryFour"));
    File mockSubdirectoryOne = whenDirectory(mockFile("MockSubdirectoryOne"), mockFileFour, mockFileFive);
    File mockSubdirectoryThree = whenDirectory(mockFile("MockSubdirectoryThree"), mockSubdirectoryFour, mockFileSeven);
    File mockSubdirectoryTwo = whenDirectory(mockFile("MockSubdirectoryTwo"), mockSubdirectoryThree, mockFileSix);

    Answer<File[]> listFilesWithFilterAnswer = new Answer<File[]>() {

      @Override
      public File[] answer(InvocationOnMock invocationOnMock) throws Throwable {

        File directory = (File) invocationOnMock.getMock();
        FileFilter fileFilter = invocationOnMock.getArgument(0);

        List<File> files = new ArrayList<>();

        for (File file : directory.listFiles()) {
          if (fileFilter.accept(file)) {
            files.add(file);
          }
        }

        return files.toArray(new File[files.size()]);
      }
    };

    whenDirectory(this.mockFile, mockSubdirectoryOne, mockSubdirectoryTwo, mockFileOne, mockFileTwo, mockFileThree);
    doAnswer(listFilesWithFilterAnswer).when(this.mockFile).listFiles(any(FileFilter.class));
    doAnswer(listFilesWithFilterAnswer).when(mockSubdirectoryOne).listFiles(any(FileFilter.class));
    doAnswer(listFilesWithFilterAnswer).when(mockSubdirectoryTwo).listFiles(any(FileFilter.class));
    doAnswer(listFilesWithFilterAnswer).when(mockSubdirectoryThree).listFiles(any(FileFilter.class));
    doAnswer(listFilesWithFilterAnswer).when(mockSubdirectoryFour).listFiles(any(FileFilter.class));

    FileFilter expectedFileFilter = ComposableFileFilter.or(DirectoriesOnlyFilter.INSTANCE, file -> !file.isHidden());

    assertThat(FileSystemUtils.count(this.mockFile, expectedFileFilter)).isEqualTo(4);

    verify(this.mockFile, never()).exists();
    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, times(1)).listFiles(eq(expectedFileFilter));

    Arrays.asList(mockSubdirectoryOne, mockSubdirectoryTwo, mockSubdirectoryThree, mockSubdirectoryFour)
      .forEach(subdirectory -> {
        verify(subdirectory, times(3)).isDirectory();
        verify(subdirectory, times(1)).isFile();
        verify(subdirectory, times(1)).listFiles(eq(expectedFileFilter));
      });

    Arrays.asList(mockFileOne, mockFileTwo, mockFileThree, mockFileFour, mockFileFive, mockFileSix, mockFileSeven)
        .forEach(file -> {
          int isDirectoryCount = Arrays.asList(mockFileTwo, mockFileFour, mockFileSix).contains(file) ? 1 : 2;
          verify(file, times(isDirectoryCount)).isDirectory();
          verify(file, times(1)).isHidden();
          verifyNoMoreInteractions(file);
        });
  }

  @Test
  @IntegrationTest
  public void countJavaSourceFilesInProjectReturnsNonZeroCount() {

    FileFilter directoryAndJavaFileFilter = ComposableFileFilter.or(
      DirectoriesOnlyFilter.INSTANCE,
      new FileExtensionFilter(FileSystemUtils.JAVA_FILE_EXTENSION)
    );

    assertThat(FileSystemUtils.count(getSourceDirectory(), directoryAndJavaFileFilter)).isGreaterThanOrEqualTo(400);
  }

  @Test
  @IntegrationTest
  public void countGroovySourceFilesInProjectReturnsZero() {

    FileFilter directoryAndGroovyFileFilter = ComposableFileFilter.or(DirectoriesOnlyFilter.INSTANCE,
      new FileExtensionFilter(FileSystemUtils.GROOVY_FILE_EXTENSION));

    assertThat(FileSystemUtils.count(getSourceDirectory(), directoryAndGroovyFileFilter)).isZero();
  }

  @Test
  @IntegrationTest
  public void countKotlinSourceFilesInProjectReturnsZero() {

    FileFilter directoryAndKotlinFileFilter = ComposableFileFilter.or(DirectoriesOnlyFilter.INSTANCE,
      new FileExtensionFilter(FileSystemUtils.KOTLIN_FILE_EXTENSION));

    assertThat(FileSystemUtils.count(getSourceDirectory(), directoryAndKotlinFileFilter)).isZero();
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
    File mockSubdirectoryOne = whenDirectory(mockFile("MockSubdirectoryOne"), mockFileSix);
    File mockSubdirectoryTwo = whenDirectory(mockFile("MockSubdirectoryTwo"), mockSubdirectoryOne,
      mockFileFour, mockFileFive);
    File mockSubdirectoryThree = whenDirectory(mockFile("MockSubdirectoryThree"), mockFileSeven);

    Arrays.asList(mockFileOne, mockFileTwo, mockFileThree, mockFileFour, mockFileFive, mockFileSix, mockFileSeven)
        .forEach(mockFile -> doReturn(true).when(mockFile).delete());

    Arrays.asList(mockSubdirectoryOne, mockSubdirectoryTwo, mockSubdirectoryThree)
        .forEach(mockDirectory -> doReturn(true).when(mockDirectory).delete());

    doReturn(true).when(this.mockFile).delete();
    doReturn(true).when(this.mockFileFilter).accept(any(File.class));

    whenDirectory(this.mockFile, mockSubdirectoryTwo, mockSubdirectoryThree, mockFileOne, mockFileTwo, mockFileThree);

    assertThat(FileSystemUtils.deleteRecursive(this.mockFile, this.mockFileFilter)).isTrue();

    Arrays.asList(this.mockFile, mockSubdirectoryOne, mockSubdirectoryTwo, mockSubdirectoryThree)
        .forEach(mockDirectory -> {
          int isDirectoryCount = mockDirectory.equals(this.mockFile) ? 1 : 2;
          verify(mockDirectory, times(isDirectoryCount)).isDirectory();
          verify(mockDirectory, times(1)).listFiles(eq(this.mockFileFilter));
          verify(mockDirectory, times(1)).exists();
          verify(mockDirectory, times(1)).delete();
          verifyNoMoreInteractions(mockDirectory);
        });

    Arrays.asList(mockFileOne, mockFileTwo, mockFileThree, mockFileFour, mockFileFive, mockFileSix, mockFileSeven)
        .forEach(mockFile -> {
          verify(mockFile, times(1)).isDirectory();
          verify(mockFile, never()).listFiles(any(FileFilter.class));
          verify(mockFile, times(1)).exists();
          verify(mockFile, times(1)).delete();
          verifyNoMoreInteractions(mockFile);
        });

    Arrays.asList(this.mockFile, mockSubdirectoryOne, mockSubdirectoryTwo, mockSubdirectoryThree)
      .forEach(mockDirectory -> verify(this.mockFileFilter, times(1)).accept(eq(mockDirectory)));

    Arrays.asList(mockFileOne, mockFileTwo, mockFileThree, mockFileFour, mockFileFive, mockFileSix, mockFileSeven)
      .forEach(mockFile -> verify(this.mockFileFilter, never()).accept(eq(mockFile)));

    verifyNoMoreInteractions(this.mockFile, this.mockFileFilter);
  }

  @Test
  @SuppressWarnings("all")
  public void deleteRecursiveWithNonEmptyDirectoryReturnsTrue() {

    File mockFileOne = whenFile(mockFile("MockFileOne"));
    File mockFileTwo = whenFile(mockFile("MockFileTwo"));

    Arrays.asList(this.mockFile, mockFileOne, mockFileTwo)
      .forEach(mockFile -> doReturn(true).when(mockFile).delete());

    whenDirectory(this.mockFile, mockFileOne, mockFileTwo);

    assertThat(FileSystemUtils.deleteRecursive(this.mockFile)).isTrue();

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).listFiles(isA(FileFilter.class));
    verify(this.mockFile, times(2)).exists();
    verify(this.mockFile, times(1)).delete();
    verifyNoMoreInteractions(this.mockFile);

    Arrays.asList(mockFileOne, mockFileTwo).forEach(mockFile -> {
      verify(mockFile, times(1)).isDirectory();
      verify(mockFile, times(1)).exists();
      verify(mockFile, times(1)).delete();
      verifyNoMoreInteractions(mockFile);
    });
  }

  @Test
  @SuppressWarnings("all")
  public void deleteRecursiveWithFilteredNonEmptyDirectoryFailsToDeleteDirectoryAndReturnsFalse() {

    File mockFileOne = whenFile(mockFile("MockFileOne"));
    File mockFileTwo = whenFile(mockFile("MockFileTwo"));

    doReturn(false).when(this.mockFile).delete();
    doReturn(true).when(this.mockFileFilter).accept(any(File.class));

    Arrays.asList(mockFileOne, mockFileTwo).forEach(mockFile ->
      doReturn(true).when(mockFile).delete());

    whenDirectory(this.mockFile, mockFileOne, mockFileTwo);

    assertThat(FileSystemUtils.deleteRecursive(this.mockFile, this.mockFileFilter)).isFalse();

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).listFiles(eq(this.mockFileFilter));
    verify(this.mockFile, times(1)).exists();
    verify(this.mockFile, times(1)).delete();
    verify(this.mockFileFilter, times(1)).accept(eq(this.mockFile));
    verifyNoMoreInteractions(this.mockFile, this.mockFileFilter);

    Arrays.asList(mockFileOne, mockFileTwo).forEach(mockFile -> {
      verify(mockFile, times(1)).isDirectory();
      verify(mockFile, times(1)).exists();
      verify(mockFile, times(1)).delete();
      verifyNoMoreInteractions(mockFile);
    });
  }

  @Test
  @SuppressWarnings("all")
  public void deleteRecursiveWithFilteredNonEmptyDirectoryShortCircuitsAndReturnsFalse() {

    File mockFileOne = whenFile(mockFile("MockFileOne"));
    File mockFileTwo = whenFile(mockFile("MockFileTwo"));

    Arrays.asList(mockFileOne, mockFileTwo).forEach(mockFile ->
      doReturn(false).when(mockFile).delete());

    whenDirectory(this.mockFile, mockFileOne, mockFileTwo);

    assertThat(FileSystemUtils.deleteRecursive(this.mockFile, this.mockFileFilter)).isFalse();

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).listFiles(eq(this.mockFileFilter));
    verify(this.mockFile, never()).exists();
    verify(this.mockFile, never()).delete();
    verify(this.mockFileFilter, never()).accept(any(File.class));
    verifyNoMoreInteractions(this.mockFile);
    verifyNoInteractions(this.mockFileFilter);

    Arrays.asList(mockFileOne, mockFileTwo).forEach(mockFile -> {
      verify(mockFile, times(1)).isDirectory();
      verify(mockFile, times(1)).exists();
      verify(mockFile, times(1)).delete();
      verifyNoMoreInteractions(mockFile);
    });
  }

  @Test
  @SuppressWarnings("all")
  public void deleteRecursiveWithEmptyDirectoryReturnsTrue() {

    doReturn(true).when(this.mockFile).delete();

    assertThat(FileSystemUtils.deleteRecursive(whenDirectory(this.mockFile))).isTrue();

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).listFiles(isA(FileFilter.class));
    verify(this.mockFile, times(2)).exists();
    verify(this.mockFile, times(1)).delete();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void deleteRecursiveWithFilteredEmptyDirectoryReturnsFalse() {

    doReturn(false).when(this.mockFileFilter).accept(any(File.class));

    assertThat(FileSystemUtils.deleteRecursive(whenDirectory(this.mockFile), this.mockFileFilter)).isFalse();

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).listFiles(isA(FileFilter.class));
    verify(this.mockFile, never()).exists();
    verify(this.mockFile, never()).delete();
    verify(this.mockFileFilter, times(1)).accept(eq(this.mockFile));
    verifyNoMoreInteractions(this.mockFile, this.mockFileFilter);
  }

  @Test
  @SuppressWarnings("all")
  public void deleteRecursiveWithFileReturnsTrue() {

    doReturn(true).when(this.mockFile).delete();

    assertThat(FileSystemUtils.deleteRecursive(whenFile(this.mockFile))).isTrue();

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, never()).listFiles(any(FileFilter.class));
    verify(this.mockFile, times(2)).exists();
    verify(this.mockFile, times(1)).delete();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void deleteResusiveWithFilteredFileReturnsFalse() {

    doReturn(false).when(this.mockFileFilter).accept(any(File.class));

    assertThat(FileSystemUtils.deleteRecursive(whenFile(this.mockFile), this.mockFileFilter)).isFalse();

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, never()).listFiles(any(FileFilter.class));
    verify(this.mockFile, never()).exists();
    verify(this.mockFile, never()).delete();
    verify(this.mockFileFilter, times(1)).accept(eq(this.mockFile));
    verifyNoMoreInteractions(this.mockFile, this.mockFileFilter);
  }

  @Test
  public void deleteRecursiveWithNullFileIsNullSafeReturnsFalse() {
    assertThat(FileSystemUtils.deleteRecursive(null)).isFalse();
  }

  @Test
  public void deleteRecursiveWithNullFileAndNullFileFiltersIsNullSafeReturnsFalse() {
    assertThat(FileSystemUtils.deleteRecursive(null, null)).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void isEmptyDirectoryWithEmptyDirectoryReturnsTrue() {

    assertThat(FileSystemUtils.isEmptyDirectory(whenDirectory(this.mockFile))).isTrue();

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).listFiles();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void isEmptyDirectoryWithNonEmptyDirectoryReturnsFalse() {

    assertThat(FileSystemUtils.isEmptyDirectory(whenDirectory(this.mockFile, new File[10]))).isFalse();

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).listFiles();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void isEmptyDirectoryWithFileReturnsFalse() {

    assertThat(FileSystemUtils.isEmptyDirectory(whenFile(this.mockFile))).isFalse();

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, never()).isFile();
    verify(this.mockFile, never()).listFiles();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  public void isEmptyDirectoryWithNullIsNullSafeReturnsFalse() {
    assertThat(FileSystemUtils.isEmptyDirectory(null)).isFalse();
  }

  @Test
  public void isRelativeToWorkingDirectoryWithWorkingDirectoryReturnsTrue() {
    assertThat(FileSystemUtils.isRelativeToWorkingDirectory(WORKING_DIRECTORY)).isTrue();
  }

  @Test
  public void isRelativeToWorkingDirectoryWithRelativeFileReturnsTrue() {

    File relativeFile = new File(WORKING_DIRECTORY, "/relative/path/to/file.ext");

    assertThat(FileSystemUtils.isRelativeToWorkingDirectory(relativeFile)).isTrue();
  }

  @Test
  public void isRelativeToWorkingDirectoryWithParentOfWorkingDirectoryReturnsFalse() {
    assertThat(FileSystemUtils.isRelativeToWorkingDirectory(WORKING_DIRECTORY.getParentFile())).isFalse();
  }

  @Test
  public void isRelativeToWorkingDirectoryWithNonRelativeFileReturnsFalse() {

    File nonRelativeFile = new File("/non/relative/path/to/working/directory");

    assertThat(FileSystemUtils.isRelativeToWorkingDirectory(nonRelativeFile)).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void isRelativeToWorkingDirectoryWithChildOfWorkingDirectoryReturnsTrue() {
    assertThat(FileSystemUtils.isRelativeToWorkingDirectory(newFile(WORKING_DIRECTORY,
      WORKING_DIRECTORY.listFiles()[0]))).isTrue();
  }

  @Test
  public void isRelativeToWorkingDirectoryWithFileSystemUtilsClassReturnsTrue() {
    assertThat(FileSystemUtils.isRelativeToWorkingDirectory(getLocation(FileSystemUtils.class))).isTrue();
  }

  @Test
  public void isRelativeToWorkingDirectoryWithNullIsNullSafeReturnsFalse() {
    assertThat(FileSystemUtils.isRelativeToWorkingDirectory(null)).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void listFilesWithNonEmptyFilteredDirectory() {

    File mockFileOne = whenFile(mockFile("MockFileOne"));
    File mockFileTwo = whenFile(mockFile("MockFileTwo"));
    File mockFileThree = whenFile(mockFile("MockFileThree"));
    File mockSubdirectory = whenDirectory(mockFile("MockSubdirectory"), mockFileOne, mockFileTwo);

    whenDirectory(this.mockFile, mockSubdirectory, mockFileThree);

    assertThat(FileSystemUtils.listFiles(this.mockFile, this.mockFileFilter))
      .containsExactly(mockFileOne, mockFileTwo, mockFileThree);

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).listFiles(eq(this.mockFileFilter));
    verify(mockSubdirectory, times(2)).isDirectory();
    verify(mockSubdirectory, times(1)).listFiles(eq(this.mockFileFilter));
    verifyNoMoreInteractions(this.mockFile, mockSubdirectory);
    verifyNoInteractions(this.mockFileFilter);

    Arrays.asList(mockFileOne, mockFileTwo, mockFileThree).forEach(mockFile -> {
      verify(mockFile, times(1)).isDirectory();
      verifyNoMoreInteractions(mockFile);
    });
  }

  @Test
  @SuppressWarnings("all")
  public void listFilesWithNonEmptyDirectory() {

    File mockFileOne = whenFile(mockFile("MockFileOne"));
    File mockFileTwo = whenFile(mockFile("MockFileTwo"));
    File mockFileThree = whenFile(mockFile("MockFileThree"));
    File mockSubdirectory = whenDirectory(mockFile("MockSubdirectory"), mockFileOne, mockFileTwo);

    whenDirectory(this.mockFile, mockSubdirectory, mockFileThree);

    assertThat(FileSystemUtils.listFiles(this.mockFile)).containsExactly(mockFileOne, mockFileTwo, mockFileThree);

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).listFiles(isA(FileFilter.class));
    verify(mockSubdirectory, times(2)).isDirectory();
    verify(mockSubdirectory, times(1)).listFiles(isA(FileFilter.class));
    verifyNoMoreInteractions(this.mockFile, mockSubdirectory);

    Arrays.asList(mockFileOne, mockFileTwo, mockFileThree).forEach(mockFile -> {
      verify(mockFile, times(1)).isDirectory();
      verifyNoMoreInteractions(mockFile);
    });
  }

  @Test
  @SuppressWarnings("all")
  public void listFilesWithNonEmptyDirectoryAndNullFileFilterIsNullSafe() {

    File mockFileOne = whenFile(mockFile("MockFileOne"));
    File mockFileTwo = whenFile(mockFile("MockfileTwo"));

    assertThat(FileSystemUtils.listFiles(whenDirectory(this.mockFile, mockFileOne, mockFileTwo), null))
      .containsExactly(mockFileOne, mockFileTwo);

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).listFiles(ArgumentMatchers.<FileFilter>isNull());
    verifyNoMoreInteractions(this.mockFile);

    Arrays.asList(mockFileOne, mockFileTwo).forEach(mockFile -> {
      verify(mockFile, times(1)).isDirectory();
      verifyNoMoreInteractions(mockFile);
    });
  }

  @Test
  @SuppressWarnings("all")
  public void listFilesWithEmptyDirectory() {

    assertThat(FileSystemUtils.listFiles(whenDirectory(this.mockFile))).isEqualTo(FileSystemUtils.NO_FILES);

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).listFiles(isA(FileFilter.class));
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void listFilesWithFile() {

    assertThat(FileSystemUtils.listFiles(whenFile(this.mockFile), null)).isEqualTo(FileSystemUtils.NO_FILES);

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, never()).listFiles(any(FileFilter.class));
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  public void listFilesWithNullIsNullSafe() {
    assertThat(FileSystemUtils.listFiles(null, this.mockFileFilter)).isEqualTo(FileSystemUtils.NO_FILES);
  }

  @Test
  @SuppressWarnings("all")
  public void safeListFilesWithDirectory() {

    File mockFileOne = mockFile("MockFileOne");
    File mockFileTwo = mockFile("MockFileTwo");

    whenDirectory(this.mockFile, mockFileOne, mockFileTwo);

    assertThat(FileSystemUtils.safeListFiles(this.mockFile)).containsExactly(mockFileOne, mockFileTwo);

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).listFiles(isA(FileFilter.class));
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void safeListFilesWithDirectoryAndFileFilter() {

    File mockFileOne = mockFile("MockFileOne");
    File mockFileTwo = mockFile("MockFileTwo");

    whenDirectory(this.mockFile, mockFileOne, mockFileTwo);

    assertThat(FileSystemUtils.safeListFiles(this.mockFile, this.mockFileFilter))
      .containsExactly(mockFileOne, mockFileTwo);

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).listFiles(eq(this.mockFileFilter));
    verifyNoMoreInteractions(this.mockFile, this.mockFileFilter);
  }

  @Test
  @SuppressWarnings("all")
  public void safeListFilesWithFile() {

    assertThat(FileSystemUtils.safeListFiles(whenFile(this.mockFile))).isEqualTo(FileSystemUtils.NO_FILES);

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, never()).listFiles(any(FileFilter.class));
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  public void safeListFilesWithNullFileIsNullSafe() {
    assertThat(FileSystemUtils.safeListFiles(null)).isEqualTo(FileSystemUtils.NO_FILES);
  }

  @Test
  public void safeListFilesWithNullFileAndNullFileFilterIsNullSafe() {
    assertThat(FileSystemUtils.safeListFiles(null, null)).isEqualTo(FileSystemUtils.NO_FILES);
  }

  @Test
  @SuppressWarnings("all")
  public void sizeOfFile() {

    assertThat(FileSystemUtils.size(whenFile(this.mockFile, 1_024_000L))).isEqualTo(1_024_000L);

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, times(1)).exists();
    verify(this.mockFile, times(1)).length();
    verify(this.mockFile, never()).listFiles(any(FileFilter.class));
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void sizeOfFilteredFile() {

    doReturn(false).when(this.mockFileFilter).accept(any(File.class));

    assertThat(FileSystemUtils.size(whenFile(this.mockFile, 1_024L), this.mockFileFilter)).isZero();

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, never()).exists();
    verify(this.mockFile, never()).length();
    verify(this.mockFile, never()).listFiles(any(FileFilter.class));
    verify(this.mockFileFilter, times(1)).accept(eq(this.mockFile));
    verifyNoMoreInteractions(this.mockFile, this.mockFileFilter);
  }

  @Test
  @SuppressWarnings("all")
  public void sizeOfEmptyDirectory() {

    assertThat(FileSystemUtils.size(whenDirectory(this.mockFile))).isEqualTo(0L);

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, never()).exists();
    verify(this.mockFile, never()).length();
    verify(this.mockFile, times(1)).listFiles(isA(FileFilter.class));
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void sizeOfDirectoryWithOneFile() {

    File mockFile = whenFile(mockFile("MockFileOne"), 512L);

    assertThat(FileSystemUtils.size(whenDirectory(this.mockFile, mockFile))).isEqualTo(512L);

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, never()).exists();
    verify(this.mockFile, never()).length();
    verify(this.mockFile, times(1)).listFiles(isA(FileFilter.class));
    verify(mockFile, times(1)).isDirectory();
    verify(mockFile, times(1)).length();
    verifyNoMoreInteractions(this.mockFile, mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void sizeOfDirectoryWithTwoFiles() {

    File mockFileOne = whenFile(mockFile("MockFileOne"), 512L);
    File mockFileTwo = whenFile(mockFile("MockFileTwo"), 512L);

    assertThat(FileSystemUtils.size(whenDirectory(this.mockFile, mockFileOne, mockFileTwo))).isEqualTo(1_024L);

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, never()).exists();
    verify(this.mockFile, never()).length();
    verify(this.mockFile, times(1)).listFiles(isA(FileFilter.class));

    Arrays.asList(mockFileOne, mockFileTwo).forEach(mockFile -> {
      verify(mockFile, times(1)).isDirectory();
      verify(mockFile, times(1)).length();
    });

    verifyNoMoreInteractions(this.mockFile, mockFileOne, mockFileTwo);
  }

  @Test
  @SuppressWarnings("all")
  public void sizeOfDirectoryWithSubdirectoriesAndFiles() {

    File mockFileOne = whenFile(mockFile("MockFileOne"), 256L);
    File mockFileTwo = whenFile(mockFile("MockFileTwo"), 256L);
    File mockFileThree = whenFile(mockFile("MockFileThree"), 1_024L);
    File mockFileFour = whenFile(mockFile("MockFileFour"), 512L);
    File mockFileFive = whenFile(mockFile("MockFileFive"), 2_048L);
    File mockSubdirectoryThree = whenDirectory(mockFile("MockSubDirectoryThree"), mockFileFour, mockFileFive);
    File mockSubdirectoryTwo = whenDirectory(mockFile("MockSubDirectoryTwo"), mockFileThree);
    File mockSubdirectoryOne = whenDirectory(mockFile("MockSubDirectoryOne"), mockSubdirectoryThree, mockFileTwo);

    whenDirectory(this.mockFile, mockSubdirectoryOne, mockSubdirectoryTwo, mockFileOne);

    assertThat(FileSystemUtils.size(this.mockFile)).isEqualTo(4_096l);

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, never()).exists();
    verify(this.mockFile, never()).length();
    verify(this.mockFile, times(1)).listFiles(isA(FileFilter.class));
    verifyNoMoreInteractions(this.mockFile);

    Arrays.asList(mockSubdirectoryOne, mockSubdirectoryTwo, mockSubdirectoryThree).forEach(mockSubdirectory -> {
      verify(mockSubdirectory, times(2)).isDirectory();
      verify(mockSubdirectory, times(1)).isFile();
      verify(mockSubdirectory, never()).exists();
      verify(mockSubdirectory, never()).length();
      verify(mockSubdirectory, times(1)).listFiles(isA(FileFilter.class));
      verifyNoMoreInteractions(mockSubdirectory);
    });

    Arrays.asList(mockFileOne, mockFileTwo, mockFileThree, mockFileFour, mockFileFive).forEach(mockFile -> {
      verify(mockFile, times(1)).isDirectory();
      verify(mockFile, times(1)).length();
      verifyNoMoreInteractions(mockFile);
    });
  }

  @Test
  @SuppressWarnings("all")
  public void sizeOfDirectoryWithSubdirectoriesAndFilesWhenFiltered() {

    File mockFileOne = whenFile(mockFile("MockFileOne"), true, 1_024L);
    File mockFileTwo = whenFile(mockFile("MockFileTwo"), false, 2_048L);
    File mockFileThree = whenFile(mockFile("MockFileThree"), true, 4_096L);
    File mockSubdirectory = whenDirectory(mockFile("MockSubdirectory"), mockFileTwo, mockFileThree);

    Answer<File[]> listFilesWithFileFilterAnswer = new Answer<File[]>() {

      @Override
      public File[] answer(InvocationOnMock invocationOnMock) throws Throwable {

        File directory = (File) invocationOnMock.getMock();
        FileFilter fileFilter = invocationOnMock.getArgument(0);

        List<File> files = new ArrayList<>();

        for (File file : directory.listFiles()) {
          if (fileFilter.accept(file)) {
            files.add(file);
          }
        }

        return files.toArray(new File[files.size()]);
      }
    };

    whenDirectory(this.mockFile, mockSubdirectory, mockFileOne);

    doAnswer(listFilesWithFileFilterAnswer).when(this.mockFile).listFiles(any(FileFilter.class));
    doAnswer(listFilesWithFileFilterAnswer).when(mockSubdirectory).listFiles(any(FileFilter.class));

    FileFilter expectedFileFilter = ComposableFileFilter.or(DirectoriesOnlyFilter.INSTANCE, file -> !file.isHidden());

    assertThat(FileSystemUtils.size(this.mockFile, expectedFileFilter)).isEqualTo(2_048L);
    //assertThat(FileSystemUtils.size(this.mockFile)).isEqualTo(2_048L);

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, never()).isHidden();
    verify(this.mockFile, never()).length();
    verify(this.mockFile, times(1)).listFiles();
    verify(this.mockFile, times(1)).listFiles(eq(expectedFileFilter));
    verify(mockSubdirectory, times(3)).isDirectory();
    verify(mockSubdirectory, times(1)).isFile();
    verify(mockSubdirectory, never()).isHidden();
    verify(mockSubdirectory, never()).length();
    verify(mockSubdirectory, times(1)).listFiles();
    verify(mockSubdirectory, times(1)).listFiles(eq(expectedFileFilter));
    verifyNoMoreInteractions(this.mockFile, mockSubdirectory);

    Arrays.asList(mockFileOne, mockFileThree).forEach(mockFile -> {
      verify(mockFile, times(1)).isDirectory();
      verify(mockFile, never()).isFile();
      verify(mockFile, times(1)).isHidden();
      verify(mockFile, never()).length();
      verifyNoMoreInteractions(mockFile);
    });

    verify(mockFileTwo, times(2)).isDirectory();
    verify(mockFileTwo, never()).isFile();
    verify(mockFileTwo, times(1)).isHidden();
    verify(mockFileTwo, times(1)).length();
    verifyNoMoreInteractions(mockFileTwo);
  }
}
