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
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.assertj.core.api.Assertions.assertThatIllegalStateException;
import static org.cp.elements.lang.CheckedExceptionsFactory.newIOException;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileFilter;
import java.io.FileNotFoundException;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.net.URL;

import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;

import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.test.AbstractBaseTestSuite;
import org.cp.elements.test.annotation.IntegrationTest;

import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit Tests for {@link FileUtils}.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.InputStream
 * @see java.net.URL
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.cp.elements.io.FileUtils
 * @see org.cp.elements.test.AbstractBaseTestSuite
 * @see org.cp.elements.test.annotation.IntegrationTest
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class FileUtilsUnitTests extends AbstractBaseTestSuite {

  @Mock
  private File mockFile;

  @BeforeClass
  public static void beforeTestSuite() {
    FileSystemUtils.delete(new File(TEMPORARY_DIRECTORY, "writeToFileThenReadFromFileIsSuccessful.txt"));
  }

  private @NotNull File newFile(@NotNull String pathname) {
    return new File(pathname);
  }

  private @NotNull File newFile(@Nullable File parent, @NotNull String pathname) {
    return new File(parent, pathname);
  }

  @Test
  @IntegrationTest
  public void assertExistsWithExistingDirectory() throws FileNotFoundException {

    assertThat(FileUtils.assertExists(TEMPORARY_DIRECTORY)).isEqualTo(TEMPORARY_DIRECTORY);
    assertThat(FileUtils.assertExists(USER_HOME)).isEqualTo(USER_HOME);
    assertThat(FileUtils.assertExists(WORKING_DIRECTORY)).isEqualTo(WORKING_DIRECTORY);
  }

  @Test
  public void assertExistsWithExistingFile() throws FileNotFoundException {

    doReturn(true).when(this.mockFile).exists();

    assertThat(FileUtils.assertExists(this.mockFile)).isSameAs(this.mockFile);

    verify(this.mockFile, times(1)).exists();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  public void assertExistsWithNonExistingDirectory() {

    assertThatExceptionOfType(FileNotFoundException.class)
      .isThrownBy(() -> FileUtils.assertExists(newFile("/absolute/path/to/non/existing/directory")))
      .withMessage("[/absolute/path/to/non/existing/directory] was not found")
      .withNoCause();
  }

  @Test
  public void assertExistsWithNonExistingFile() {

    assertThatExceptionOfType(FileNotFoundException.class)
      .isThrownBy(() -> FileUtils.assertExists(newFile("relative/path/to/non/existing/file.ext")))
      .withMessage("[relative/path/to/non/existing/file.ext] was not found")
      .withNoCause();
  }

  @Test
  public void assertExistsWithNullIsNullSafe() {

    assertThatExceptionOfType(FileNotFoundException.class)
      .isThrownBy(() -> FileUtils.assertExists(null))
      .withMessage("[null] was not found")
      .withNoCause();
  }

  @Test
  @SuppressWarnings("all")
  public void createDirectoryWithNonExistingDirectory() {

    doReturn(false).when(this.mockFile).isDirectory();
    doReturn(false).when(this.mockFile).isFile();
    doReturn(true).when(this.mockFile).mkdirs();

    assertThat(FileUtils.createDirectory(this.mockFile)).isTrue();

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, times(1)).mkdirs();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void createDirectoryWithExistingDirectory() {

    doReturn(true).when(this.mockFile).isDirectory();
    doReturn(false).when(this.mockFile).isFile();

    assertThat(FileUtils.createDirectory(this.mockFile)).isTrue();

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, never()).mkdirs();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void createDirectoryWithExistingFile() {

    doReturn(true).when(this.mockFile).isFile();

    assertThat(FileUtils.createDirectory(this.mockFile)).isFalse();

    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, never()).isDirectory();
    verify(this.mockFile, never()).mkdirs();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  public void createDirectoryWithNullIsNullSafe() {
    assertThat(FileUtils.createDirectory(null)).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void createFileWithNonExistingFile() throws IOException {

    doReturn(false).when(this.mockFile).isDirectory();
    doReturn(false).when(this.mockFile).isFile();
    doReturn(true).when(this.mockFile).createNewFile();

    assertThat(FileUtils.createFile(this.mockFile)).isTrue();

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, times(1)).createNewFile();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void createFileWithExistingDirectory() throws IOException {

    doReturn(true).when(this.mockFile).isDirectory();

    assertThat(FileUtils.createFile(this.mockFile)).isFalse();

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, never()).isFile();
    verify(this.mockFile, never()).createNewFile();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void createFileWithExistingFile() throws IOException {

    doReturn(false).when(this.mockFile).isDirectory();
    doReturn(true).when(this.mockFile).isFile();

    assertThat(FileUtils.createFile(this.mockFile)).isTrue();

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, never()).createNewFile();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  public void createFileWithNullIsNullSafe() {
    assertThat(FileUtils.createFile(null)).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void createFileThrowsIOExceptionOnFileCreateNewFile() throws IOException {

    doReturn(false).when(this.mockFile).isDirectory();
    doReturn(false).when(this.mockFile).isFile();
    doThrow(newIOException("TEST")).when(this.mockFile).createNewFile();

    assertThat(FileUtils.createFile(this.mockFile)).isFalse();

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, times(1)).createNewFile();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void deleteExistingFileIsSuccessful() {

    doReturn(true).when(this.mockFile).exists();
    doReturn(true).when(this.mockFile).delete();

    assertThat(FileUtils.delete(this.mockFile)).isTrue();

    verify(this.mockFile, times(1)).exists();
    verify(this.mockFile, times(1)).delete();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void deleteExistingFileIsUnsuccessful() {

    doReturn(true).when(this.mockFile).exists();
    doReturn(false).when(this.mockFile).delete();

    assertThat(FileUtils.delete(this.mockFile)).isFalse();

    verify(this.mockFile, times(1)).exists();
    verify(this.mockFile, times(1)).delete();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void deleteNonExistingFileIsUnsuccessful() {

    doReturn(false).when(this.mockFile).exists();

    assertThat(FileUtils.delete(this.mockFile)).isFalse();

    verify(this.mockFile, times(1)).exists();
    verify(this.mockFile, never()).delete();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  public void deleteNullIsNullSafe() {
    assertThat(FileUtils.delete(null)).isFalse();
  }

  @Test
  public void getExtensionFromFilesWithExtension() {

    assertThat(FileUtils.getExtension(newFile("/absolute/path/to/file.ext"))).isEqualTo("ext");
    assertThat(FileUtils.getExtension(newFile("relative/path/to/file.ext"))).isEqualTo("ext");
    assertThat(FileUtils.getExtension(newFile("FileUtils.java"))).isEqualTo("java");
    assertThat(FileUtils.getExtension(newFile("FileUtilsUnitTests.class"))).isEqualTo("class");
    assertThat(FileUtils.getExtension(newFile("search.c"))).isEqualTo("c");
    assertThat(FileUtils.getExtension(newFile("sort.cpp"))).isEqualTo("cpp");
    assertThat(FileUtils.getExtension(newFile("/path/to/file/with/two/extensions/test.java.class")))
      .isEqualTo("java.class");
  }

  @Test
  public void getExtensionFromFilesWithNoExtension() {

    assertThat(FileUtils.getExtension(newFile("file"))).isEqualTo(FileUtils.NO_FILE_EXTENSION);
    assertThat(FileUtils.getExtension(newFile("file."))).isEqualTo(FileUtils.NO_FILE_EXTENSION);
    assertThat(FileUtils.getExtension(newFile("exe"))).isEqualTo(FileUtils.NO_FILE_EXTENSION);
  }

  @Test
  public void getExtensionFromFilesWithNoNameAndNoExtension() {

    assertThat(FileUtils.getExtension(newFile("  "))).isEqualTo(FileUtils.NO_FILE_EXTENSION);
    assertThat(FileUtils.getExtension(newFile(""))).isEqualTo(FileUtils.NO_FILE_EXTENSION);
  }

  @Test
  public void getExtensionWithNullThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> FileUtils.getExtension(null))
      .withMessage("File is required")
      .withNoCause();
  }

  @Test
  public void getLocationFromStandardFileLocations() {

    //assertThat(FileUtils.getLocation(TEMPORARY_DIRECTORY)).isEqualTo(TEMPORARY_DIRECTORY.getParent());
    assertThat(FileUtils.getLocation(USER_HOME)).isEqualTo(USER_HOME.getParent());
    assertThat(FileUtils.getLocation(WORKING_DIRECTORY)).isEqualTo(WORKING_DIRECTORY.getParent());
  }

  @Test
  public void getLocationFromFilesWithLocation() {

    assertThat(FileUtils.getLocation(newFile("/absolute/path/to/file.ext"))).isEqualTo("/absolute/path/to");
    assertThat(FileUtils.getLocation(newFile("relative/path/to/file.ext")))
      .isEqualTo(WORKING_DIRECTORY.getAbsolutePath().concat(File.separator).concat("relative/path/to"));
    assertThat(FileUtils.getLocation(newFile("/location/to/a/file/system/directory")))
      .isEqualTo("/location/to/a/file/system");
    assertThat(FileUtils.getLocation(newFile("/location/to/a/file/system/directory/")))
      .isEqualTo("/location/to/a/file/system");
  }

  @Test
  public void getLocationFromFileWithNoLocation() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> FileUtils.getLocation(newFile("file.ext")))
      .withMessage("Location of file [file.ext] cannot be determined")
      .withNoCause();
  }

  @Test
  public void getLocationWithNullThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> FileUtils.getLocation(null))
      .withMessage("File is required")
      .withNoCause();
  }

  @Test
  public void getNameOfFilesWithName() {

    assertThat(FileUtils.getName(newFile("/absolute/path/to/file.ext"))).isEqualTo("file");
    assertThat(FileUtils.getName(newFile("relative/path/to/file.ext"))).isEqualTo("file");
    assertThat(FileUtils.getName(newFile("FileUtilsUnitTests.java"))).isEqualTo("FileUtilsUnitTests");
    assertThat(FileUtils.getName(newFile("FileUtils.class"))).isEqualTo("FileUtils");
    assertThat(FileUtils.getName(newFile("search.c"))).isEqualTo("search");
    assertThat(FileUtils.getName(newFile("sort.cpp"))).isEqualTo("sort");
    assertThat(FileUtils.getName(newFile("/path/to/file/with/two/extensions/test.java.class"))).isEqualTo("test");
  }

  @Test
  public void getNameOfFilesWithNoName() {

    assertThat(FileUtils.getName(newFile(".exe"))).isEqualTo(FileUtils.NO_FILE_NAME);
    assertThat(FileUtils.getName(newFile("."))).isEqualTo(FileUtils.NO_FILE_NAME);
    assertThat(FileUtils.getName(newFile("  "))).isEqualTo(FileUtils.NO_FILE_NAME);
    assertThat(FileUtils.getName(newFile(""))).isEqualTo(FileUtils.NO_FILE_NAME);
  }

  @Test
  public void getNameWithNullThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> FileUtils.getName(null))
      .withMessage("File is required")
      .withNoCause();
  }

  @Test
  public void isDirectoryWithDirectories() {

    assertThat(FileUtils.isDirectory(TEMPORARY_DIRECTORY)).isTrue();
    assertThat(FileUtils.isDirectory(USER_HOME)).isTrue();
    assertThat(FileUtils.isDirectory(WORKING_DIRECTORY)).isTrue();
  }

  @Test
  public void isDirectoryWithNonDirectories() {

    assertThat(FileUtils.isDirectory(newFile(TEMPORARY_DIRECTORY, "non_existing_directory/"))).isFalse();
    assertThat(FileUtils.isDirectory(newFile(USER_HOME, "nonExistingFile.ext"))).isFalse();
    assertThat(FileUtils.isDirectory(newFile(WORKING_DIRECTORY, "cp-elements-1.0.0.jar"))).isFalse();
  }

  @Test
  public void isDirectoryWithNullIsNullSafe() {
    assertThat(FileUtils.isDirectory(null)).isFalse();
  }

  @Test
  public void isEmptyWithExistingNonEmptyDirectoryReturnsFalse() {

    assertThat(FileUtils.isEmpty(USER_HOME)).isFalse();
    assertThat(FileUtils.isEmpty(WORKING_DIRECTORY)).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void isEmptyWithExistingNonEmptyFileReturnsFalse() {

    doReturn(true).when(this.mockFile).isFile();
    doReturn(1L).when(this.mockFile).length();

    assertThat(FileUtils.isEmpty(this.mockFile)).isFalse();

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, times(1)).length();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void isEmptyWithExistingEmptyFileReturnsTrue() {

    doReturn(true).when(this.mockFile).isFile();
    doReturn(0L).when(this.mockFile).length();

    assertThat(FileUtils.isEmpty(this.mockFile)).isTrue();

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, times(1)).length();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void isEmptyWithNonExistingFileReturnsTrue() {

    doReturn(false).when(this.mockFile).isFile();

    assertThat(FileUtils.isEmpty(this.mockFile)).isTrue();

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, never()).length();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  public void isEmptyWithNullFileIsNullSafeReturnsTrue() {
    assertThat(FileUtils.isEmpty(null)).isTrue();
  }

  @Test
  public void isNotEmptyWithExistingNonEmptyDirectoryReturnsTrue() {

    assertThat(FileUtils.isNotEmpty(USER_HOME)).isTrue();
    assertThat(FileUtils.isNotEmpty(WORKING_DIRECTORY)).isTrue();
  }

  @Test
  public void isNotEmptyWithExistingEmptyDirectoryReturnsFalse() {

    File emptyDirectory = newFile(TEMPORARY_DIRECTORY, "emptyDirectory");

    try {
      assertThat(emptyDirectory.mkdir()).isTrue();
      assertThat(emptyDirectory).isDirectory();
      assertThat(FileUtils.isNotEmpty(emptyDirectory)).isFalse();
    }
    finally {
      assertThat(emptyDirectory.delete()).isTrue();
    }
  }

  @Test
  public void isNotEmptyWithExistingNonEmptyFileReturnsTrue() {
    assertThat(FileUtils.isNotEmpty(getLocation(FileUtils.class))).isTrue();
  }

  @Test
  public void isNotEmptyWithExistingEmptyFileReturnsFalse() throws IOException {

    File emptyFile = File.createTempFile("emptyFile", "txt", TEMPORARY_DIRECTORY);

    try {
      assertThat(emptyFile).isNotNull();
      assertThat(emptyFile).isFile();
      assertThat(FileUtils.isNotEmpty(emptyFile)).isFalse();
    }
    finally {
      assertThat(emptyFile.delete()).isTrue();
    }
  }

  @Test
  public void isNotEmptyWithNonExistingFileReturnsFalse() {
    assertThat(FileUtils.isNotEmpty(newFile(WORKING_DIRECTORY, "nonExistingPathname"))).isFalse();
  }

  @Test
  public void isNotEmptyWithNullFileIsNullSafeReturnsFalse() {
    assertThat(FileUtils.isNotEmpty(null)).isFalse();
  }

  @Test
  public void isExistingWithExistingDirectory() {

    assertThat(FileUtils.isExisting(TEMPORARY_DIRECTORY)).isTrue();
    assertThat(FileUtils.isExisting(USER_HOME)).isTrue();
    assertThat(FileUtils.isExisting(WORKING_DIRECTORY)).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void isExistingWithExistingFile() {

    doReturn(true).when(this.mockFile).exists();

    assertThat(FileUtils.isExisting(this.mockFile)).isTrue();

    verify(this.mockFile, never()).isDirectory();
    verify(this.mockFile, never()).isFile();
    verify(this.mockFile, times(1)).exists();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  public void isExistingWithNonExistingDirectory() {
    assertThat(FileUtils.isExisting(newFile("/absolute/path/to/non/existing/directory"))).isFalse();
  }

  @Test
  public void isExistingWithNonExistingFile() {
    assertThat(FileUtils.isExisting(newFile("relative/path/to/non/existing/file.ext"))).isFalse();
  }

  @Test
  public void isExistingWithNullIsNullSafe() {
    assertThat(FileUtils.isExisting(null)).isFalse();
  }

  @Test
  public void isNonExistingWithNonExistingDirectoryReturnsTrue() {
    assertThat(FileUtils.isNonExisting(newFile("/absolute/path/to/non/existing/directory"))).isTrue();
  }

  @Test
  public void isNonExistingWithNonExistingFileReturnsTrue() {
    assertThat(FileUtils.isNonExisting(newFile("relative/path/to/non/existing/file.ext"))).isTrue();
  }

  @Test
  public void isNonExistingWithExistingDirectoryReturnsFalse() {
    assertThat(FileUtils.isNonExisting(WORKING_DIRECTORY)).isFalse();
  }

  @Test
  public void isNonExistingWithExistingFileReturnsFalse() {
    assertThat(FileUtils.isNonExisting(getLocation(FileUtils.class))).isFalse();
  }

  @Test
  public void isNonExistingWithNullFileIsNullSafeReturnsTrue() {
    assertThat(FileUtils.isNonExisting(null)).isTrue();
  }

  @Test
  public void isFileWithFile() {

    doReturn(true).when(this.mockFile).isFile();

    assertThat(FileUtils.isFile(this.mockFile)).isTrue();

    verify(this.mockFile, never()).isDirectory();
    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, never()).exists();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  public void isFileWithNonFiles() {

    assertThat(FileUtils.isFile(WORKING_DIRECTORY)).isFalse();
    assertThat(FileUtils.isFile(newFile("/absolute/path/to/non/existing/directory/"))).isFalse();
    assertThat(FileUtils.isFile(newFile("relative/path/to/non/existing/file.ext"))).isFalse();
  }

  @Test
  public void isFileWithNullIsNullSafeReturnsFalse() {
    assertThat(FileUtils.isFile(null)).isFalse();
  }

  @Test
  public void newFileWithNonNullNonExistingFileIsSuccessful() {

    File file = FileUtils.newFile("/absolute/path/to/file.ext");

    assertThat(file).isNotNull();
    assertThat(file.exists()).isFalse();
    assertThat(file.getAbsolutePath()).isEqualTo("/absolute/path/to/file.ext");
  }

  @Test
  @IntegrationTest
  public void newFileWithNonNullExistingDirectoryIsSuccessful() {

    assertThat(WORKING_DIRECTORY).isDirectory();

    File workingDirectory = FileUtils.newFile(WORKING_DIRECTORY.getAbsolutePath());

    assertThat(workingDirectory).isNotNull();
    assertThat(workingDirectory).isDirectory();
    assertThat(workingDirectory).isEqualTo(WORKING_DIRECTORY);
  }

  @Test
  @IntegrationTest
  public void newFileWithNonNullExistingFileIsSuccessful() {

    File fileUtilsClass = getLocation(FileUtils.class);

    assertThat(fileUtilsClass).isNotNull();
    assertThat(fileUtilsClass).isFile();
    assertThat(FileUtils.newFile(fileUtilsClass.getAbsolutePath())).isEqualTo(fileUtilsClass);
  }

  @Test
  public void newFileWithNullPathnameThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> FileUtils.newFile(null))
      .withMessage("A pathname for the File is required")
      .withNoCause();
  }

  @Test
  public void nullSafeFileFilterWithNonNullFileFilter() {

    FileFilter mockFileFilter = mock(FileFilter.class);

    assertThat(FileUtils.nullSafeFileFilter(mockFileFilter)).isSameAs(mockFileFilter);

    verifyNoInteractions(mockFileFilter);
  }

  @Test
  public void nullSafeFileFilterWithNullFileFilterAcceptsFiles() {

    FileFilter fileFilter = FileUtils.nullSafeFileFilter(null);

    assertThat(fileFilter).isNotNull();
    assertThat(fileFilter.accept(newFile("test"))).isTrue();
  }

  @Test
  public void nullSafeFileFilterWithNullFileFilterRejectsFiles() {

    FileFilter fileFilter = FileUtils.nullSafeFileFilter(null, false);

    assertThat(fileFilter).isNotNull();
    assertThat(fileFilter.accept(newFile("test"))).isFalse();
  }

  @Test
  public void nullSafeFilenameFilterWithNonNullFilenameFilter() {

    FilenameFilter mockFilenameFilter = mock(FilenameFilter.class);

    assertThat(FileUtils.nullSafeFilenameFilter(mockFilenameFilter)).isSameAs(mockFilenameFilter);

    verifyNoInteractions(mockFilenameFilter);
  }

  @Test
  public void nullSafeFilenameFilterWithNullFilenameFilterAcceptsFiles() {

    FilenameFilter filenameFilter = FileUtils.nullSafeFilenameFilter(null);

    assertThat(filenameFilter).isNotNull();
    assertThat(filenameFilter.accept(WORKING_DIRECTORY, "file.ext")).isTrue();
  }

  @Test
  public void nullSafeFilenameFilterWithNullFilenameFilterRejectsFiles() {

    FilenameFilter filenameFilter = FileUtils.nullSafeFilenameFilter(null, false);

    assertThat(filenameFilter).isNotNull();
    assertThat(filenameFilter.accept(WORKING_DIRECTORY, "file.ext")).isFalse();
  }

  @Test
  @IntegrationTest
  public void readFromFile() throws IOException, URISyntaxException {

    URL testFileTxtUrl = getClass().getClassLoader().getResource("testFile.txt");

    assertThat(testFileTxtUrl).isNotNull();

    File testFileTxt = new File(testFileTxtUrl.toURI());

    assertThat(testFileTxt).isNotNull();
    assertThat(testFileTxt).isFile();

    String contents = FileUtils.read(testFileTxt);

    assertThat(contents).isNotEmpty();
    assertThat(contents).isEqualTo(String.format("I solemnly swear that I am up to no good!%sMischief Managed!",
      StringUtils.LINE_SEPARATOR));
  }

  @Test
  public void readFromNullFile() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> FileUtils.read(null))
      .withMessage("[null] must be a file")
      .withNoCause();
  }

  @Test
  public void readFromUnreadableFile() throws IOException {

    doReturn(true).when(this.mockFile).isFile();
    doReturn(false).when(this.mockFile).canRead();
    doReturn("/path/to/file.ext").when(this.mockFile).getCanonicalPath();

    assertThatIllegalStateException()
      .isThrownBy(() -> FileUtils.read(this.mockFile))
      .withMessage("[/path/to/file.ext] is not readable")
      .withNoCause();

    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, times(1)).canRead();
    verify(this.mockFile, times(1)).getCanonicalPath();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  public void sizeOfExistingDirectory() {
    assertThat(FileUtils.size(WORKING_DIRECTORY)).isEqualTo(0L);
  }

  @Test
  @SuppressWarnings("all")
  public void sizeOfExistingFile() {

    doReturn(true).when(this.mockFile).isFile();
    doReturn(1L).when(this.mockFile).length();

    assertThat(FileUtils.size(this.mockFile)).isEqualTo(1L);

    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, times(1)).length();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  public void sizeOfNonExistingDirectory() {
    assertThat(FileUtils.size(new File("/absolute/path/to/non/existing/directory"))).isZero();
  }

  @Test
  @SuppressWarnings("all")
  public void sizeOfNonExistingFile() {

    doReturn(false).when(this.mockFile).isFile();

    assertThat(FileUtils.size(this.mockFile)).isEqualTo(0L);

    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, never()).length();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  public void sizeOfNullIsNullSafe() {
    assertThat(FileUtils.size(null)).isEqualTo(0L);
  }

  @Test
  @SuppressWarnings("all")
  public void tryGetCanonicalFileElseGetAbsoluteFile() throws IOException {

    doReturn(this.mockFile).when(this.mockFile).getCanonicalFile();

    assertThat(FileUtils.tryGetCanonicalFileElseGetAbsoluteFile(this.mockFile)).isEqualTo(this.mockFile);

    verify(this.mockFile, never()).getAbsoluteFile();
    verify(this.mockFile, times(1)).getCanonicalFile();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void tryGetCanonicalFileElseGetAbsoluteFileWhenGetCanonicalFileThrowsIOException() throws IOException {

    File mockAbsoluteFile = mock(File.class);

    doReturn(mockAbsoluteFile).when(this.mockFile).getAbsoluteFile();
    doThrow(newIOException("test")).when(this.mockFile).getCanonicalFile();

    assertThat(FileUtils.tryGetCanonicalFileElseGetAbsoluteFile(this.mockFile)).isEqualTo(mockAbsoluteFile);

    verify(this.mockFile, times(1)).getAbsoluteFile();
    verify(this.mockFile, times(1)).getCanonicalFile();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  public void tryGetCanonicalPathElseGetAbsolutePath() throws IOException {

    doReturn("/path/to/file.ext").when(this.mockFile).getCanonicalPath();

    assertThat(FileUtils.tryGetCanonicalPathElseGetAbsolutePath(this.mockFile)).isEqualTo("/path/to/file.ext");

    verify(this.mockFile, never()).getAbsolutePath();
    verify(this.mockFile, times(1)).getCanonicalPath();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  public void tryGetCanonicalPathElseGetAbsolutePathWhenGetCanonicalPathThrowsIOException() throws IOException {

    doReturn("/path/to/file.ext").when(this.mockFile).getAbsolutePath();
    doThrow(newIOException("test")).when(this.mockFile).getCanonicalPath();

    assertThat(FileUtils.tryGetCanonicalPathElseGetAbsolutePath(this.mockFile)).isEqualTo("/path/to/file.ext");

    verify(this.mockFile, times(1)).getAbsolutePath();
    verify(this.mockFile, times(1)).getCanonicalPath();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  public void writeNullInputStreamToFile() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> FileUtils.write(null, this.mockFile))
      .withMessage("InputStream is required")
      .withNoCause();

    verifyNoInteractions(this.mockFile);
  }

  @Test
  public void writeInputStreamToNullFile() {

    InputStream mockInputStream = mock(InputStream.class);

    assertThatIllegalArgumentException()
      .isThrownBy(() -> FileUtils.write(mockInputStream, null))
      .withMessage("File is required")
      .withNoCause();

    verifyNoInteractions(mockInputStream);
  }

  @Test
  public void writeToExistingUnwritableFile() throws IOException {

    doReturn(true).when(this.mockFile).exists();
    doReturn(false).when(this.mockFile).canWrite();
    doReturn("/path/to/file.ext").when(this.mockFile).getCanonicalPath();

    InputStream mockInputStream = mock(InputStream.class);

    assertThatIllegalStateException()
      .isThrownBy(() -> FileUtils.write(mockInputStream, this.mockFile))
      .withMessage("[/path/to/file.ext] is not writable")
      .withNoCause();

    verify(this.mockFile, times(1)).exists();
    verify(this.mockFile, times(1)).canWrite();
    verify(this.mockFile, times(1)).getCanonicalPath();
    verifyNoInteractions(mockInputStream);
  }

  @Test
  @IntegrationTest
  public void writeToFileThenReadFromFileIsSuccessful() throws IOException {

    String expectedContent = "This is a test of the read/write File operations in FileUtils!";

    InputStream in = new ByteArrayInputStream(expectedContent.getBytes());

    File testFile = newFile(TEMPORARY_DIRECTORY, "writeToFileThenReadFromFileIsSuccessful.txt");

    assertThat(testFile).isNotNull();
    assertThat(testFile).doesNotExist();

    testFile.deleteOnExit();

    assertThat(FileUtils.write(in, testFile)).isSameAs(testFile);
    assertThat(testFile).isFile();
    assertThat(testFile.length()).isGreaterThan(0L);

    String actualContent = FileUtils.read(testFile);

    assertThat(actualContent).isEqualTo(expectedContent);
  }
}
