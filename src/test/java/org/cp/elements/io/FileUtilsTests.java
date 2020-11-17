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

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.net.URL;

import org.cp.elements.lang.StringUtils;
import org.cp.elements.test.AbstractBaseTestSuite;
import org.cp.elements.test.annotation.IntegrationTest;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit Tests for {@link FileUtils}.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.net.URL
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.rules.ExpectedException
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
public class FileUtilsTests extends AbstractBaseTestSuite {

  @Mock
  private File mockFile;

  @BeforeClass
  public static void beforeTestSuite() {
    FileSystemUtils.delete(new File(TEMPORARY_DIRECTORY, "writeToFileThenReadFromFileIsSuccessful.txt"));
  }

  private File newFile(String pathname) {
    return new File(pathname);
  }

  private File newFile(File parent, String pathname) {
    return new File(parent, pathname);
  }

  @Test
  public void assertExistsWithExistingDirectory() throws FileNotFoundException {
    assertThat(FileUtils.assertExists(WORKING_DIRECTORY)).isEqualTo(WORKING_DIRECTORY);
  }

  @Test
  @SuppressWarnings("all")
  public void assertExistsWithExistingFile() throws FileNotFoundException {

    when(this.mockFile.exists()).thenReturn(true);

    assertThat(FileUtils.assertExists(this.mockFile)).isEqualTo(this.mockFile);

    verify(this.mockFile, times(1)).exists();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test(expected = FileNotFoundException.class)
  public void assertExistsWithNonExistingDirectory() throws FileNotFoundException {

    try {
      FileUtils.assertExists(newFile("/absolute/path/to/non/existing/directory"));
    }
    catch (FileNotFoundException expected) {

      assertThat(expected).hasMessage("[/absolute/path/to/non/existing/directory] was not found");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = FileNotFoundException.class)
  public void assertExistsWithNonExistingFile() throws FileNotFoundException {

    try {
      FileUtils.assertExists(newFile("relative/path/to/non/existing/file.ext"));
    }
    catch (FileNotFoundException expected) {

      assertThat(expected).hasMessage("[relative/path/to/non/existing/file.ext] was not found");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = FileNotFoundException.class)
  public void assertExistsWithNull() throws FileNotFoundException {

    try {
      FileUtils.assertExists(null);
    }
    catch (FileNotFoundException expected) {

      assertThat(expected).hasMessage("[null] was not found");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  @SuppressWarnings("all")
  public void createDirectoryWithNonExistingDirectory() {

    when(this.mockFile.isDirectory()).thenReturn(false);
    when(this.mockFile.isFile()).thenReturn(false);
    when(this.mockFile.mkdirs()).thenReturn(true);

    assertThat(FileUtils.createDirectory(this.mockFile)).isTrue();

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, times(1)).mkdirs();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void createDirectoryWithExistingDirectory() {

    when(this.mockFile.isDirectory()).thenReturn(true);
    when(this.mockFile.isFile()).thenReturn(false);

    assertThat(FileUtils.createDirectory(this.mockFile)).isTrue();

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, never()).mkdirs();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void createDirectoryWithExistingFile() {

    when(this.mockFile.isFile()).thenReturn(true);

    assertThat(FileUtils.createDirectory(this.mockFile)).isFalse();

    verify(this.mockFile, never()).isDirectory();
    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, never()).mkdirs();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  public void createDirectoryWithNull() {
    assertThat(FileUtils.createDirectory(null)).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void createFileWithNonExistingFile() throws IOException {

    when(this.mockFile.isDirectory()).thenReturn(false);
    when(this.mockFile.isFile()).thenReturn(false);
    when(this.mockFile.createNewFile()).thenReturn(true);

    assertThat(FileUtils.createFile(this.mockFile)).isTrue();

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, times(1)).createNewFile();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void createFileWithExistingDirectory() throws IOException {

    when(this.mockFile.isDirectory()).thenReturn(true);

    assertThat(FileUtils.createFile(this.mockFile)).isFalse();

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, never()).isFile();
    verify(this.mockFile, never()).createNewFile();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void createFileWithExistingFile() throws IOException {

    when(this.mockFile.isDirectory()).thenReturn(false);
    when(this.mockFile.isFile()).thenReturn(true);

    assertThat(FileUtils.createFile(this.mockFile)).isTrue();

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, never()).createNewFile();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  public void createFileWithNull() {
    assertThat(FileUtils.createFile(null)).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void createFileThrowsIOExceptionOnFileCreateNewFile() throws IOException {

    when(this.mockFile.isDirectory()).thenReturn(false);
    when(this.mockFile.isFile()).thenReturn(false);
    when(this.mockFile.createNewFile()).thenThrow(new IOException("test"));

    assertThat(FileUtils.createFile(this.mockFile)).isFalse();

    verify(this.mockFile, times(1)).isDirectory();
    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, times(1)).createNewFile();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void deleteExistingFileIsSuccessful() {

    when(this.mockFile.exists()).thenReturn(true);
    when(this.mockFile.delete()).thenReturn(true);

    assertThat(FileUtils.delete(this.mockFile)).isTrue();

    verify(this.mockFile, times(1)).exists();
    verify(this.mockFile, times(1)).delete();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void deleteExistingFileIsUnsuccessful() {

    when(this.mockFile.exists()).thenReturn(true);
    when(this.mockFile.delete()).thenReturn(false);

    assertThat(FileUtils.delete(this.mockFile)).isFalse();

    verify(this.mockFile, times(1)).exists();
    verify(this.mockFile, times(1)).delete();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void deleteNonExistingFileIsUnsuccessful() {

    when(this.mockFile.exists()).thenReturn(false);

    assertThat(FileUtils.delete(this.mockFile)).isFalse();

    verify(this.mockFile, times(1)).exists();
    verify(this.mockFile, never()).delete();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  public void deleteNullIsUnsuccessful() {
    assertThat(FileUtils.delete(null)).isFalse();
  }

  @Test
  public void getExtensionOfFilesWithExtension() {

    assertThat(FileUtils.getExtension(newFile("/absolute/path/to/file.ext"))).isEqualTo("ext");
    assertThat(FileUtils.getExtension(newFile("relative/path/to/file.ext"))).isEqualTo("ext");
    assertThat(FileUtils.getExtension(newFile("FileUtils.java"))).isEqualTo("java");
    assertThat(FileUtils.getExtension(newFile("FileUtilsTests.class"))).isEqualTo("class");
    assertThat(FileUtils.getExtension(newFile("search.c"))).isEqualTo("c");
    assertThat(FileUtils.getExtension(newFile("sort.cpp"))).isEqualTo("cpp");
    assertThat(FileUtils.getExtension(newFile("/path/to/file/with/two/extensions/test.java.class")))
      .isEqualTo("java.class");
  }

  @Test
  public void getExtensionOfFilesWithNoExtension() {

    assertThat(FileUtils.getExtension(newFile("file"))).isEqualTo(StringUtils.EMPTY_STRING);
    assertThat(FileUtils.getExtension(newFile("file."))).isEqualTo(StringUtils.EMPTY_STRING);
    assertThat(FileUtils.getExtension(newFile("exe"))).isEqualTo(StringUtils.EMPTY_STRING);
  }

  @Test(expected = IllegalArgumentException.class)
  public void getExtensionWithNull() {

    try {
      FileUtils.getExtension(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("File cannot be null");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void getLocationOfFilesWithLocation() {

    assertThat(FileUtils.getLocation(newFile("/absolute/path/to/file.ext"))).isEqualTo("/absolute/path/to");
    assertThat(FileUtils.getLocation(newFile("relative/path/to/file.ext"))).isEqualTo(String.format(
      "%1$s%2$srelative/path/to", WORKING_DIRECTORY.getAbsolutePath(), File.separator));
    assertThat(FileUtils.getLocation(newFile("/location/to/a/file/system/directory"))).isEqualTo(
      "/location/to/a/file/system");
    assertThat(FileUtils.getLocation(WORKING_DIRECTORY)).isEqualTo(WORKING_DIRECTORY.getParent());
  }

  @Test(expected = IllegalArgumentException.class)
  public void getLocationOfFileWithNoLocation() {

    try {
      FileUtils.getLocation(newFile("file.ext"));
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Unable to determine the location of file [file.ext]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void getLocationWithNull() {

    try {
      FileUtils.getLocation(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("File cannot be null");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void getNameOfFilesWithName() {

    assertThat(FileUtils.getName(newFile("/absolute/path/to/file.ext"))).isEqualTo("file");
    assertThat(FileUtils.getName(newFile("relative/path/to/file.ext"))).isEqualTo("file");
    assertThat(FileUtils.getName(newFile("FileUtilsTests.java"))).isEqualTo("FileUtilsTests");
    assertThat(FileUtils.getName(newFile("FileUtils.class"))).isEqualTo("FileUtils");
    assertThat(FileUtils.getName(newFile("search.c"))).isEqualTo("search");
    assertThat(FileUtils.getName(newFile("sort.cpp"))).isEqualTo("sort");
    assertThat(FileUtils.getName(newFile("/path/to/file/with/two/extensions/test.java.class"))).isEqualTo("test");
  }

  @Test
  public void getNameOfFilesWithNoName() {

    assertThat(FileUtils.getName(newFile(".exe"))).isEqualTo(StringUtils.EMPTY_STRING);
    assertThat(FileUtils.getName(newFile("."))).isEqualTo(StringUtils.EMPTY_STRING);
  }

  @Test(expected = IllegalArgumentException.class)
  public void getNameWithNull() {

    try {
      FileUtils.getName(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("File cannot be null");
      assertThat(expected).hasNoCause();

      throw expected;
    }
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
    assertThat(FileUtils.isDirectory(newFile(WORKING_DIRECTORY, "cp-elements-1.0.0.SNAPSHOT.jar"))).isFalse();
  }

  @Test
  public void isDirectoryWithNull() {
    assertThat(FileUtils.isDirectory(null)).isFalse();
  }

  @Test
  public void isEmptyWithExistingNonEmptyDirectoryIsTrue() {
    assertThat(FileUtils.isEmpty(WORKING_DIRECTORY)).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void isEmptyWithExistingNonEmptyFileIsFalse() {

    when(this.mockFile.isFile()).thenReturn(true);
    when(this.mockFile.length()).thenReturn(1L);

    assertThat(FileUtils.isEmpty(this.mockFile)).isFalse();

    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, times(1)).length();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void isEmptyWithExistingEmptyFileIsTrue() {

    when(this.mockFile.isFile()).thenReturn(true);
    when(this.mockFile.length()).thenReturn(0L);

    assertThat(FileUtils.isEmpty(this.mockFile)).isTrue();

    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, times(1)).length();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  @SuppressWarnings("all")
  public void isEmptyWithNonExistingFileIsTrue() {

    when(this.mockFile.isFile()).thenReturn(false);

    assertThat(FileUtils.isEmpty(this.mockFile)).isTrue();

    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, never()).length();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  public void isEmptyWithNullIsTrue() {
    assertThat(FileUtils.isEmpty(null)).isTrue();
  }

  @Test
  public void isExistingWithExistingDirectory() {
    assertThat(FileUtils.isExisting(WORKING_DIRECTORY)).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void isExistingWithExistingFile() {

    when(this.mockFile.exists()).thenReturn(true);

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
  public void isExistingWithNull() {
    assertThat(FileUtils.isExisting(null)).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void isFileWithFile() {

    when(this.mockFile.isFile()).thenReturn(true);

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
  public void isFileWithNull() {
    assertThat(FileUtils.isFile(null)).isFalse();
  }

  @Test
  public void newFileWithNonNullNonExistingPathnameIsSuccessful() {

    File file = FileUtils.newFile("/absolute/path/to/file.ext");

    assertThat(file).isNotNull();
    assertThat(file.exists()).isFalse();
    assertThat(file.getAbsolutePath()).isEqualTo("/absolute/path/to/file.ext");
  }

  @Test
  @IntegrationTest
  public void newFileWithNonNullExistingDirectoryIsSuccessful() {

    assertThat(WORKING_DIRECTORY.isDirectory()).isTrue();

    File workingDirectory = FileUtils.newFile(WORKING_DIRECTORY.getAbsolutePath());

    assertThat(workingDirectory).isNotNull();
    assertThat(workingDirectory.isDirectory()).isTrue();
    assertThat(workingDirectory).isEqualTo(WORKING_DIRECTORY);
  }

  @Test
  @IntegrationTest
  public void newFileWithNonNullExistingFileIsSuccessful() {

    File fileUtilsClass = getLocation(FileUtils.class);

    assertThat(fileUtilsClass).isNotNull();
    assertThat(fileUtilsClass.isFile()).isTrue();
    assertThat(FileUtils.newFile(fileUtilsClass.getAbsolutePath())).isEqualTo(fileUtilsClass);
  }

  @Test(expected = NullPointerException.class)
  public void newFileWithNullPathnameIsUnsuccessful() {

    try {
      FileUtils.newFile(null);
    }
    catch (NullPointerException expected) {

      assertThat(expected).hasMessage(null);
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void readFromFile() throws IOException, URISyntaxException {

    URL testFileTxtUrl = getClass().getClassLoader().getResource("testFile.txt");

    assertThat(testFileTxtUrl).isNotNull();

    File testFileTxt = new File(testFileTxtUrl.toURI());

    assertThat(testFileTxt).isNotNull();
    assertThat(testFileTxt.isFile()).isTrue();

    String contents = FileUtils.read(testFileTxt);

    assertThat(contents).isNotEmpty();
    assertThat(contents).isEqualTo(String.format("I solemnly swear that I am up to no good!%sMischief Managed!",
      StringUtils.LINE_SEPARATOR));
  }

  @Test(expected = IllegalArgumentException.class)
  public void readFromNonFile() throws IOException {

    try {
      FileUtils.read(null);
    }
    catch (IOException expected) {

      assertThat(expected).hasMessage("[null] must be a valid file");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @SuppressWarnings("all")
  @Test(expected = IllegalStateException.class)
  public void readFromUnreadableFile() throws IOException {

    when(this.mockFile.isFile()).thenReturn(true);
    when(this.mockFile.canRead()).thenReturn(false);
    when(this.mockFile.getCanonicalPath()).thenReturn("/path/to/file.ext");

    try {
      FileUtils.read(this.mockFile);
    }
    catch (IllegalStateException expected) {

      assertThat(expected).hasMessage("[/path/to/file.ext] is unreadable");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(this.mockFile, times(1)).isFile();
      verify(this.mockFile, times(1)).canRead();
      verify(this.mockFile, times(1)).getCanonicalPath();
      verifyNoMoreInteractions(this.mockFile);
    }
  }

  @Test
  public void sizeOfExistingDirectory() {
    assertThat(FileUtils.size(WORKING_DIRECTORY)).isEqualTo(0L);
  }

  @Test
  @SuppressWarnings("all")
  public void sizeOfExistingFile() {

    when(this.mockFile.isFile()).thenReturn(true);
    when(this.mockFile.length()).thenReturn(1L);

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

    when(this.mockFile.isFile()).thenReturn(false);

    assertThat(FileUtils.size(this.mockFile)).isEqualTo(0L);

    verify(this.mockFile, times(1)).isFile();
    verify(this.mockFile, never()).length();
    verifyNoMoreInteractions(this.mockFile);
  }

  @Test
  public void sizeOfNull() {
    assertThat(FileUtils.size(null)).isEqualTo(0L);
  }

  @Test
  @SuppressWarnings("all")
  public void tryGetCanonicalFileElseGetAbsoluteFile() throws IOException {

    when(this.mockFile.getCanonicalFile()).thenReturn(this.mockFile);

    assertThat(FileUtils.tryGetCanonicalFileElseGetAbsoluteFile(this.mockFile)).isEqualTo(this.mockFile);

    verify(this.mockFile, never()).getAbsoluteFile();
    verify(this.mockFile, times(1)).getCanonicalFile();
  }

  @Test
  @SuppressWarnings("all")
  public void tryGetCanonicalFileElseGetAbsoluteFileWhenGetCanonicalFileThrowsIOException() throws IOException {

    File expectedMockAbsoluteFile = mock(File.class, "expectedMockAbsoluteFile");

    when(this.mockFile.getAbsoluteFile()).thenReturn(expectedMockAbsoluteFile);
    when(this.mockFile.getCanonicalFile()).thenThrow(new IOException("test"));

    assertThat(FileUtils.tryGetCanonicalFileElseGetAbsoluteFile(this.mockFile)).isEqualTo(expectedMockAbsoluteFile);

    verify(this.mockFile, times(1)).getAbsoluteFile();
    verify(this.mockFile, times(1)).getCanonicalFile();
  }

  @Test
  @SuppressWarnings("all")
  public void tryGetCanonicalPathElseGetAbsolutePath() throws IOException {

    when(this.mockFile.getCanonicalPath()).thenReturn("/path/to/file.ext");

    assertThat(FileUtils.tryGetCanonicalPathElseGetAbsolutePath(this.mockFile)).isEqualTo("/path/to/file.ext");

    verify(this.mockFile, never()).getAbsolutePath();
    verify(this.mockFile, times(1)).getCanonicalPath();
  }

  @Test
  @SuppressWarnings("all")
  public void tryGetCanonicalPathElseGetAbsolutePathWhenGetCanonicalPathThrowsIOException() throws IOException {

    when(this.mockFile.getAbsolutePath()).thenReturn("/path/to/file.ext");
    when(this.mockFile.getCanonicalPath()).thenThrow(new IOException("test"));

    assertThat(FileUtils.tryGetCanonicalPathElseGetAbsolutePath(this.mockFile)).isEqualTo("/path/to/file.ext");

    verify(this.mockFile, times(1)).getAbsolutePath();
    verify(this.mockFile, times(1)).getCanonicalPath();
  }

  @Test(expected = IllegalArgumentException.class)
  public void writeNullInputStreamToFile() throws IOException {

    try {
      FileUtils.write(null, this.mockFile);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("InputStream cannot be null");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verifyZeroInteractions(this.mockFile);
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void writeInputStreamToNullFile() throws IOException {

    InputStream mockInputStream = mock(InputStream.class);

    try {
      FileUtils.write(mockInputStream, null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("File cannot be null");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verifyZeroInteractions(mockInputStream);
    }
  }

  @SuppressWarnings("all")
  @Test(expected = IllegalStateException.class)
  public void writeToExistingUnwritableFile() throws IOException {

    when(this.mockFile.exists()).thenReturn(true);
    when(this.mockFile.canWrite()).thenReturn(false);
    when(this.mockFile.getCanonicalPath()).thenReturn("/path/to/file.ext");

    InputStream mockInputStream = mock(InputStream.class);

    try {
      FileUtils.write(mockInputStream, this.mockFile);
    }
    catch (IllegalStateException expected) {

      assertThat(expected).hasMessage("[/path/to/file.ext] is not writable");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(this.mockFile, times(1)).exists();
      verify(this.mockFile, times(1)).canWrite();
      verify(this.mockFile, times(1)).getCanonicalPath();
      verifyZeroInteractions(mockInputStream);
    }
  }

  @Test
  @IntegrationTest
  public void writeToFileThenReadFromFileIsSuccessful() throws IOException {

    String expectedContent = "This is a test of the read/write File operations in FileUtils!";

    InputStream in = new ByteArrayInputStream(expectedContent.getBytes());

    File testFile = newFile(TEMPORARY_DIRECTORY, "writeToFileThenReadFromFileIsSuccessful.txt");

    assertThat(testFile).isNotNull();
    assertThat(testFile.exists()).isFalse();

    testFile.deleteOnExit();

    assertThat(FileUtils.write(in, testFile)).isSameAs(testFile);
    assertThat(testFile.isFile()).isTrue();
    assertThat(testFile.length()).isGreaterThan(0L);

    String actualContent = FileUtils.read(testFile);

    assertThat(actualContent).isEqualTo(expectedContent);
  }
}
