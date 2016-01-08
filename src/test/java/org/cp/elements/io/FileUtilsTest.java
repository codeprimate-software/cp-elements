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
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import org.cp.elements.lang.StringUtils;
import org.cp.elements.test.AbstractBaseTestSuite;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * The FileUtilsTest class is a test suite of test cases testing the contract and functionality of the FileUtils class.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.rules.ExpectedException
 * @see org.mockito.Mockito
 * @see org.cp.elements.io.FileUtils
 * @see org.cp.elements.test.AbstractBaseTestSuite
 * @since 1.0.0
 */
public class FileUtilsTest extends AbstractBaseTestSuite {

  @Rule
  public ExpectedException expectedException = ExpectedException.none();

  protected File newFile(String pathname) {
    return new File(pathname);
  }

  protected File newFile(File parent, String pathname) {
    return new File(parent, pathname);
  }

  @Test
  public void assertExistsWithExistingDirectory() throws FileNotFoundException {
    FileUtils.assertExists(WORKING_DIRECTORY);
  }

  @Test
  public void assertExistsWithExistingFile() throws FileNotFoundException {
    FileUtils.assertExists(getLocation(FileUtils.class));
  }

  @Test
  public void assertExistsWithNonExistingDirectory() throws FileNotFoundException {
    expectedException.expect(FileNotFoundException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("(/absolute/path/to/non/existing/directory) was not found");

    FileUtils.assertExists(newFile("/absolute/path/to/non/existing/directory"));
  }

  @Test
  public void assertExistsWithNonExistingFile() throws FileNotFoundException {
    expectedException.expect(FileNotFoundException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("(relative/path/to/non/existing/file.ext) was not found");

    FileUtils.assertExists(newFile("relative/path/to/non/existing/file.ext"));
  }

  @Test
  public void assertExistsWithNull() throws FileNotFoundException {
    expectedException.expect(FileNotFoundException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("(null) was not found");

    FileUtils.assertExists(null);
  }

  @Test
  public void getExtensionOfFilesWithExtension() {
    assertThat(FileUtils.getExtension(newFile("/absolute/path/to/file.ext")), is(equalTo("ext")));
    assertThat(FileUtils.getExtension(newFile("relative/path/to/file.ext")), is(equalTo("ext")));
    assertThat(FileUtils.getExtension(newFile("FileUtils.java")), is(equalTo("java")));
    assertThat(FileUtils.getExtension(newFile("FileUtilsTest.class")), is(equalTo("class")));
    assertThat(FileUtils.getExtension(newFile("search.c")), is(equalTo("c")));
    assertThat(FileUtils.getExtension(newFile("sort.cpp")), is(equalTo("cpp")));
    assertThat(FileUtils.getExtension(newFile("/path/to/file/with/two/extensions/test.java.class")),
      is(equalTo("java.class")));
  }

  @Test
  public void getExtensionOfFilesWithNoExtension() {
    assertThat(FileUtils.getExtension(newFile("file")), is(equalTo(StringUtils.EMPTY_STRING)));
    assertThat(FileUtils.getExtension(newFile("file.")), is(equalTo(StringUtils.EMPTY_STRING)));
    assertThat(FileUtils.getExtension(newFile("exe")), is(equalTo(StringUtils.EMPTY_STRING)));
  }

  @Test
  public void getExtensionWithNull() {
    expectedException.expect(NullPointerException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("File cannot be null");

    FileUtils.getExtension(null);
  }

  @Test
  public void getLocationOfFilesWithLocation() {
    assertThat(FileUtils.getLocation(newFile("/absolute/path/to/file.ext")), is(equalTo("/absolute/path/to")));
    assertThat(FileUtils.getLocation(newFile("relative/path/to/file.ext")), is(equalTo(String.format(
      "%1$s%2$srelative/path/to", WORKING_DIRECTORY.getAbsolutePath(), File.separator))));
    assertThat(FileUtils.getLocation(newFile("/location/to/a/file/system/directory")),
      is(equalTo("/location/to/a/file/system")));
    assertThat(FileUtils.getLocation(WORKING_DIRECTORY), is(equalTo(WORKING_DIRECTORY.getParent())));
  }

  @Test
  public void getLocationOfFileWithNoLocation() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("Unable to determine the location of file (file.ext)");

    FileUtils.getLocation(new File("file.ext"));
  }

  @Test
  public void getLocationWithNull() {
    expectedException.expect(NullPointerException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("File cannot be null");

    FileUtils.getLocation(null);
  }

  @Test
  public void getNameOfFilesWithName() {
    assertThat(FileUtils.getName(newFile("/absolute/path/to/file.ext")), is(equalTo("file")));
    assertThat(FileUtils.getName(newFile("relative/path/to/file.ext")), is(equalTo("file")));
    assertThat(FileUtils.getName(newFile("FileUtilsTest.java")), is(equalTo("FileUtilsTest")));
    assertThat(FileUtils.getName(newFile("FileUtils.class")), is(equalTo("FileUtils")));
    assertThat(FileUtils.getName(newFile("search.c")), is(equalTo("search")));
    assertThat(FileUtils.getName(newFile("sort.cpp")), is(equalTo("sort")));
    assertThat(FileUtils.getName(newFile("/path/to/file/with/two/extensions/test.java.class")),
      is(equalTo("test")));
  }

  @Test
  public void getNameOfFilesWithNoName() {
    assertThat(FileUtils.getName(newFile(".exe")), is(equalTo(StringUtils.EMPTY_STRING)));
    assertThat(FileUtils.getName(newFile(".")), is(equalTo(StringUtils.EMPTY_STRING)));
  }

  @Test
  public void getNameWithNull() {
    expectedException.expect(NullPointerException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("File cannot be null");

    FileUtils.getName(null);
  }

  @Test
  public void isDirectoryWithDirectories() {
    assertThat(FileUtils.isDirectory(TEMPORARY_DIRECTORY), is(true));
    assertThat(FileUtils.isDirectory(USER_HOME), is(true));
    assertThat(FileUtils.isDirectory(WORKING_DIRECTORY), is(true));
  }

  @Test
  public void isDirectoryWithNonDirectories() {
    assertThat(FileUtils.isDirectory(newFile(WORKING_DIRECTORY, "non_existing_directory/")), is(false));
    assertThat(FileUtils.isDirectory(newFile(WORKING_DIRECTORY, "nonExistingFile.ext")), is(false));
    assertThat(FileUtils.isDirectory(newFile(WORKING_DIRECTORY, "cp-elements-1.0.0.SNAPSHOT.jar")), is(false));
  }

  @Test
  public void isDirectoryWithNull() {
    assertThat(FileUtils.isDirectory(null), is(false));
  }

  @Test
  public void isExistingWithExistingFiles() {
    assertThat(FileUtils.isExisting(WORKING_DIRECTORY), is(true));
    assertThat(FileUtils.isExisting(getLocation(FileUtils.class)), is(true));
  }

  @Test
  public void isExistingWithNonExistingFiles() {
    assertThat(FileUtils.isExisting(new File("/path/to/non/existing/pathname")), is(false));
  }

  @Test
  public void isExistingWithNull() {
    assertThat(FileUtils.isExisting(null), is(false));
  }

  @Test
  public void isFileWithFiles() {
    assertThat(FileUtils.isFile(getLocation(FileUtils.class)), is(true));
  }

  @Test
  public void isFileWithNonFiles() {
    assertThat(FileUtils.isFile(WORKING_DIRECTORY), is(false));
    assertThat(FileUtils.isFile(new File("relative/path/to/non/existing/directory/")), is(false));
    assertThat(FileUtils.isFile(new File("/absolute/path/to/non/existing/file.ext")), is(false));
  }

  @Test
  public void isFileWithNull() {
    assertThat(FileUtils.isFile(null), is(false));
  }

  @Test
  @SuppressWarnings("all")
  public void testTryGetCanonicalFileElseGetAbsoluteFile() throws IOException {
    File mockFile = mock(File.class);

    when(mockFile.getCanonicalFile()).thenReturn(mockFile);

    assertSame(mockFile, FileUtils.tryGetCanonicalFileElseGetAbsoluteFile(mockFile));

    verify(mockFile, never()).getAbsoluteFile();
    verify(mockFile, times(1)).getCanonicalFile();
  }

  @Test
  @SuppressWarnings("all")
  public void testTryGetCanonicalFileElseGetAbsoluteFileWhenGetCanonicalFileThrowIOException() throws IOException {
    File expectedMockAbsoluteFile = mock(File.class, "expectedMockAbsoluteFile");
    File mockFile = mock(File.class, "mockFile");

    when(mockFile.getCanonicalFile()).thenThrow(new IOException("io error!"));
    when(mockFile.getAbsoluteFile()).thenReturn(expectedMockAbsoluteFile);

    assertSame(expectedMockAbsoluteFile, FileUtils.tryGetCanonicalFileElseGetAbsoluteFile(mockFile));

    verify(mockFile, times(1)).getAbsoluteFile();
    verify(mockFile, times(1)).getCanonicalFile();
  }

  @Test
  @SuppressWarnings("all")
  public void testTryGetCanonicalPathElseGetAbsolutePath() throws IOException {
    File mockFile = mock(File.class);

    when(mockFile.getCanonicalPath()).thenReturn("/absolute/path/to/mock/file");

    assertEquals("/absolute/path/to/mock/file", FileUtils.tryGetCanonicalPathElseGetAbsolutePath(mockFile));

    verify(mockFile, never()).getAbsolutePath();
    verify(mockFile, times(1)).getCanonicalPath();
  }

  @Test
  @SuppressWarnings("all")
  public void testTryGetCanonicalPathElseGetAbsolutePathWhenGetCanonicalPathThrowsIOException() throws IOException {
    File mockFile = mock(File.class);

    when(mockFile.getAbsolutePath()).thenReturn("/absolute/path/to/mock/file");
    when(mockFile.getCanonicalPath()).thenThrow(new IOException("io error!"));

    assertEquals("/absolute/path/to/mock/file", FileUtils.tryGetCanonicalPathElseGetAbsolutePath(mockFile));

    verify(mockFile, times(1)).getAbsolutePath();
    verify(mockFile, times(1)).getCanonicalPath();
  }

}
