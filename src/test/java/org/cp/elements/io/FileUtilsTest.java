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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.IOException;

import org.cp.elements.test.AbstractBaseTestSuite;
import org.junit.Test;

/**
 * The FileUtilsTest class is a test suite of test cases testing the contract and functionality of the FileUtils class.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.io.FileUtils
 * @see org.cp.elements.test.AbstractBaseTestSuite
 * @since 1.0.0
 */
@SuppressWarnings("all")
public class FileUtilsTest extends AbstractBaseTestSuite {

  @Test
  public void testGetExtension() {
    File file = new File("/absolute/path/to/file.ext");
    String fileExtension = FileUtils.getExtension(file);

    assertEquals("ext", fileExtension);

    file = new File("FileUtils.java");
    fileExtension = FileUtils.getExtension(file);

    assertEquals("java", fileExtension);

    file = new File("FileUtilsTest.class");
    fileExtension = FileUtils.getExtension(file);

    assertEquals("class", fileExtension);

    file = new File("relative/path/to/some/file");
    fileExtension = FileUtils.getExtension(file);

    assertEquals("", fileExtension);

    file = new File("/path/to/some/file/with/two/extensions/test.java.class");
    fileExtension = FileUtils.getExtension(file);

    assertEquals("java.class", fileExtension);
  }

  @Test(expected = NullPointerException.class)
  public void testGetExtensionWithNullFile() {
    try {
      FileUtils.getExtension(null);
    }
    catch (NullPointerException expected) {
      assertEquals("The File from which to get the extension cannot be null!", expected.getMessage());
      throw expected;
    }
  }

  @Test
  public void testGetLocation() {
    File file = new File("/absolute/path/to/file.ext");
    String location = FileUtils.getLocation(file);

    assertEquals("/absolute/path/to", location);

    file = new File("relative/path/to/another/file.ext");
    location = FileUtils.getLocation(file);

    assertEquals(String.format("%1$s%2$srelative/path/to/another", WORKING_DIRECTORY.getAbsolutePath(), File.separator),
      location);

    file = new File("/location/to/a/file/system/directory");
    location = FileUtils.getLocation(file);

    assertEquals("/location/to/a/file/system", location);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testGetLocationWithNonLocatableFile() {
    try {
       FileUtils.getLocation(new File("file.ext"));
    }
    catch (IllegalArgumentException expected) {
      assertEquals("Unable to find the location of file (file.ext)!", expected.getMessage());
      throw expected;
    }
  }

  @Test(expected = NullPointerException.class)
  public void testGetLocationWithNullFile() {
    try {
      FileUtils.getLocation(null);
    }
    catch (NullPointerException expected) {
      assertEquals("The File to get the location of cannot be null!", expected.getMessage());
      throw expected;
    }
  }

  @Test
  public void testGetNameWithoutExtension() {
    File file = new File("/absolute/path/to/file.ext");
    String filename = FileUtils.getNameWithoutExtension(file);

    assertEquals("file", filename);

    file = new File("FileUtilsTest.java");
    filename = FileUtils.getNameWithoutExtension(file);

    assertEquals("FileUtilsTest", filename);

    file = new File("relative/path/to/someFile");
    filename = FileUtils.getNameWithoutExtension(file);

    assertEquals("someFile", filename);

    file = new File("FileUtils");
    filename = FileUtils.getNameWithoutExtension(file);

    assertEquals("FileUtils", filename);

    file = new File("/path/to/some/file/with/two/extensions/test.java.class");
    filename = FileUtils.getNameWithoutExtension(file);

    assertEquals("test", filename);
  }

  @Test(expected = NullPointerException.class)
  public void testGetNameWithoutExtensionWithNullFile() {
    try {
      FileUtils.getNameWithoutExtension(null);
    }
    catch (NullPointerException expected) {
      assertEquals("The File from which to get the name of without extension cannot be null!", expected.getMessage());
      throw expected;
    }
  }

  @Test
  public void testIsDirectory() {
    assertTrue(FileUtils.isDirectory(TEMPORARY_DIRECTORY));
    assertTrue(FileUtils.isDirectory(USER_HOME));
    assertTrue(FileUtils.isDirectory(WORKING_DIRECTORY));
    assertFalse(FileUtils.isDirectory(new File(WORKING_DIRECTORY, "non_existing_directory/")));
    assertFalse(FileUtils.isDirectory(new File(WORKING_DIRECTORY, "cp-elements-1.0.0.SNAPSHOT.jar")));
    assertFalse(FileUtils.isDirectory(new File(WORKING_DIRECTORY, "non_existing_file.ext")));
    assertFalse(FileUtils.isDirectory(null));
  }

  @Test
  public void testIsExisting() {
    assertTrue(FileUtils.isExisting(getLocation(FileUtils.class)));
    assertTrue(FileUtils.isExisting(WORKING_DIRECTORY));
    assertFalse(FileUtils.isExisting(new File("/path/to/non_existing/pathname")));
    assertFalse(FileUtils.isExisting(null));
  }

  @Test
  public void testIsFile() {
    assertTrue(FileUtils.isFile(getLocation(FileUtils.class)));
    assertFalse(FileUtils.isFile(WORKING_DIRECTORY));
    assertFalse(FileUtils.isFile(new File("/path/to/non_existing/file.ext")));
    assertFalse(FileUtils.isFile(new File("/path/to/non_existing/directory/")));
  }

  @Test
  public void testTryGetCanonicalFileElseGetAbsoluteFile() throws IOException {
    File mockFile = mock(File.class);

    when(mockFile.getCanonicalFile()).thenReturn(mockFile);

    assertSame(mockFile, FileUtils.tryGetCanonicalFileElseGetAbsoluteFile(mockFile));

    verify(mockFile, never()).getAbsoluteFile();
    verify(mockFile, times(1)).getCanonicalFile();
  }

  @Test
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
  public void testTryGetCanonicalPathElseGetAbsolutePath() throws IOException {
    File mockFile = mock(File.class);

    when(mockFile.getCanonicalPath()).thenReturn("/absolute/path/to/mock/file");

    assertEquals("/absolute/path/to/mock/file", FileUtils.tryGetCanonicalPathElseGetAbsolutePath(mockFile));

    verify(mockFile, never()).getAbsolutePath();
    verify(mockFile, times(1)).getCanonicalPath();
  }

  @Test
  public void testTryGetCanonicalPathElseGetAbsolutePathWhenGetCanonicalPathThrowsIOException() throws IOException {
    File mockFile = mock(File.class);

    when(mockFile.getAbsolutePath()).thenReturn("/absolute/path/to/mock/file");
    when(mockFile.getCanonicalPath()).thenThrow(new IOException("io error!"));

    assertEquals("/absolute/path/to/mock/file", FileUtils.tryGetCanonicalPathElseGetAbsolutePath(mockFile));

    verify(mockFile, times(1)).getAbsolutePath();
    verify(mockFile, times(1)).getCanonicalPath();
  }

}
