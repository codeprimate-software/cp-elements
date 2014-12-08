/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.io;

import static org.junit.Assert.*;

import java.io.File;
import java.io.IOException;

import org.cp.elements.test.AbstractMockingTestSuite;
import org.easymock.EasyMock;
import org.jmock.Expectations;
import org.junit.Test;

/**
 * The FileUtilsTest class is a test suite of test cases testing the contract and functionality of the FileUtils class.
 *
 * @author John J. Blum
 * @see org.cp.elements.io.FileUtils
 * @see org.cp.elements.test.AbstractMockingTestSuite
 * @see org.junit.Assert
 * @see org.junit.Test
 */
public class FileUtilsTest extends AbstractMockingTestSuite {

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
    File mockFile = EasyMock.createMock(File.class);

    EasyMock.expect(mockFile.getCanonicalFile()).andReturn(mockFile);
    EasyMock.expectLastCall().once();
    EasyMock.replay(mockFile);

    assertSame(mockFile, FileUtils.tryGetCanonicalFileElseGetAbsoluteFile(mockFile));

    EasyMock.verify(mockFile);
  }

  @Test
  public void testTryGetCanonicalFileElseGetAbsoluteFileWhenGetCanonicalFileThrowIOException() throws IOException {
    File expectedMockAbsoluteFile = EasyMock.createMock(File.class);
    File mockFile = EasyMock.createMock(File.class);

    EasyMock.expect(mockFile.getCanonicalFile()).andThrow(new IOException("io error!"));
    EasyMock.expect(mockFile.getAbsoluteFile()).andReturn(expectedMockAbsoluteFile);
    EasyMock.expectLastCall().once();
    EasyMock.replay(mockFile);

    assertSame(expectedMockAbsoluteFile, FileUtils.tryGetCanonicalFileElseGetAbsoluteFile(mockFile));

    EasyMock.verify(mockFile);
  }

  @Test
  public void testTryGetCanonicalPathElseGetAbsolutePath() throws IOException {
    final File mockFile = mock(File.class, "testTryGetCanonicalPathElseGetAbsolutePath.mockFile");

    checking(new Expectations() {{
      oneOf(mockFile).getCanonicalPath();
      will(returnValue("/absolute/path/to/mock/file"));
      never(mockFile).getAbsolutePath();
    }});

    assertEquals("/absolute/path/to/mock/file", FileUtils.tryGetCanonicalPathElseGetAbsolutePath(mockFile));
  }

  @Test
  public void testTryGetCanonicalPathElseGetAbsolutePathWhenGetCanonicalPathThrowsIOException() throws IOException {
    final File mockFile = mock(File.class, "testTryGetCanonicalPathElseGetAbsolutePath.mockFile");

    checking(new Expectations() {{
      oneOf(mockFile).getCanonicalPath();
      will(throwException(new IOException("io error!")));
      oneOf(mockFile).getAbsolutePath();
      will(returnValue("/absolute/path/to/mock/file"));
    }});

    assertEquals("/absolute/path/to/mock/file", FileUtils.tryGetCanonicalPathElseGetAbsolutePath(mockFile));
  }

}
