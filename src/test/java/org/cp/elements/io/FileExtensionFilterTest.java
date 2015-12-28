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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.Arrays;

import org.cp.elements.lang.StringUtils;
import org.cp.elements.test.AbstractBaseTestSuite;
import org.cp.elements.test.TestUtils;
import org.cp.elements.util.ArrayUtils;
import org.junit.Test;

/**
 * The FileExtensionFilterTest class is a test suite of test cases testing the contract and functionality of the
 * FileExtensionFilter class.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.cp.elements.io.FileExtensionFilter
 * @see org.cp.elements.test.AbstractBaseTestSuite
 * @since 1.0.0
 */
public class FileExtensionFilterTest extends AbstractBaseTestSuite {

  @Test
  public void testConstructFileExtensionFilterWithClassExtension() {
    FileExtensionFilter fileFilter = new FileExtensionFilter("class");

    assertNotNull(fileFilter);
    assertNotNull(fileFilter.getFileExtensions());
    assertEquals(1, fileFilter.getFileExtensions().length);
    assertEquals("class", fileFilter.getFileExtensions()[0]);
  }

  @Test
  public void testConstructFileExtensionFilterWithDotJavaExtension() {
    FileExtensionFilter fileFilter = new FileExtensionFilter(".java");

    assertNotNull(fileFilter);
    assertNotNull(fileFilter.getFileExtensions());
    assertEquals(1, fileFilter.getFileExtensions().length);
    assertEquals("java", fileFilter.getFileExtensions()[0]);
  }

  @Test
  public void testConstructFileExtensionFilterWithAnArrayOfFileExtensions() {
    String[] expectedFileExtensions = { "ada", ".c", ".cpp", "java", ".rb" };

    FileExtensionFilter fileFilter = new FileExtensionFilter(expectedFileExtensions);

    assertNotNull(fileFilter);

    String[] actualFileExtensions = fileFilter.getFileExtensions();

    Arrays.sort(actualFileExtensions);

    TestUtils.assertEquals(ArrayUtils.transform(expectedFileExtensions, (value) -> value.startsWith(StringUtils.DOT_SEPARATOR)
      ? value.substring(1) : value), actualFileExtensions);
  }

  @Test
  public void testConstructFileExtensionFilterWithBlankFileExtension() {
    FileExtensionFilter fileFilter = new FileExtensionFilter("  ");

    assertNotNull(fileFilter);
    assertNotNull(fileFilter.getFileExtensions());
    assertEquals(0, fileFilter.getFileExtensions().length);
  }

  @Test
  public void testConstructFileExtensionFilterWithEmptyFileExtension() {
    FileExtensionFilter fileFilter = new FileExtensionFilter("");

    assertNotNull(fileFilter);
    assertNotNull(fileFilter.getFileExtensions());
    assertEquals(0, fileFilter.getFileExtensions().length);
  }

  @Test
  public void testConstructFileExtensionFilterWithNullFileExtension() {
    FileExtensionFilter fileFilter = new FileExtensionFilter((String) null);

    assertNotNull(fileFilter);
    assertNotNull(fileFilter.getFileExtensions());
    assertEquals(0, fileFilter.getFileExtensions().length);
  }

  @Test
  public void testConstructFileExtensionFilterWithNullEmptyAndBlankFileExtensions() {
    FileExtensionFilter fileFilter = new FileExtensionFilter(null, "", "  ");

    assertNotNull(fileFilter);
    assertNotNull(fileFilter.getFileExtensions());
    assertEquals(0, fileFilter.getFileExtensions().length);
  }

  @Test
  public void testAccept() {
    FileExtensionFilter fileFilter = new FileExtensionFilter("class");

    File classFile = new File(getBuildOutputDirectory(),
      "classes/org/cp/elements/io/FileExtensionFilter.class");

    File javaFile = new File(getBuildOutputDirectory(),
      "../src/main/java/org/cp/elements/io/FileExtensionFilter.java");

    assertNotNull(classFile);
    assertTrue(classFile.isFile());
    assertNotNull(javaFile);
    assertTrue(javaFile.isFile());
    assertTrue(fileFilter.accept(classFile));
    assertFalse(fileFilter.accept(javaFile));
    assertFalse(fileFilter.accept(new File(System.getProperty("user.dir"))));
  }

  @Test
  public void testAcceptsFileWithoutExtension() {
    FileExtensionFilter fileFilter = new FileExtensionFilter();

    File fileWithExtension = new File("/path/to/some/file.ext");
    File fileWithoutExtension = new File("/path/to/some/file");

    assertFalse(fileFilter.accept(fileWithExtension));
    assertTrue(fileFilter.accept(fileWithoutExtension));
  }

  @Test
  public void testAcceptsFileWithAndWithoutFileExtension() {
    FileExtensionFilter fileFilter = new FileExtensionFilter("java", ".CLASS", ".");

    assertTrue(fileFilter.accept(new File("/path/to/a/source.java")));
    assertTrue(fileFilter.accept(new File("absolute/path/to/a/binary.class")));
    assertTrue(fileFilter.accept(new File("/path/to/a/no/extension/file")));
    assertTrue(fileFilter.accept(new File("file.")));
    assertFalse(fileFilter.accept(new File("/path/to/a/non/source.cpp")));
    assertFalse(fileFilter.accept(new File("/path/to/a/class.file")));
  }

}
