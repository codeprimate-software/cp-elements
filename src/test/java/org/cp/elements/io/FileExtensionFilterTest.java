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
 * Test suite of test cases testing the contract and functionality of the {@link FileExtensionFilter} class.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see org.junit.Test
 * @see org.cp.elements.io.FileExtensionFilter
 * @see org.cp.elements.test.AbstractBaseTestSuite
 * @since 1.0.0
 */
public class FileExtensionFilterTest extends AbstractBaseTestSuite {

  protected File newFile(String pathname) {
    return new File(pathname);
  }

  protected File newFile(File parent, String pathname) {
    return new File(parent, pathname);
  }

  @Test
  public void constructFileExtensionFilterWithClassExtension() {
    FileExtensionFilter fileFilter = new FileExtensionFilter("class");

    assertNotNull(fileFilter);
    assertNotNull(fileFilter.getFileExtensions());
    assertEquals(1, fileFilter.getFileExtensions().size());
    assertEquals("class", fileFilter.getFileExtensions().iterator().next());
  }

  @Test
  public void constructFileExtensionFilterWithDotJavaExtension() {
    FileExtensionFilter fileFilter = new FileExtensionFilter(".java");

    assertNotNull(fileFilter);
    assertNotNull(fileFilter.getFileExtensions());
    assertEquals(1, fileFilter.getFileExtensions().size());
    assertEquals("java", fileFilter.getFileExtensions().iterator().next());
  }

  @Test
  public void constructFileExtensionFilterWithAnArrayOfFileExtensions() {
    String[] expectedFileExtensions = { "ada", ".c", ".cpp", "groovy", "java", ".rb" };

    FileExtensionFilter fileFilter = new FileExtensionFilter(expectedFileExtensions);

    assertNotNull(fileFilter);

    String[] actualFileExtensions = fileFilter.getFileExtensions().toArray(new String[expectedFileExtensions.length]);

    Arrays.sort(actualFileExtensions);

    TestUtils.assertEquals(ArrayUtils.transform(expectedFileExtensions, (value) -> value.startsWith(StringUtils.DOT_SEPARATOR)
      ? value.substring(1) : value), actualFileExtensions);
  }

  @Test
  public void constructFileExtensionFilterWithBlankFileExtension() {
    FileExtensionFilter fileFilter = new FileExtensionFilter("  ");

    assertNotNull(fileFilter);
    assertNotNull(fileFilter.getFileExtensions());
    assertEquals(0, fileFilter.getFileExtensions().size());
  }

  @Test
  public void constructFileExtensionFilterWithEmptyFileExtension() {
    FileExtensionFilter fileFilter = new FileExtensionFilter("");

    assertNotNull(fileFilter);
    assertNotNull(fileFilter.getFileExtensions());
    assertEquals(0, fileFilter.getFileExtensions().size());
  }

  @Test
  public void constructFileExtensionFilterWithNullFileExtension() {
    FileExtensionFilter fileFilter = new FileExtensionFilter((String) null);

    assertNotNull(fileFilter);
    assertNotNull(fileFilter.getFileExtensions());
    assertEquals(0, fileFilter.getFileExtensions().size());
  }

  @Test
  public void constructFileExtensionFilterWithNullEmptyAndBlankFileExtensions() {
    FileExtensionFilter fileFilter = new FileExtensionFilter(null, "", "  ");

    assertNotNull(fileFilter);
    assertNotNull(fileFilter.getFileExtensions());
    assertEquals(0, fileFilter.getFileExtensions().size());
  }

  @Test
  public void acceptIsSuccessful() {
    FileExtensionFilter fileFilter = new FileExtensionFilter("class");

    File classFile = newFile(getBuildOutputDirectory(),
      "classes/org/cp/elements/io/FileExtensionFilter.class");

    File javaFile = newFile(getSourceDirectory(),
      "main/java/org/cp/elements/io/FileExtensionFilter.java");

    assertNotNull(classFile);
    assertTrue(classFile.isFile());
    assertNotNull(javaFile);
    assertTrue(javaFile.isFile());
    assertTrue(fileFilter.accept(classFile));
    assertFalse(fileFilter.accept(javaFile));
    assertFalse(fileFilter.accept(new File(System.getProperty("user.dir"))));
  }

  @Test
  public void acceptsFilesWithoutAnExtension() {
    FileExtensionFilter fileFilter = new FileExtensionFilter();

    assertTrue(fileFilter.accept(newFile("/path/to/some/file.ext")));
    assertTrue(fileFilter.accept(newFile("/path/to/some/file")));
  }

  @Test
  public void acceptsFilesWithAndWithoutAnExtension() {
    FileExtensionFilter fileFilter = new FileExtensionFilter("java", ".CLASS", ".");

    assertTrue(fileFilter.accept(newFile("/path/to/a/source.java")));
    assertTrue(fileFilter.accept(newFile("absolute/path/to/a/binary.class")));
    assertTrue(fileFilter.accept(newFile("/path/to/a/no/extension/file")));
    assertTrue(fileFilter.accept(newFile("file.")));
    assertFalse(fileFilter.accept(newFile("/path/to/a/non/source.cpp")));
    assertFalse(fileFilter.accept(newFile("/path/to/a/class.file")));
  }
}
