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

import org.cp.elements.lang.StringUtils;
import org.junit.Test;

/**
 * The FileExtensionFilterTest class is a test suite of test cases testing the contract and functionality of the
 * FileExtensionFilter class.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.io.FileExtensionFilter
 * @see org.junit.Test
 * @since 1.0.0
 */
public class FileExtenstionFilterTest {

  @Test
  public void testConstructFileExtensionFilter() {
    FileExtensionFilter filter = new FileExtensionFilter(".java");

    assertNotNull(filter);
    assertEquals(".java", filter.getFileExtension());
  }

  @Test(expected = IllegalArgumentException.class)
  public void testConstructFileExtensionFilterWithNullFileExtension() {
    try {
      new FileExtensionFilter(null);
    }
    catch (IllegalArgumentException expected) {
      assertEquals("The file extension must be specified!", expected.getMessage());
      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void testConstructFileExtensionFilterWithEmptyFileExtension() {
    try {
      new FileExtensionFilter(StringUtils.EMPTY_STRING);
    }
    catch (IllegalArgumentException expected) {
      assertEquals("The file extension must be specified!", expected.getMessage());
      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void testConstructFileExtensionFilterWithBlankFileExtension() {
    try {
      new FileExtensionFilter("  ");
    }
    catch (IllegalArgumentException expected) {
      assertEquals("The file extension must be specified!", expected.getMessage());
      throw expected;
    }
  }

  @Test
  public void testAccept() {
    FileExtensionFilter filter = new FileExtensionFilter(".class");

    File classFile = new File(System.getProperty("user.dir"), "classes/org/cp/elements/io/FileExtensionFilter.class");
    File javaFile = new File(System.getProperty("user.dir"), "../src/main/java/org/cp/elements/io/FileExtensionFilter.java");

    assertNotNull(classFile);
    assertTrue(classFile.isFile());
    assertNotNull(javaFile);
    assertTrue(javaFile.isFile());
    assertTrue(filter.accept(classFile));
    assertFalse(filter.accept(javaFile));
    assertFalse(filter.accept(new File(System.getProperty("user.dir"))));
  }

}
