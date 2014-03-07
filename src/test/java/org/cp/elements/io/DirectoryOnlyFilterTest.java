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

import org.cp.elements.test.CommonBaseTestSuite;
import org.junit.Test;

/**
 * The DirectoryOnlyFilterTest class is a test suite of test cases testing the contract and functionality of the
 * DirectoryOnlyFilter class.
 * <p/>
 * @author John J. Blum
 * @see java.io.File
 * @see org.cp.elements.io.DirectoryOnlyFilter
 * @see org.cp.elements.test.CommonBaseTestSuite
 * @see org.junit.Test
 * @since 1.0.0
 */
public class DirectoryOnlyFilterTest extends CommonBaseTestSuite {

  private DirectoryOnlyFilter filter = new DirectoryOnlyFilter();

  @Test
  public void testAcceptWithDirectory() {
    assertTrue(filter.accept(new File(System.getProperty("user.dir"))));
    assertTrue(filter.accept(new File(System.getProperty("user.home"))));
    assertTrue(filter.accept(new File(System.getProperty("java.io.tmpdir"))));
  }

  @Test
  public void testAcceptWithFile() {
    File directoryOnlyFilterClass = new File(getBuildOutputDirectoryAsWorkingDirectory(),
      "classes/org/cp/elements/io/DirectoryOnlyFilter.class");

    assertNotNull(directoryOnlyFilterClass);
    assertTrue(directoryOnlyFilterClass.isFile());
    assertFalse(filter.accept(directoryOnlyFilterClass));
  }

  @Test
  public void testAcceptWithNonExistingDirectory() {
    File nonExistingDirectory = new File(System.getProperty("user.dir"), "relative/path/to/non_existing/directory/");

    assertNotNull(nonExistingDirectory);
    assertFalse(nonExistingDirectory.isDirectory());
    assertFalse(nonExistingDirectory.exists());
    assertFalse(filter.accept(nonExistingDirectory));
  }

  @Test
  public void testAcceptWithNonExistingFile() {
    File nonExistingFile = new File(System.getProperty("user.dir"), "relative/path/to/non_existing/file.ext");

    assertNotNull(nonExistingFile);
    assertFalse(nonExistingFile.isFile());
    assertFalse(nonExistingFile.exists());
    assertFalse(filter.accept(nonExistingFile));
  }

}
