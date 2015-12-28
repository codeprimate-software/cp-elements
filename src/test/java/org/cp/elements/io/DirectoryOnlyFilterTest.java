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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;

import org.cp.elements.test.AbstractBaseTestSuite;
import org.junit.Test;

/**
 * The DirectoryOnlyFilterTest class is a test suite of test cases testing the contract and functionality of the
 * DirectoryOnlyFilter class.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see java.io.File
 * @see org.cp.elements.io.DirectoryOnlyFilter
 * @see org.cp.elements.test.AbstractBaseTestSuite
 * @since 1.0.0
 */
public class DirectoryOnlyFilterTest extends AbstractBaseTestSuite {

  private DirectoryOnlyFilter filter = new DirectoryOnlyFilter();

  @Test
  public void testAcceptWithDirectory() {
    assertTrue(filter.accept(new File(System.getProperty("user.dir"))));
    assertTrue(filter.accept(new File(System.getProperty("user.home"))));
    assertTrue(filter.accept(new File(System.getProperty("java.io.tmpdir"))));
  }

  @Test
  public void testAcceptWithFile() {
    File directoryOnlyFilterClass = new File(getBuildOutputDirectory(),
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
