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
 * The FileOnlyFilterTest class is a test suite of test cases testing the contract and functionality of the
 * FileOnlyFilter class.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see org.cp.elements.io.FileOnlyFilter
 * @see org.cp.elements.test.AbstractBaseTestSuite
 * @see org.junit.Test
 * @since 1.0.0
 */
public class FileOnlyFilterTest extends AbstractBaseTestSuite {

  private FileOnlyFilter filter = new FileOnlyFilter();

  @Test
  public void testAcceptWithFile() {
    File fileOnlyFilterClass = new File(getBuildOutputDirectory(),
      "classes/org/cp/elements/io/FileOnlyFilter.class");

    assertNotNull(fileOnlyFilterClass);
    assertTrue(fileOnlyFilterClass.isFile());
    assertTrue(filter.accept(fileOnlyFilterClass));
  }

  @Test
  public void testAcceptWithDirectory() {
    assertFalse(filter.accept(new File(System.getProperty("user.dir"))));
    assertFalse(filter.accept(new File(System.getProperty("user.home"))));
    assertFalse(filter.accept(new File(System.getProperty("java.io.tmpdir"))));
  }

  @Test
  public void testAcceptWithNonExistingFile() {
    File nonExistingFile = new File(System.getProperty("user.dir"), "relative/path/to/non_existing/file.ext");

    assertNotNull(nonExistingFile);
    assertFalse(nonExistingFile.isFile());
    assertFalse(nonExistingFile.exists());
    assertFalse(filter.accept(nonExistingFile));
  }

  @Test
  public void testAcceptWithNonExistingDirectory() {
    File nonExistingDirectory = new File(System.getProperty("user.dir"), "relative/path/to/non_existing/directory/");

    assertNotNull(nonExistingDirectory);
    assertFalse(nonExistingDirectory.isDirectory());
    assertFalse(nonExistingDirectory.exists());
    assertFalse(filter.accept(nonExistingDirectory));
  }

}
