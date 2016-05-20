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
import static org.junit.Assert.assertTrue;

import java.io.File;

import org.cp.elements.test.AbstractBaseTestSuite;
import org.junit.Test;

/**
 * The DirectoriesOnlyFilterTests class is a test suite of test cases testing the contract and functionality
 * of the {@link DirectoriesOnlyFilter} class.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see org.junit.Test
 * @see DirectoriesOnlyFilter
 * @see org.cp.elements.test.AbstractBaseTestSuite
 * @since 1.0.0
 */
public class DirectoriesOnlyFilterTests extends AbstractBaseTestSuite {

  private final DirectoriesOnlyFilter directoriesOnlyFilter = DirectoriesOnlyFilter.INSTANCE;

  protected File newFile(File parent, String pathname) {
    return new File(parent, pathname);
  }

  @Test
  public void acceptsDirectories() {
    assertTrue(directoriesOnlyFilter.accept(TEMPORARY_DIRECTORY));
    assertTrue(directoriesOnlyFilter.accept(USER_HOME));
    assertTrue(directoriesOnlyFilter.accept(WORKING_DIRECTORY));
  }

  @Test
  public void rejectsFile() {
    File directoriesOnlyFilterClass = getLocation(DirectoriesOnlyFilter.class);

    assertTrue(directoriesOnlyFilterClass.isFile());
    assertFalse(directoriesOnlyFilter.accept(directoriesOnlyFilterClass));
  }

  @Test
  public void rejectsNonExistingDirectory() {
    File nonExistingDirectory = newFile(WORKING_DIRECTORY, "relative/path/to/non/existing/directory/");

    assertFalse(nonExistingDirectory.exists());
    assertFalse(directoriesOnlyFilter.accept(nonExistingDirectory));
  }

  @Test
  public void rejectsNonExistingFile() {
    File nonExistingFile = newFile(USER_HOME, "relative/path/to/non/existing/file.ext");

    assertFalse(nonExistingFile.exists());
    assertFalse(directoriesOnlyFilter.accept(nonExistingFile));
  }

  @Test
  public void rejectsNull() {
    assertFalse(directoriesOnlyFilter.accept(null));
  }
}
