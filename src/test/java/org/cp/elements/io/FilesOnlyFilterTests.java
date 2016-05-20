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

import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import java.io.File;

import org.cp.elements.test.AbstractBaseTestSuite;
import org.junit.Test;

/**
 * The FilesOnlyFilterTests class is a test suite of test cases testing the contract and functionality
 * of the {@link FilesOnlyFilter} class.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see org.junit.Test
 * @see org.cp.elements.io.FilesOnlyFilter
 * @see org.cp.elements.test.AbstractBaseTestSuite
 * @since 1.0.0
 */
public class FilesOnlyFilterTests extends AbstractBaseTestSuite {

  private FilesOnlyFilter filesOnlyFilter = FilesOnlyFilter.INSTANCE;

  protected File newFile(File parent, String pathname) {
    return new File(parent, pathname);
  }

  @Test
  public void acceptsFile() {
    File filesOnlyFilterClass = getLocation(FilesOnlyFilter.class);

    assertThat(filesOnlyFilterClass, is(notNullValue(File.class)));
    assertTrue(filesOnlyFilterClass.exists());
    assertTrue(filesOnlyFilter.accept(filesOnlyFilterClass));
  }

  @Test
  public void rejectsDirectories() {
    assertFalse(filesOnlyFilter.accept(TEMPORARY_DIRECTORY));
    assertFalse(filesOnlyFilter.accept(USER_HOME));
    assertFalse(filesOnlyFilter.accept(WORKING_DIRECTORY));
  }

  @Test
  public void rejectsNonExistingDirectory() {
    File nonExistingDirectory = newFile(USER_HOME, "relative/path/to/non/existing/directory/");

    assertThat(nonExistingDirectory, is(notNullValue(File.class)));
    assertFalse(nonExistingDirectory.exists());
    assertFalse(filesOnlyFilter.accept(nonExistingDirectory));
  }

  @Test
  public void rejectsNonExistingFile() {
    File nonExistingFile = newFile(WORKING_DIRECTORY, "relative/path/to/non/existing/file.ext");

    assertThat(nonExistingFile, is(notNullValue(File.class)));
    assertFalse(nonExistingFile.exists());
    assertFalse(filesOnlyFilter.accept(nonExistingFile));
  }

  @Test
  public void rejectsNull() {
    assertFalse(filesOnlyFilter.accept(null));
  }
}
