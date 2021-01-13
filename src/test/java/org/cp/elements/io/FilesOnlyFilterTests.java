/*
 * Copyright 2011-Present Author or Authors.
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
import static org.junit.Assert.assertThat;

import java.io.File;

import org.cp.elements.test.AbstractBaseTestSuite;
import org.junit.Test;

/**
 * Test suite of test cases testing the contract and functionality of the {@link FilesOnlyFilter} class.
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

  private File newFile(File parent, String pathname) {
    return new File(parent, pathname);
  }

  @Test
  public void acceptsFile() {

    File filesOnlyFilterClass = getLocation(FilesOnlyFilter.class);

    assertThat(filesOnlyFilterClass, is(notNullValue(File.class)));
    assertThat(filesOnlyFilterClass.isFile(), is(true));
    assertThat(filesOnlyFilter.accept(filesOnlyFilterClass), is(true));
  }

  @Test
  public void rejectsDirectories() {

    assertThat(filesOnlyFilter.accept(TEMPORARY_DIRECTORY), is(false));
    assertThat(filesOnlyFilter.accept(USER_HOME), is(false));
    assertThat(filesOnlyFilter.accept(WORKING_DIRECTORY), is(false));
  }

  @Test
  public void rejectsNonExistingDirectory() {

    File nonExistingDirectory = newFile(USER_HOME, "relative/path/to/non/existing/directory/");

    assertThat(nonExistingDirectory, is(notNullValue(File.class)));
    assertThat(nonExistingDirectory.exists(), is(false));
    assertThat(filesOnlyFilter.accept(nonExistingDirectory), is(false));
  }

  @Test
  public void rejectsNonExistingFile() {

    File nonExistingFile = newFile(WORKING_DIRECTORY, "relative/path/to/non/existing/file.ext");

    assertThat(nonExistingFile, is(notNullValue(File.class)));
    assertThat(nonExistingFile.exists(), is(false));
    assertThat(filesOnlyFilter.accept(nonExistingFile), is(false));
  }

  @Test
  public void rejectsNull() {
    assertThat(filesOnlyFilter.accept(null), is(false));
  }
}
