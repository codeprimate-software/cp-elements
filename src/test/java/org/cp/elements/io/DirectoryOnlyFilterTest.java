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
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import java.io.File;

import org.cp.elements.test.AbstractBaseTestSuite;
import org.junit.Test;

/**
 * The DirectoryOnlyFilterTest class is a test suite of test cases testing the contract and functionality of the
 * {@link DirectoryOnlyFilter} class.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see org.junit.Test
 * @see org.cp.elements.io.DirectoryOnlyFilter
 * @see org.cp.elements.test.AbstractBaseTestSuite
 * @since 1.0.0
 */
public class DirectoryOnlyFilterTest extends AbstractBaseTestSuite {

  @Test
  public void acceptsDirectories() {
    assertTrue(DirectoryOnlyFilter.INSTANCE.accept(TEMPORARY_DIRECTORY));
    assertTrue(DirectoryOnlyFilter.INSTANCE.accept(USER_HOME));
    assertTrue(DirectoryOnlyFilter.INSTANCE.accept(WORKING_DIRECTORY));
  }

  @Test
  public void rejectsFile() {
    File directoryOnlyFilterClass = getLocation(DirectoryOnlyFilter.class);

    assertThat(directoryOnlyFilterClass.isFile(), is(true));
    assertThat(DirectoryOnlyFilter.INSTANCE.accept(directoryOnlyFilterClass), is(false));
  }

  @Test
  public void rejectsNonExistingDirectory() {
    File nonExistingDirectory = new File(WORKING_DIRECTORY, "relative/path/to/non/existing/directory/");

    assertThat(nonExistingDirectory.exists(), is(false));
    assertThat(DirectoryOnlyFilter.INSTANCE.accept(nonExistingDirectory), is(false));
  }

  @Test
  public void rejectsNonExistingFile() {
    File nonExistingFile = new File(System.getProperty("user.dir"), "relative/path/to/non/existing/file.ext");

    assertThat(nonExistingFile.exists(), is(false));
    assertThat(DirectoryOnlyFilter.INSTANCE.accept(nonExistingFile), is(false));
  }

  @Test
  public void rejectsNull() {
    assertThat(DirectoryOnlyFilter.INSTANCE.accept(null), is(false));
  }

}
