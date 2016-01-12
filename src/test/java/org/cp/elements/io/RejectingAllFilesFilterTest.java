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
import static org.junit.Assert.assertThat;

import java.io.File;

import org.cp.elements.test.AbstractBaseTestSuite;
import org.junit.Test;

/**
 * The RejectingAllFilesFilterTest class is a test suite of test cases testing the contract and functionality
 * of the RejectingAllFilesFilter class.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.junit.Test
 * @see org.cp.elements.io.RejectingAllFilesFilter
 * @see org.cp.elements.test.AbstractBaseTestSuite
 * @since 1.0.0
 */
public class RejectingAllFilesFilterTest extends AbstractBaseTestSuite {

  @Test
  public void rejectsExistingDirectory() {
    assertThat(WORKING_DIRECTORY.isDirectory(), is(true));
    assertThat(RejectingAllFilesFilter.INSTANCE.accept(WORKING_DIRECTORY), is(false));
  }

  @Test
  public void rejectsExistingFile() {
    File rejectingAllFilesFilterClass = getLocation(RejectingAllFilesFilter.class);

    assertThat(rejectingAllFilesFilterClass, is(notNullValue()));
    assertThat(rejectingAllFilesFilterClass.isFile(), is(true));
    assertThat(RejectingAllFilesFilter.INSTANCE.accept(rejectingAllFilesFilterClass), is(false));
  }

  @Test
  public void rejectsNonExistingDirectory() {
    File nonExistingDirectory = new File("/absolute/path/to/non/existing/directory");

    assertThat(nonExistingDirectory.exists(), is(false));
    assertThat(RejectingAllFilesFilter.INSTANCE.accept(nonExistingDirectory), is(false));
  }

  @Test
  public void rejectsNonExistingFile() {
    File nonExistingFile = new File("relative/path/to/non/existing/file.ext");

    assertThat(nonExistingFile.exists(), is(false));
    assertThat(RejectingAllFilesFilter.INSTANCE.accept(nonExistingFile), is(false));
  }

  @Test
  public void rejectsNull() {
    assertThat(RejectingAllFilesFilter.INSTANCE.accept(null), is(false));
  }

}
