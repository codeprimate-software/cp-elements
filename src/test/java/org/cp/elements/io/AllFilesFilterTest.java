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

import java.io.File;

import org.cp.elements.test.AbstractBaseTestSuite;
import org.junit.Test;

/**
 * The AllFilesFilterTest class is a test suite of test cases testing the contract and functionality
 * of the AllFilesFilter class.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see org.junit.Test
 * @see org.cp.elements.io.AllFilesFilter
 * @since 1.0.0
 */
public class AllFilesFilterTest extends AbstractBaseTestSuite {

  @Test
  public void acceptsExistingDirectory() {
    assertThat(AllFilesFilter.INSTANCE.accept(WORKING_DIRECTORY), is(true));
  }

  @Test
  public void acceptsExistingFile() {
    assertThat(AllFilesFilter.INSTANCE.accept(getLocation(AllFilesFilter.class)), is(true));
  }

  @Test
  public void acceptsNonExistingDirectory() {
    assertThat(AllFilesFilter.INSTANCE.accept(new File("/absolute/path/to/non/existing/directory")), is(true));
  }

  @Test
  public void acceptsNonExistingFile() {
    assertThat(AllFilesFilter.INSTANCE.accept(new File("relative/path/to/non/existing/file.ext")), is(true));
  }

  @Test
  public void rejectsNull() {
    assertThat(AllFilesFilter.INSTANCE.accept(null), is(false));
  }

}
