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
 * The DefaultFileFilterTest class is a test suite of test cases testing the contract and functionality
 * of the DefaultFileFilter class.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.junit.Test
 * @see org.cp.elements.io.DefaultFileFilter
 * @see org.cp.elements.test.AbstractBaseTestSuite
 * @since 1.0.0
 */
public class DefaultFileFilterTest extends AbstractBaseTestSuite {

  @Test
  public void acceptsAllFiles() {
    assertThat(DefaultFileFilter.DEFAULT_ACCEPT.accept(WORKING_DIRECTORY), is(true));
    assertThat(DefaultFileFilter.DEFAULT_ACCEPT.accept(getLocation(DefaultFileFilter.class)), is(true));
    assertThat(DefaultFileFilter.DEFAULT_ACCEPT.accept(new File("/absolute/path/to/non/existing/directory")), is(true));
    assertThat(DefaultFileFilter.DEFAULT_ACCEPT.accept(new File("relative/path/to/non/existing/file.ext")), is(true));
    assertThat(DefaultFileFilter.DEFAULT_ACCEPT.accept(null), is(true));
  }

  @Test
  public void rejectsAllFiles() {
    assertThat(DefaultFileFilter.DEFAULT_REJECT.accept(WORKING_DIRECTORY), is(false));
    assertThat(DefaultFileFilter.DEFAULT_REJECT.accept(getLocation(DefaultFileFilter.class)), is(false));
    assertThat(DefaultFileFilter.DEFAULT_REJECT.accept(new File("/absolute/path/to/non/existing/directory")), is(false));
    assertThat(DefaultFileFilter.DEFAULT_REJECT.accept(new File("relative/path/to/non/existing/file.ext")), is(false));
    assertThat(DefaultFileFilter.DEFAULT_REJECT.accept(null), is(false));
  }

}
