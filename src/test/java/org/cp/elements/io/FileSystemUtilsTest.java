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

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * The FileSystemUtilsTest class is a test suite of test cases testing the contract and functionality
 * of the FileSystemUtilsTest class.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.io.FileSystemUtils
 * @since 1.0.0
 */
public class FileSystemUtilsTest {

  @Rule
  public ExpectedException expectedException = ExpectedException.none();

  @Test
  public void appendToPathWithNonNullBasePathnameAndNonEmptyPathElements() {
    assertThat(FileSystemUtils.appendToPath("/absolute/path/to", "a", "relative", "location", "containing", "file.ext"),
      is(equalTo("/absolute/path/to/a/relative/location/containing/file.ext")));
  }

  @Test
  public void appendToPathWithNonNullBasePathnameAndEmptyPathElements() {
    assertThat(FileSystemUtils.appendToPath("/absolute/path/to/nothing"), is(equalTo("/absolute/path/to/nothing")));
  }

  @Test
  public void appendToPathWithNullBasePathname() {
    expectedException.expect(NullPointerException.class);
    expectedException.expectCause(is(nullValue(Throwable.class)));
    expectedException.expectMessage("basePathname cannot be null");

    FileSystemUtils.appendToPath(null, "relative", "path", "to", "file.ext");
  }

}
