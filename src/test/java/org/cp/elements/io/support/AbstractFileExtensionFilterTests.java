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

package org.cp.elements.io.support;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;

import java.io.File;
import java.util.Arrays;
import java.util.Set;

import org.cp.elements.io.FileExtensionFilter;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * AbstractFileExtensionFilterTests is an abstract base class containing test cases common to all
 * {@link org.cp.elements.io.FileExtensionFilter} tests in the {@link org.cp.elements.io.support} package.
 *
 * @author John Blum
 * @see java.io.File
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.rules.ExpectedException
 * @see org.cp.elements.io.FileExtensionFilter
 * @since 1.0.0
 */
public abstract class AbstractFileExtensionFilterTests {

  @Rule
  public ExpectedException exception = ExpectedException.none();

  protected abstract String[] expectedFileExtensions();

  protected int expectedSize() {
    return expectedFileExtensions().length;
  }

  protected abstract FileExtensionFilter fileExtensionFilter();

  protected File newFile(String pathname) {
    return new File(pathname);
  }

  protected abstract String[] unexpectedFileExtensions();

  @Test
  public void acceptIsSuccessfulWithExpectedFileExtensions() {
    Set<String> fileExtensions = fileExtensionFilter().getFileExtensions();

    assertThat(fileExtensions, is(notNullValue(Set.class)));
    assertThat(fileExtensions.size(), is(equalTo(expectedSize())));
    assertThat(fileExtensions.containsAll(Arrays.asList(expectedFileExtensions())), is(true));

    for (String fileExtension : fileExtensions) {
      assertThat(fileExtensionFilter().accept(newFile(String.format("file.%s", fileExtension))), is(true));
    }
  }

  @Test
  public void acceptWithNullThrowsIllegalArgumentException() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("File cannot be null");

    fileExtensionFilter().accept(null);
  }

  @Test
  public void rejectIsSuccessfulWithUnexpectedFileExtensions() {
    for (String fileExtension : unexpectedFileExtensions()) {
      assertThat(fileExtensionFilter().accept(newFile(fileExtension)), is(false));
    }
  }
}
