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

package org.cp.elements.io.support;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.Arrays;
import java.util.Set;

import org.junit.Test;

/**
 * Test suite of test cases testing the contract and functionality of the {@link TextFileExtensionFilter} class.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see org.junit.Test
 * @see org.cp.elements.io.support.TextFileExtensionFilter
 * @since 1.0.0
 */
public class TextFileExtensionFilterTest {

  private TextFileExtensionFilter fileExtensionFilter = new TextFileExtensionFilter();

  protected File newFile(String pathname) {
    return new File(pathname);
  }

  @Test
  public void acceptIsSuccessful() {
    Set<String> fileExtensions = fileExtensionFilter.getFileExtensions();

    assertThat(fileExtensions, is(notNullValue(Set.class)));
    assertThat(fileExtensions.size(), is(equalTo(TextFileExtensionFilter.TEXT_FILE_EXTENSIONS.length)));
    assertThat(fileExtensions.containsAll(Arrays.asList(TextFileExtensionFilter.TEXT_FILE_EXTENSIONS)), is(true));

    for (String fileExtension : TextFileExtensionFilter.TEXT_FILE_EXTENSIONS) {
      assertTrue(fileExtensionFilter.accept(newFile(String.format("file.%1$s", fileExtension))));
    }
  }

  @Test
  public void rejectIsSuccessful() {
    assertFalse(fileExtensionFilter.accept(newFile("/path/to/a/file.bin")));
    assertFalse(fileExtensionFilter.accept(newFile("absolute/path/to/a/file.pdf")));
    assertFalse(fileExtensionFilter.accept(newFile("/path/to/file.wrt")));
    assertFalse(fileExtensionFilter.accept(newFile("/path/to/a/source.java")));
    assertFalse(fileExtensionFilter.accept(newFile("absolute/path/to/a/binary.class")));
    assertFalse(fileExtensionFilter.accept(newFile("/path/to/a/file.ext")));
    assertFalse(fileExtensionFilter.accept(newFile("absolute/path/to/a/file")));
  }
}
