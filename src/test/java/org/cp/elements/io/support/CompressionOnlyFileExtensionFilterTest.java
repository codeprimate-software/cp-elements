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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.File;

import org.cp.elements.test.TestUtils;
import org.junit.Test;

/**
 * The CompressionOnlyFileExtensionFilterTest class is a test suite of test cases testing the contract and functionality
 * of the CompressionOnlyFileExtensionFilter class.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see org.cp.elements.io.support.CompressionOnlyFileExtensionFilterTest
 * @see org.junit.Test
 * @since 1.0.0
 */
public class CompressionOnlyFileExtensionFilterTest {

  @Test
  public void testAccept() {
    CompressionOnlyFileExtensionFilter fileExtensionFilter = new CompressionOnlyFileExtensionFilter();

    TestUtils.assertEquals(CompressionOnlyFileExtensionFilter.COMPRESSION_ONLY_FILE_EXTENSIONS,
      fileExtensionFilter.getFileExtensions());

    for (String fileExtension : CompressionOnlyFileExtensionFilter.COMPRESSION_ONLY_FILE_EXTENSIONS) {
      assertTrue(fileExtensionFilter.accept(new File(String.format("file.%1$s", fileExtension))));
    }
  }

  @Test
  public void testReject() {
    CompressionOnlyFileExtensionFilter fileExtensionFilter = new CompressionOnlyFileExtensionFilter();

    assertFalse(fileExtensionFilter.accept(new File("/path/to/a/file/archive.tar")));
    assertFalse(fileExtensionFilter.accept(new File("absolute/path/to/a/file.jar")));
    assertFalse(fileExtensionFilter.accept(new File("/path/to/a/source.java")));
    assertFalse(fileExtensionFilter.accept(new File("absolute/path/to/a/binary.class")));
    assertFalse(fileExtensionFilter.accept(new File("/path/to/a/file.ext")));
    assertFalse(fileExtensionFilter.accept(new File("absolute/path/to/a/file")));
  }

}
