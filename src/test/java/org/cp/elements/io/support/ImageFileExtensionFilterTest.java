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
 * The ImageFileExtensionFilterTest class is a test suite of test cases testing the contract and functionality
 * of the ImageFileExtensionFilter class.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see org.cp.elements.io.support.ImageFileExtensionFilter
 * @see org.junit.Test
 * @since 1.0.0
 */
public class ImageFileExtensionFilterTest {

  @Test
  public void testAccept() {
    ImageFileExtensionFilter fileExtensionFilter = new ImageFileExtensionFilter();

    TestUtils.assertEquals(ImageFileExtensionFilter.IMAGE_FILE_EXTENSIONS, fileExtensionFilter.getFileExtensions());

    for (String fileExtension : ImageFileExtensionFilter.IMAGE_FILE_EXTENSIONS) {
      assertTrue(fileExtensionFilter.accept(new File(String.format("file.%1$s", fileExtension))));
    }
  }

  @Test
  public void testReject() {
    ImageFileExtensionFilter fileExtensionFilter = new ImageFileExtensionFilter();

    assertFalse(fileExtensionFilter.accept(new File("/path/to/a/audio/file.mp3")));
    assertFalse(fileExtensionFilter.accept(new File("absolute/path/to/a/image/file.img")));
    assertFalse(fileExtensionFilter.accept(new File("/path/to/a/video/file.mov")));
    assertFalse(fileExtensionFilter.accept(new File("/path/to/a/source.java")));
    assertFalse(fileExtensionFilter.accept(new File("absolute/path/to/a/binary.class")));
    assertFalse(fileExtensionFilter.accept(new File("/path/to/a/file.ext")));
    assertFalse(fileExtensionFilter.accept(new File("absolute/path/to/a/file")));
  }

}
