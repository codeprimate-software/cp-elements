/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.io.support;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.File;

import org.cp.elements.test.TestUtils;
import org.junit.Test;

/**
 * The VideoFileExtensionFilterTest class is a test suite of test cases testing the contract and functionality
 * of the VideoFileExtensionFilter class.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see org.cp.elements.io.support.VideoFileExtensionFilter
 * @see org.junit.Test
 * @since 1.0.0
 */
public class VideoFileExtensionFilterTest {

  @Test
  public void testAccept() {
    VideoFileExtensionFilter fileExtensionFilter = new VideoFileExtensionFilter();

    TestUtils.assertEquals(VideoFileExtensionFilter.VIDEO_FILE_EXTENSIONS, fileExtensionFilter.getFileExtensions());

    for (String fileExtension : VideoFileExtensionFilter.VIDEO_FILE_EXTENSIONS) {
      assertTrue(fileExtensionFilter.accept(new File(String.format("file.%1$s", fileExtension))));
    }
  }

  @Test
  public void testReject() {
    VideoFileExtensionFilter fileExtensionFilter = new VideoFileExtensionFilter();

    assertFalse(fileExtensionFilter.accept(new File("/path/to/a/audio/file.mp3")));
    assertFalse(fileExtensionFilter.accept(new File("absolute/path/to/a/image/file.jpg")));
    assertFalse(fileExtensionFilter.accept(new File("/path/to/video/file.vid")));
    assertFalse(fileExtensionFilter.accept(new File("/path/to/a/source.java")));
    assertFalse(fileExtensionFilter.accept(new File("absolute/path/to/a/binary.class")));
    assertFalse(fileExtensionFilter.accept(new File("/path/to/a/file.ext")));
    assertFalse(fileExtensionFilter.accept(new File("absolute/path/to/a/file")));
  }

}
