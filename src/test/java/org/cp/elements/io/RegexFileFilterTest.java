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

package org.cp.elements.io;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.IOException;

import org.junit.Test;

/**
 * The RegexFileFilterTest class is a test suite of test cases testing the contract and functionality
 * of the RegexFileFilter class.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.io.RegexFileFilter
 * @since 1.0.0
 */
@SuppressWarnings("all")
public class RegexFileFilterTest {

  @Test
  public void testConstructRegexFileFilter() {
    assertEquals("[.*/]+.*\\.dat", new RegexFileFilter("[.*/]+.*\\.dat").getRegularExpression());
  }

  @Test
  public void testAccept() throws IOException {
    File mockFileOne = mock(File.class, "mockFileOne");
    File mockFileTwo = mock(File.class, "mockFileTwo");

    when(mockFileOne.getCanonicalPath()).thenReturn("./db.dat");
    when(mockFileTwo.getCanonicalPath()).thenReturn("/path/to/db.dat");

    RegexFileFilter regexFileFilter = new RegexFileFilter("[.*/]+.*\\.dat");

    assertTrue(regexFileFilter.accept(mockFileOne));
    assertTrue(regexFileFilter.accept(mockFileTwo));

    verify(mockFileOne, times(1)).getCanonicalPath();
    verify(mockFileTwo, times(1)).getCanonicalPath();
  }

  @Test
  public void testReject() throws IOException {
    File mockFile= mock(File.class, "mockFile");

    when(mockFile.getCanonicalPath()).thenReturn("relative/path/to/some/junk.data");

    RegexFileFilter regexFileFilter = new RegexFileFilter("[.*/]+.*\\.dat");

    assertFalse(regexFileFilter.accept(mockFile));

    verify(mockFile, times(1)).getCanonicalPath();
  }

}
