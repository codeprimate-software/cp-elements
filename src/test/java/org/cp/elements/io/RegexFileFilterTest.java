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

import static org.junit.Assert.*;

import java.io.File;
import java.io.IOException;

import org.cp.elements.test.AbstractMockingTestSuite;
import org.jmock.Expectations;
import org.junit.Test;

/**
 * The RegexFileFilterTest class is a test suite of test cases testing the contract and functionality
 * of the RegexFileFilter class.
 *
 * @author John J. Blum
 * @see org.jmock.Mockery
 * @see org.junit.Test
 * @see org.cp.elements.io.RegexFileFilter
 * @see org.cp.elements.test.AbstractMockingTestSuite
 * @since 1.0.0
 */
public class RegexFileFilterTest extends AbstractMockingTestSuite {

  @Test
  public void testConstructRegexFileFilter() {
    RegexFileFilter regexFileFilter = new RegexFileFilter("[.*/]+.*\\.dat");
    assertEquals("[.*/]+.*\\.dat", regexFileFilter.getRegularExpression());
  }

  @Test
  public void testAccept() throws IOException {
    final File mockFileOne = mock(File.class, "testAccept.mockFileOne");
    final File mockFileTwo = mock(File.class, "testAccept.mockFileTwo");

    checking(new Expectations() {{
      allowing(mockFileOne).getCanonicalPath();
      will(returnValue("./db.dat"));
      allowing(mockFileTwo).getCanonicalPath();
      will(returnValue("/path/to/db.dat"));
    }});

    RegexFileFilter regexFileFilter = new RegexFileFilter("[.*/]+.*\\.dat");

    assertTrue(regexFileFilter.accept(mockFileOne));
    assertTrue(regexFileFilter.accept(mockFileTwo));
  }

  @Test
  public void testReject() throws IOException {
    final File mockFile= mock(File.class, "testReject.mockFile");

    checking(new Expectations() {{
      allowing(mockFile).getCanonicalPath();
      will(returnValue("relative/path/to/some/junk.data"));
    }});

    RegexFileFilter regexFileFilter = new RegexFileFilter("[.*/]+.*\\.dat");

    assertFalse(regexFileFilter.accept(mockFile));
  }

}
