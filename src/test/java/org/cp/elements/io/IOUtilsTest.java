/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 *
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 *
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 *
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 *
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.io;

import static org.junit.Assert.*;

import java.io.Closeable;
import java.io.IOException;

import org.cp.elements.test.AbstractMockingTestSuite;
import org.jmock.Expectations;
import org.junit.Test;

/**
 * The IOUtilsTest class is a test suite for testing the contract and functionality of the IOUtils class.
 * </p>
 * @author John J. Blum
 * @since 1.0.0
 * @see org.cp.elements.io.IOUtils
 * @see org.cp.elements.test.AbstractMockingTestSuite
 * @see org.easymock.EasyMock
 * @see org.junit.Test
 */
public class IOUtilsTest extends AbstractMockingTestSuite {

  @Test
  public void testClose() throws Exception {
    final Closeable mockCloseable = mockContext.mock(Closeable.class);

    mockContext.checking(new Expectations() {{
      oneOf(mockCloseable).close();
    }});

    assertTrue(IOUtils.close(mockCloseable));
  }

  @Test
  public void testCloseThrowsIOException() throws Exception {
    final Closeable mockCloseable = mockContext.mock(Closeable.class);

    mockContext.checking(new Expectations() {{
      oneOf(mockCloseable).close();
      will(throwException(new IOException("i/o error!")));
    }});

    assertFalse(IOUtils.close(mockCloseable));
  }

}
