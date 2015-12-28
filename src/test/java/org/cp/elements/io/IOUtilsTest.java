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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.Closeable;
import java.io.IOException;

import org.cp.elements.test.AbstractMockingTestSuite;
import org.jmock.Expectations;
import org.junit.Test;

/**
 * The IOUtilsTest class is a test suite for testing the contract and functionality of the IOUtils class.
 *
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
