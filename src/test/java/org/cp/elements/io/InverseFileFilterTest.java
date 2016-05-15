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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.FileFilter;

import org.junit.Test;

/**
 * The InverseFileFilterTest class is a test suite of test cases testing the contract and functionality
 * of the InverseFileFilter class.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.io.InverseFileFilter
 * @since 1.0.0
 */
public class InverseFileFilterTest {

  @Test
  public void testConstructInverseFileFilter() {
    FileFilter mockFileFiler = mock(FileFilter.class);
    InverseFileFilter inverseFileFilter = new InverseFileFilter(mockFileFiler);
    assertSame(mockFileFiler, inverseFileFilter.getDelegate());
  }

  @Test(expected = IllegalArgumentException.class)
  public void testConstructInverseFileFilterWithNullDelegate() {
    try {
      new InverseFileFilter(null);
    }
    catch (NullPointerException expected) {
      assertEquals("The delegating FileFilter must not be null!", expected.getMessage());
      throw expected;
    }
  }

  @Test
  public void testAccept() {
    File mockFile = mock(File.class);
    FileFilter mockFileFilter = mock(FileFilter.class, "fileFilter");
    InverseFileFilter inverseFileFilter = new InverseFileFilter(mockFileFilter);

    when(mockFileFilter.accept(any(File.class))).thenReturn(false);

    assertTrue(inverseFileFilter.accept(mockFile));

    verify(mockFileFilter, times(1)).accept(eq(mockFile));
  }

  @Test
  public void testReject() {
    File mockFile = mock(File.class);
    FileFilter mockFileFilter = mock(FileFilter.class, "fileFilter");
    InverseFileFilter inverseFileFilter = new InverseFileFilter(mockFileFilter);

    when(mockFileFilter.accept(any(File.class))).thenReturn(true);

    assertFalse(inverseFileFilter.accept(mockFile));

    verify(mockFileFilter, times(1)).accept(eq(mockFile));
  }
}
