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
