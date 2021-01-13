/*
 * Copyright 2011-Present Author or Authors.
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

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.IOException;
import java.util.regex.Pattern;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Test suite of test cases testing the contract and functionality of the {@link RegexFileFilter} class.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.rules.ExpectedException
 * @see org.mockito.Mockito
 * @see org.cp.elements.io.RegexFileFilter
 * @since 1.0.0
 */
@SuppressWarnings("all")
public class RegexFileFilterTests {

  @Rule
  public ExpectedException exception = ExpectedException.none();

  protected File mockFile(String name) {
    return mock(File.class, name);
  }

  @Test
  public void constructRegexFileFilterWithRegularExpression() {
    assertThat(new RegexFileFilter("[a-zA-Z0-9_*/]+[a-zA-Z0-9_]+\\.dat").getRegularExpression(),
      is(equalTo("[a-zA-Z0-9_*/]+[a-zA-Z0-9_]+\\.dat")));
  }

  @Test
  public void constructRegexFileFilterWithPattern() {
    Pattern expectedPattern = Pattern.compile("[a-zA-Z0-9_]+");
    assertThat(new RegexFileFilter(expectedPattern).getPattern(), is(equalTo(expectedPattern)));
  }

  @Test
  public void constructRegexFileFilterWithNullPattern() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("The Regular Expression (Pattern) cannot be null");

    new RegexFileFilter((Pattern) null);
  }

  @Test
  public void acceptsMatchingFiles() throws IOException {
    File mockFileOne = mockFile("MockFileOne");
    File mockFileTwo = mockFile("MockFileTwo");
    File mockFileThree = mockFile("MockFileThree");

    when(mockFileOne.getCanonicalPath()).thenReturn("./db.dat");
    when(mockFileTwo.getCanonicalPath()).thenReturn("/absolute/path/to/db.dat");
    when(mockFileThree.getCanonicalPath()).thenReturn("relative/path/to/db.dat");

    RegexFileFilter regexFileFilter = new RegexFileFilter("[a-zA-Z0-9_/\\.]+.*\\.dat");

    assertThat(regexFileFilter.accept(mockFileOne), is(true));
    assertThat(regexFileFilter.accept(mockFileTwo), is(true));
    assertThat(regexFileFilter.accept(mockFileThree), is(true));

    verify(mockFileOne, times(1)).getCanonicalPath();
    verify(mockFileTwo, times(1)).getCanonicalPath();
    verify(mockFileThree, times(1)).getCanonicalPath();
  }

  @Test
  public void rejectsNonMatchingFiles() throws IOException {
    File mockFile= mockFile("MockFile");

    when(mockFile.getCanonicalPath()).thenReturn("/path/to/some/junk.data");

    RegexFileFilter regexFileFilter = new RegexFileFilter("[.*/]+.*\\.dat");

    assertThat(regexFileFilter.accept(mockFile), is(false));

    verify(mockFile, times(1)).getCanonicalPath();
  }

  @Test
  public void rejectsNullFiles() {
    assertThat(new RegexFileFilter(".").accept(null), is(false));
  }
}
