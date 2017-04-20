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

import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.FileFilter;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Test suite of test cases testing the contract and functionality of the {@link InverseFileFilter} class.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.junit.rules.ExpectedException
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.cp.elements.io.InverseFileFilter
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class InverseFileFilterTests {

  @Rule
  public ExpectedException exception = ExpectedException.none();

  @Mock
  private File mockFile;

  @Mock
  private FileFilter mockFileFilter;

  @Test
  public void constructInverseFileFilter() {
    InverseFileFilter inverseFileFilter = new InverseFileFilter(mockFileFilter);

    assertThat(inverseFileFilter.getDelegate(), is(sameInstance(mockFileFilter)));
  }

  @Test
  public void constructInverseFileFilterWithNullDelegate() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("FileFilter must not be null");

    new InverseFileFilter(null);
  }

  @Test
  public void accept() {
    InverseFileFilter inverseFileFilter = new InverseFileFilter(mockFileFilter);

    when(mockFileFilter.accept(any(File.class))).thenReturn(false);

    assertThat(inverseFileFilter.accept(mockFile), is(true));

    verify(mockFileFilter, times(1)).accept(eq(mockFile));
  }

  @Test
  public void reject() {
    InverseFileFilter inverseFileFilter = new InverseFileFilter(mockFileFilter);

    when(mockFileFilter.accept(any(File.class))).thenReturn(true);

    assertThat(inverseFileFilter.accept(mockFile), is(false));

    verify(mockFileFilter, times(1)).accept(eq(mockFile));
  }
}
