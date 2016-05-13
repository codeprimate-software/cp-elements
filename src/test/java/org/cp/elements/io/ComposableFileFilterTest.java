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

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.instanceOf;
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

import org.cp.elements.lang.LogicalOperator;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

/**
 * The ComposableFileFilterTest class is a test suite of test cases testing the contract and functionality
 * of the {@link ComposableFileFilter} class.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.runners.MockitoJUnitRunner
 * @see org.cp.elements.io.ComposableFileFilter
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class ComposableFileFilterTest {

  @Mock
  private File mockFile;

  @Mock
  private FileFilter mockFileFilterOne;

  @Mock
  private FileFilter mockFileFilterTwo;

  @Test
  public void composeAndReturnsNull() {
    assertThat(ComposableFileFilter.and(null, null), is(nullValue()));
  }

  @Test
  public void composeAndReturnsLeft() {
    assertThat(ComposableFileFilter.and(mockFileFilterOne, null), is(sameInstance(mockFileFilterOne)));
  }

  @Test
  public void composeAndReturnsRight() {
    assertThat(ComposableFileFilter.and(null, mockFileFilterTwo), is(sameInstance(mockFileFilterTwo)));
  }

  @Test
  public void composeAndReturnsComposableFileFilter() {
    FileFilter fileFilter = ComposableFileFilter.and(mockFileFilterOne, mockFileFilterTwo);

    assertThat(fileFilter, is(instanceOf(ComposableFileFilter.class)));
    assertThat(((ComposableFileFilter) fileFilter).getLeftOperand(), is(sameInstance(mockFileFilterOne)));
    assertThat(((ComposableFileFilter) fileFilter).getOperator(), is(equalTo(LogicalOperator.AND)));
    assertThat(((ComposableFileFilter) fileFilter).getRightOperand(), is(sameInstance(mockFileFilterTwo)));
  }

  @Test
  public void composeOrReturnsNull() {
    assertThat(ComposableFileFilter.or(null, null), is(nullValue()));
  }

  @Test
  public void composeOrReturnsLeft() {
    assertThat(ComposableFileFilter.or(mockFileFilterOne, null), is(sameInstance(mockFileFilterOne)));
  }

  @Test
  public void composeOrReturnsRight() {
    assertThat(ComposableFileFilter.or(null, mockFileFilterTwo), is(sameInstance(mockFileFilterTwo)));
  }

  @Test
  public void composeOrReturnsComposableFileFilter() {
    FileFilter fileFilter = ComposableFileFilter.or(mockFileFilterOne, mockFileFilterTwo);

    assertThat(fileFilter, is(instanceOf(ComposableFileFilter.class)));
    assertThat(((ComposableFileFilter) fileFilter).getLeftOperand(), is(sameInstance(mockFileFilterOne)));
    assertThat(((ComposableFileFilter) fileFilter).getOperator(), is(equalTo(LogicalOperator.OR)));
    assertThat(((ComposableFileFilter) fileFilter).getRightOperand(), is(sameInstance(mockFileFilterTwo)));
  }

  @Test
  public void composeXorReturnsNull() {
    assertThat(ComposableFileFilter.xor(null, null), is(nullValue()));
  }

  @Test
  public void composeXorReturnsLeft() {
    assertThat(ComposableFileFilter.xor(mockFileFilterOne, null), is(sameInstance(mockFileFilterOne)));
  }

  @Test
  public void composeXorReturnsRight() {
    assertThat(ComposableFileFilter.xor(null, mockFileFilterTwo), is(sameInstance(mockFileFilterTwo)));
  }

  @Test
  public void composeXorReturnsComposableFileFilter() {
    FileFilter fileFilter = ComposableFileFilter.xor(mockFileFilterOne, mockFileFilterTwo);

    assertThat(fileFilter, is(instanceOf(ComposableFileFilter.class)));
    assertThat(((ComposableFileFilter) fileFilter).getLeftOperand(), is(sameInstance(mockFileFilterOne)));
    assertThat(((ComposableFileFilter) fileFilter).getOperator(), is(equalTo(LogicalOperator.XOR)));
    assertThat(((ComposableFileFilter) fileFilter).getRightOperand(), is(sameInstance(mockFileFilterTwo)));
  }

  @Test
  public void acceptAnd() {
    when(mockFileFilterOne.accept(any(File.class))).thenReturn(true);
    when(mockFileFilterTwo.accept(any(File.class))).thenReturn(false);

    assertThat(ComposableFileFilter.and(mockFileFilterOne, mockFileFilterOne).accept(mockFile), is(true));
    assertThat(ComposableFileFilter.and(mockFileFilterOne, mockFileFilterTwo).accept(mockFile), is(false));
    assertThat(ComposableFileFilter.and(mockFileFilterTwo, mockFileFilterOne).accept(mockFile), is(false));
    assertThat(ComposableFileFilter.and(mockFileFilterTwo, mockFileFilterTwo).accept(mockFile), is(false));

    verify(mockFileFilterOne, times(3)).accept(eq(mockFile));
    verify(mockFileFilterTwo, times(3)).accept(eq(mockFile));
  }

  @Test
  public void acceptOr() {
    when(mockFileFilterOne.accept(any(File.class))).thenReturn(true);
    when(mockFileFilterTwo.accept(any(File.class))).thenReturn(false);

    assertThat(ComposableFileFilter.or(mockFileFilterOne, mockFileFilterOne).accept(mockFile), is(true));
    assertThat(ComposableFileFilter.or(mockFileFilterOne, mockFileFilterTwo).accept(mockFile), is(true));
    assertThat(ComposableFileFilter.or(mockFileFilterTwo, mockFileFilterOne).accept(mockFile), is(true));
    assertThat(ComposableFileFilter.or(mockFileFilterTwo, mockFileFilterTwo).accept(mockFile), is(false));

    verify(mockFileFilterOne, times(3)).accept(eq(mockFile));
    verify(mockFileFilterTwo, times(3)).accept(eq(mockFile));
  }

  @Test
  public void acceptXor() {
    when(mockFileFilterOne.accept(any(File.class))).thenReturn(true);
    when(mockFileFilterTwo.accept(any(File.class))).thenReturn(false);

    assertThat(ComposableFileFilter.xor(mockFileFilterOne, mockFileFilterOne).accept(mockFile), is(false));
    assertThat(ComposableFileFilter.xor(mockFileFilterOne, mockFileFilterTwo).accept(mockFile), is(true));
    assertThat(ComposableFileFilter.xor(mockFileFilterTwo, mockFileFilterOne).accept(mockFile), is(true));
    assertThat(ComposableFileFilter.xor(mockFileFilterTwo, mockFileFilterTwo).accept(mockFile), is(false));

    verify(mockFileFilterOne, times(4)).accept(eq(mockFile));
    verify(mockFileFilterTwo, times(4)).accept(eq(mockFile));
  }

}
