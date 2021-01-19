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

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.FileFilter;

import org.cp.elements.lang.LogicalOperator;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit Tests for {@link ComposableFileFilter}.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.cp.elements.io.ComposableFileFilter
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class ComposableFileFilterTests {

  @Mock
  private File mockFile;

  @Mock
  private FileFilter mockFileFilterOne;

  @Mock
  private FileFilter mockFileFilterTwo;

  @Before
  public void setup() {

    when(this.mockFileFilterOne.accept(any(File.class))).thenReturn(true);
    when(this.mockFileFilterTwo.accept(any(File.class))).thenReturn(false);
  }

  @Test
  public void composeReturnsNull() {
    assertThat(ComposableFileFilter.builder().compose(null, null)).isNull();
  }

  @Test
  public void composeReturnsReturnsLeft() {
    assertThat(ComposableFileFilter.builder().compose(this.mockFileFilterOne, null)).isSameAs(this.mockFileFilterOne);
  }

  @Test
  public void composeReturnsReturnsRight() {
    assertThat(ComposableFileFilter.builder().compose(null, this.mockFileFilterTwo)).isSameAs(this.mockFileFilterTwo);
  }

  @Test
  public void composeReturnsComposableFileFilter() {

    FileFilter fileFilter = ComposableFileFilter.builder().compose(this.mockFileFilterOne, this.mockFileFilterTwo);

    assertThat(fileFilter).isInstanceOf(ComposableFileFilter.class);
    assertThat(((ComposableFileFilter) fileFilter).getLeftOperand()).isSameAs(this.mockFileFilterOne);
    assertThat(((ComposableFileFilter) fileFilter).getOperator()).isSameAs(LogicalOperator.AND);
    assertThat(((ComposableFileFilter) fileFilter).getRightOperand()).isSameAs(this.mockFileFilterTwo);
  }

  @Test
  public void composeAndReturnsNull() {
    assertThat(ComposableFileFilter.and(null, null)).isNull();
  }

  @Test
  public void composeAndReturnsLeft() {
    assertThat(ComposableFileFilter.and(this.mockFileFilterOne, null)).isSameAs(this.mockFileFilterOne);
  }

  @Test
  public void composeAndReturnsRight() {
    assertThat(ComposableFileFilter.and(null, this.mockFileFilterTwo)).isSameAs(this.mockFileFilterTwo);
  }

  @Test
  public void composeAndReturnsComposableFileFilter() {

    FileFilter fileFilter = ComposableFileFilter.and(this.mockFileFilterOne, this.mockFileFilterTwo);

    assertThat(fileFilter).isInstanceOf(ComposableFileFilter.class);
    assertThat(((ComposableFileFilter) fileFilter).getLeftOperand()).isSameAs(this.mockFileFilterOne);
    assertThat(((ComposableFileFilter) fileFilter).getOperator()).isEqualTo(LogicalOperator.AND);
    assertThat(((ComposableFileFilter) fileFilter).getRightOperand()).isSameAs(this.mockFileFilterTwo);
  }

  @Test
  public void composeAndWithNoFileFiltersReturnsNull() {
    assertThat(ComposableFileFilter.and()).isNull();
  }

  @Test
  public void composeAndWithNullFileFilterArrayReturnsNull() {
    assertThat(ComposableFileFilter.and((FileFilter[]) null)).isNull();
  }

  @Test
  public void composeOrReturnsNull() {
    assertThat(ComposableFileFilter.or(null, null)).isNull();
  }

  @Test
  public void composeOrReturnsLeft() {
    assertThat(ComposableFileFilter.or(this.mockFileFilterOne, null)).isSameAs(this.mockFileFilterOne);
  }

  @Test
  public void composeOrReturnsRight() {
    assertThat(ComposableFileFilter.or(null, this.mockFileFilterTwo)).isSameAs(this.mockFileFilterTwo);
  }

  @Test
  public void composeOrReturnsComposableFileFilter() {

    FileFilter fileFilter = ComposableFileFilter.or(this.mockFileFilterOne, this.mockFileFilterTwo);

    assertThat(fileFilter).isInstanceOf(ComposableFileFilter.class);
    assertThat(((ComposableFileFilter) fileFilter).getLeftOperand()).isSameAs(this.mockFileFilterOne);
    assertThat(((ComposableFileFilter) fileFilter).getOperator()).isEqualTo(LogicalOperator.OR);
    assertThat(((ComposableFileFilter) fileFilter).getRightOperand()).isSameAs(this.mockFileFilterTwo);
  }

  @Test
  public void composeOrWithNoFileFiltersReturnsNull() {
    assertThat(ComposableFileFilter.or()).isNull();
  }

  @Test
  public void composeOrWithNullFileFilterArrayReturnsNull() {
    assertThat(ComposableFileFilter.or((FileFilter[]) null)).isNull();
  }

  @Test
  public void composeXorReturnsNull() {
    assertThat(ComposableFileFilter.xor(null, null)).isNull();
  }

  @Test
  public void composeXorReturnsLeft() {
    assertThat(ComposableFileFilter.xor(this.mockFileFilterOne, null)).isSameAs(this.mockFileFilterOne);
  }

  @Test
  public void composeXorReturnsRight() {
    assertThat(ComposableFileFilter.xor(null, this.mockFileFilterTwo)).isSameAs(this.mockFileFilterTwo);
  }

  @Test
  public void composeXorReturnsComposableFileFilter() {

    FileFilter fileFilter = ComposableFileFilter.xor(this.mockFileFilterOne, this.mockFileFilterTwo);

    assertThat(fileFilter).isInstanceOf(ComposableFileFilter.class);
    assertThat(((ComposableFileFilter) fileFilter).getLeftOperand()).isSameAs(this.mockFileFilterOne);
    assertThat(((ComposableFileFilter) fileFilter).getOperator()).isEqualTo(LogicalOperator.XOR);
    assertThat(((ComposableFileFilter) fileFilter).getRightOperand()).isSameAs(this.mockFileFilterTwo);
  }

  @Test
  public void acceptAnd() {

    assertThat(ComposableFileFilter.and(this.mockFileFilterOne, this.mockFileFilterOne).accept(this.mockFile)).isTrue();
    assertThat(ComposableFileFilter.and(this.mockFileFilterOne, this.mockFileFilterTwo).accept(this.mockFile)).isFalse();
    assertThat(ComposableFileFilter.and(this.mockFileFilterTwo, this.mockFileFilterOne).accept(this.mockFile)).isFalse();
    assertThat(ComposableFileFilter.and(this.mockFileFilterTwo, this.mockFileFilterTwo).accept(this.mockFile)).isFalse();

    verify(this.mockFileFilterOne, times(3)).accept(eq(this.mockFile));
    verify(this.mockFileFilterTwo, times(3)).accept(eq(this.mockFile));
  }

  @Test
  public void acceptAndWithFileFilterArray() {

    assertThat(ComposableFileFilter.and(this.mockFileFilterOne, this.mockFileFilterOne, this.mockFileFilterOne).accept(this.mockFile))
      .isTrue();
    assertThat(ComposableFileFilter.and(this.mockFileFilterTwo, this.mockFileFilterTwo, this.mockFileFilterOne).accept(this.mockFile))
      .isFalse();
    assertThat(ComposableFileFilter.and(this.mockFileFilterOne, this.mockFileFilterTwo, this.mockFileFilterOne).accept(this.mockFile))
      .isFalse();
    assertThat(ComposableFileFilter.and(this.mockFileFilterTwo, this.mockFileFilterTwo, this.mockFileFilterTwo).accept(this.mockFile))
      .isFalse();
  }

  @Test
  public void acceptOr() {

    assertThat(ComposableFileFilter.or(this.mockFileFilterOne, this.mockFileFilterOne).accept(this.mockFile)).isTrue();
    assertThat(ComposableFileFilter.or(this.mockFileFilterOne, this.mockFileFilterTwo).accept(this.mockFile)).isTrue();
    assertThat(ComposableFileFilter.or(this.mockFileFilterTwo, this.mockFileFilterOne).accept(this.mockFile)).isTrue();
    assertThat(ComposableFileFilter.or(this.mockFileFilterTwo, this.mockFileFilterTwo).accept(this.mockFile)).isFalse();

    verify(this.mockFileFilterOne, times(3)).accept(eq(this.mockFile));
    verify(this.mockFileFilterTwo, times(3)).accept(eq(this.mockFile));
  }

  @Test
  public void acceptOrWithFileFilerArray() {

    assertThat(ComposableFileFilter.or(this.mockFileFilterTwo, this.mockFileFilterTwo, this.mockFileFilterTwo).accept(this.mockFile))
      .isFalse();
    assertThat(ComposableFileFilter.or(this.mockFileFilterOne, this.mockFileFilterTwo, this.mockFileFilterTwo).accept(this.mockFile))
      .isTrue();
    assertThat(ComposableFileFilter.or(this.mockFileFilterTwo, this.mockFileFilterOne, this.mockFileFilterTwo).accept(this.mockFile))
      .isTrue();
    assertThat(ComposableFileFilter.or(this.mockFileFilterOne, this.mockFileFilterOne, this.mockFileFilterTwo).accept(this.mockFile))
      .isTrue();
    assertThat(ComposableFileFilter.or(this.mockFileFilterOne, this.mockFileFilterOne, this.mockFileFilterOne).accept(this.mockFile))
      .isTrue();
  }

  @Test
  public void acceptXor() {

    assertThat(ComposableFileFilter.xor(this.mockFileFilterOne, this.mockFileFilterOne).accept(this.mockFile)).isFalse();
    assertThat(ComposableFileFilter.xor(this.mockFileFilterOne, this.mockFileFilterTwo).accept(this.mockFile)).isTrue();
    assertThat(ComposableFileFilter.xor(this.mockFileFilterTwo, this.mockFileFilterOne).accept(this.mockFile)).isTrue();
    assertThat(ComposableFileFilter.xor(this.mockFileFilterTwo, this.mockFileFilterTwo).accept(this.mockFile)).isFalse();

    verify(this.mockFileFilterOne, times(4)).accept(eq(this.mockFile));
    verify(this.mockFileFilterTwo, times(4)).accept(eq(this.mockFile));
  }
}
