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
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.io.File;
import java.io.FileFilter;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.assertj.core.api.InstanceOfAssertFactories;
import org.cp.elements.lang.LogicalOperator;
import org.mockito.Mock;
import org.mockito.Mock.Strictness;
import org.mockito.junit.jupiter.MockitoExtension;

/**
 * Unit Tests for {@link ComposableFileFilter}.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.cp.elements.io.ComposableFileFilter
 * @since 1.0.0
 */
@ExtendWith(MockitoExtension.class)
public class ComposableFileFilterUnitTests {

  @Mock(strictness = Strictness.LENIENT)
  private File mockFile;

  @Mock(strictness = Strictness.LENIENT)
  private FileFilter mockFileFilterOne;

  @Mock(strictness = Strictness.LENIENT)
  private FileFilter mockFileFilterTwo;

  @BeforeEach
  public void setup() {
    doReturn(true).when(this.mockFileFilterOne).accept(any(File.class));
    doReturn(false).when(this.mockFileFilterTwo).accept(any(File.class));
  }

  @Test
  public void composableFileFilterBuilderIsSingletonInstance() {
    assertThat(ComposableFileFilter.builder()).isSameAs(ComposableFileFilter.INSTANCE);
  }

  @Test
  public void composeReturnsDefaultFileFilter() {
    assertThat(ComposableFileFilter.builder().compose(null, null))
      .isEqualTo(ComposableFileFilter.DEFAULT_FILE_FILTER);
  }

  @Test
  public void composeReturnsLeftOperand() {

    assertThat(ComposableFileFilter.builder().compose(this.mockFileFilterOne, null))
      .isSameAs(this.mockFileFilterOne);

    verifyNoInteractions(this.mockFileFilterOne);
  }

  @Test
  public void composeReturnsRightOperand() {

    assertThat(ComposableFileFilter.builder().compose(null, this.mockFileFilterTwo))
      .isSameAs(this.mockFileFilterTwo);

    verifyNoInteractions(this.mockFileFilterTwo);
  }

  @Test
  public void composeReturnsCompositeFileFilter() {

    FileFilter fileFilter = ComposableFileFilter.builder().compose(this.mockFileFilterOne, this.mockFileFilterTwo);

    assertThat(fileFilter).isInstanceOf(ComposableFileFilter.class);

    assertThat(fileFilter)
      .asInstanceOf(InstanceOfAssertFactories.type(ComposableFileFilter.class))
      .extracting(ComposableFileFilter::getLeftOperand)
      .isSameAs(this.mockFileFilterOne);

    assertThat(fileFilter)
      .asInstanceOf(InstanceOfAssertFactories.type(ComposableFileFilter.class))
      .extracting(ComposableFileFilter::getOperator)
      .isEqualTo(LogicalOperator.AND);

    assertThat(fileFilter)
      .asInstanceOf(InstanceOfAssertFactories.type(ComposableFileFilter.class))
      .extracting(ComposableFileFilter::getRightOperand)
      .isSameAs(this.mockFileFilterTwo);

    verifyNoInteractions(this.mockFileFilterOne, this.mockFileFilterTwo);
  }

  @Test
  public void composeAndReturnsDefaultFileFilter() {
    assertThat(ComposableFileFilter.and(null, null)).isEqualTo(ComposableFileFilter.DEFAULT_FILE_FILTER);
  }

  @Test
  public void composeAndReturnsLeftOperand() {

    assertThat(ComposableFileFilter.and(this.mockFileFilterOne, null)).isSameAs(this.mockFileFilterOne);

    verifyNoInteractions(this.mockFileFilterOne);
  }

  @Test
  public void composeAndReturnsRightOperand() {

    assertThat(ComposableFileFilter.and(null, this.mockFileFilterTwo)).isSameAs(this.mockFileFilterTwo);

    verifyNoInteractions(this.mockFileFilterTwo);
  }

  @Test
  public void composeAndReturnsCompositeFileFilter() {

    FileFilter fileFilter = ComposableFileFilter.and(this.mockFileFilterOne, this.mockFileFilterTwo);

    assertThat(fileFilter).isInstanceOf(ComposableFileFilter.class);

    assertThat(fileFilter)
      .asInstanceOf(InstanceOfAssertFactories.type(ComposableFileFilter.class))
      .extracting(ComposableFileFilter::getLeftOperand)
      .isSameAs(this.mockFileFilterOne);

    assertThat(fileFilter)
      .asInstanceOf(InstanceOfAssertFactories.type(ComposableFileFilter.class))
      .extracting(ComposableFileFilter::getOperator)
      .isEqualTo(LogicalOperator.AND);

    assertThat(fileFilter)
      .asInstanceOf(InstanceOfAssertFactories.type(ComposableFileFilter.class))
      .extracting(ComposableFileFilter::getRightOperand)
      .isSameAs(this.mockFileFilterTwo);

    verifyNoInteractions(this.mockFileFilterOne, this.mockFileFilterTwo);
  }

  @Test
  public void composeFileFilterArrayReturnsCompositeFileFilter() {

    FileFilter fileFilter =
      ComposableFileFilter.and(this.mockFileFilterOne, this.mockFileFilterTwo, this.mockFileFilterOne);

    assertThat(fileFilter).isInstanceOf(ComposableFileFilter.class);

    ComposableFileFilter compositeFileFilter = (ComposableFileFilter) fileFilter;

    assertThat(compositeFileFilter.getLeftOperand()).isInstanceOf(ComposableFileFilter.class);
    assertThat(compositeFileFilter.getOperator()).isEqualTo(LogicalOperator.AND);
    assertThat(compositeFileFilter.getRightOperand()).isSameAs(this.mockFileFilterOne);

    ComposableFileFilter leftOperandCompositeFileFilter = (ComposableFileFilter) compositeFileFilter.getLeftOperand();

    assertThat(leftOperandCompositeFileFilter.getLeftOperand()).isSameAs(this.mockFileFilterOne);
    assertThat(leftOperandCompositeFileFilter.getOperator()).isEqualTo(LogicalOperator.AND);
    assertThat(leftOperandCompositeFileFilter.getRightOperand()).isSameAs(this.mockFileFilterTwo);

    verifyNoInteractions(this.mockFileFilterOne, this.mockFileFilterTwo);
  }

  @Test
  public void composeAndWithNoFileFiltersReturnsDefaultFileFilter() {
    assertThat(ComposableFileFilter.and()).isEqualTo(ComposableFileFilter.DEFAULT_FILE_FILTER);
  }

  @Test
  public void composeAndWithNullFileFilterArrayIsNullSafeReturnsDefaultFileFilter() {
    assertThat(ComposableFileFilter.and((FileFilter[]) null)).isEqualTo(ComposableFileFilter.DEFAULT_FILE_FILTER);
  }

  @Test
  public void composeOrReturnsDefaultFileFilter() {
    assertThat(ComposableFileFilter.or(null, null)).isEqualTo(ComposableFileFilter.DEFAULT_FILE_FILTER);
  }

  @Test
  public void composeOrReturnsLeftOperand() {

    assertThat(ComposableFileFilter.or(this.mockFileFilterOne, null)).isSameAs(this.mockFileFilterOne);

    verifyNoInteractions(this.mockFileFilterOne);
  }

  @Test
  public void composeOrReturnsRightOperand() {

    assertThat(ComposableFileFilter.or(null, this.mockFileFilterTwo)).isSameAs(this.mockFileFilterTwo);

    verifyNoInteractions(this.mockFileFilterTwo);
  }

  @Test
  public void composeOrReturnsCompositeFileFilter() {

    FileFilter fileFilter = ComposableFileFilter.or(this.mockFileFilterOne, this.mockFileFilterTwo);

    assertThat(fileFilter).isInstanceOf(ComposableFileFilter.class);

    assertThat(fileFilter)
      .asInstanceOf(InstanceOfAssertFactories.type(ComposableFileFilter.class))
      .extracting(ComposableFileFilter::getLeftOperand)
      .isSameAs(this.mockFileFilterOne);

    assertThat(fileFilter)
      .asInstanceOf(InstanceOfAssertFactories.type(ComposableFileFilter.class))
      .extracting(ComposableFileFilter::getOperator)
      .isEqualTo(LogicalOperator.OR);

    assertThat(fileFilter)
      .asInstanceOf(InstanceOfAssertFactories.type(ComposableFileFilter.class))
      .extracting(ComposableFileFilter::getRightOperand)
      .isSameAs(this.mockFileFilterTwo);

    verifyNoInteractions(this.mockFileFilterOne, this.mockFileFilterTwo);
  }

  @Test
  public void composeOrFileFilterArrayReturnsCompositeFileFilter() {

    FileFilter fileFilter =
      ComposableFileFilter.or(this.mockFileFilterOne, this.mockFileFilterTwo, this.mockFileFilterOne);

    assertThat(fileFilter).isInstanceOf(ComposableFileFilter.class);

    ComposableFileFilter compositeFileFilter = (ComposableFileFilter) fileFilter;

    assertThat(compositeFileFilter.getLeftOperand()).isInstanceOf(ComposableFileFilter.class);
    assertThat(compositeFileFilter.getOperator()).isEqualTo(LogicalOperator.OR);
    assertThat(compositeFileFilter.getRightOperand()).isSameAs(this.mockFileFilterOne);

    ComposableFileFilter leftOperandCompositeFileFilter = (ComposableFileFilter) compositeFileFilter.getLeftOperand();

    assertThat(leftOperandCompositeFileFilter.getLeftOperand()).isSameAs(this.mockFileFilterOne);
    assertThat(leftOperandCompositeFileFilter.getOperator()).isEqualTo(LogicalOperator.OR);
    assertThat(leftOperandCompositeFileFilter.getRightOperand()).isSameAs(this.mockFileFilterTwo);

    verifyNoInteractions(this.mockFileFilterOne, this.mockFileFilterTwo);
  }

  @Test
  public void composeOrWithNoFileFiltersReturnsDefaultFileFilter() {
    assertThat(ComposableFileFilter.or()).isEqualTo(ComposableFileFilter.DEFAULT_FILE_FILTER);
  }

  @Test
  public void composeOrWithNullFileFilterArrayIsNullSafeReturnsDefaultFileFilter() {
    assertThat(ComposableFileFilter.or((FileFilter[]) null)).isEqualTo(ComposableFileFilter.DEFAULT_FILE_FILTER);
  }

  @Test
  public void composeXorReturnsDefaultFileFilter() {
    assertThat(ComposableFileFilter.xor(null, null))
      .isEqualTo(ComposableFileFilter.DEFAULT_FILE_FILTER);
  }

  @Test
  public void composeXorReturnsLeftOperand() {

    assertThat(ComposableFileFilter.xor(this.mockFileFilterOne, null)).isSameAs(this.mockFileFilterOne);

    verifyNoInteractions(this.mockFileFilterOne);
  }

  @Test
  public void composeXorReturnsRightOperand() {

    assertThat(ComposableFileFilter.xor(null, this.mockFileFilterTwo)).isSameAs(this.mockFileFilterTwo);

    verifyNoInteractions(this.mockFileFilterTwo);
  }

  @Test
  public void composeXorReturnsCompositeFileFilter() {

    FileFilter fileFilter = ComposableFileFilter.xor(this.mockFileFilterOne, this.mockFileFilterTwo);

    assertThat(fileFilter).isInstanceOf(ComposableFileFilter.class);

    assertThat(fileFilter)
      .asInstanceOf(InstanceOfAssertFactories.type(ComposableFileFilter.class))
      .extracting(ComposableFileFilter::getLeftOperand)
      .isSameAs(this.mockFileFilterOne);

    assertThat(fileFilter)
      .asInstanceOf(InstanceOfAssertFactories.type(ComposableFileFilter.class))
      .extracting(ComposableFileFilter::getOperator)
      .isEqualTo(LogicalOperator.XOR);

    assertThat(fileFilter)
      .asInstanceOf(InstanceOfAssertFactories.type(ComposableFileFilter.class))
      .extracting(ComposableFileFilter::getRightOperand)
      .isSameAs(this.mockFileFilterTwo);

    verifyNoInteractions(this.mockFileFilterOne, this.mockFileFilterTwo);
  }

  @Test
  public void acceptAnd() {

    assertThat(ComposableFileFilter.and(this.mockFileFilterOne, this.mockFileFilterOne).accept(this.mockFile)).isTrue();
    assertThat(ComposableFileFilter.and(this.mockFileFilterOne, this.mockFileFilterTwo).accept(this.mockFile)).isFalse();
    assertThat(ComposableFileFilter.and(this.mockFileFilterTwo, this.mockFileFilterOne).accept(this.mockFile)).isFalse();
    assertThat(ComposableFileFilter.and(this.mockFileFilterTwo, this.mockFileFilterTwo).accept(this.mockFile)).isFalse();

    verify(this.mockFileFilterOne, times(3)).accept(eq(this.mockFile));
    verify(this.mockFileFilterTwo, times(3)).accept(eq(this.mockFile));
    verifyNoMoreInteractions(this.mockFileFilterOne, this.mockFileFilterTwo);
    verifyNoInteractions(this.mockFile);
  }

  @Test
  public void acceptAndWithFileFilterArray() {

    assertThat(ComposableFileFilter.and(this.mockFileFilterOne, this.mockFileFilterOne, this.mockFileFilterOne)
      .accept(this.mockFile)).isTrue();
    assertThat(ComposableFileFilter.and(this.mockFileFilterTwo, this.mockFileFilterTwo, this.mockFileFilterOne)
      .accept(this.mockFile)).isFalse();
    assertThat(ComposableFileFilter.and(this.mockFileFilterOne, this.mockFileFilterTwo, this.mockFileFilterOne)
      .accept(this.mockFile)).isFalse();
    assertThat(ComposableFileFilter.and(this.mockFileFilterTwo, this.mockFileFilterTwo, this.mockFileFilterTwo)
      .accept(this.mockFile)).isFalse();

    verify(this.mockFileFilterOne, times(4)).accept(eq(this.mockFile));
    verify(this.mockFileFilterTwo, times(3)).accept(eq(this.mockFile));
    verifyNoMoreInteractions(this.mockFileFilterOne, this.mockFileFilterTwo);
    verifyNoInteractions(this.mockFile);
  }

  @Test
  public void acceptOr() {

    assertThat(ComposableFileFilter.or(this.mockFileFilterOne, this.mockFileFilterOne).accept(this.mockFile)).isTrue();
    assertThat(ComposableFileFilter.or(this.mockFileFilterOne, this.mockFileFilterTwo).accept(this.mockFile)).isTrue();
    assertThat(ComposableFileFilter.or(this.mockFileFilterTwo, this.mockFileFilterOne).accept(this.mockFile)).isTrue();
    assertThat(ComposableFileFilter.or(this.mockFileFilterTwo, this.mockFileFilterTwo).accept(this.mockFile)).isFalse();

    verify(this.mockFileFilterOne, times(3)).accept(eq(this.mockFile));
    verify(this.mockFileFilterTwo, times(3)).accept(eq(this.mockFile));
    verifyNoMoreInteractions(this.mockFileFilterOne, this.mockFileFilterTwo);
    verifyNoInteractions(this.mockFile);
  }

  @Test
  public void acceptOrWithFileFilerArray() {

    assertThat(ComposableFileFilter.or(this.mockFileFilterTwo, this.mockFileFilterTwo, this.mockFileFilterTwo)
      .accept(this.mockFile)).isFalse();
    assertThat(ComposableFileFilter.or(this.mockFileFilterOne, this.mockFileFilterTwo, this.mockFileFilterTwo)
      .accept(this.mockFile)).isTrue();
    assertThat(ComposableFileFilter.or(this.mockFileFilterTwo, this.mockFileFilterOne, this.mockFileFilterTwo)
      .accept(this.mockFile)).isTrue();
    assertThat(ComposableFileFilter.or(this.mockFileFilterOne, this.mockFileFilterOne, this.mockFileFilterTwo)
      .accept(this.mockFile)).isTrue();
    assertThat(ComposableFileFilter.or(this.mockFileFilterOne, this.mockFileFilterOne, this.mockFileFilterOne)
      .accept(this.mockFile)).isTrue();

    verify(this.mockFileFilterOne, times(4)).accept(eq(this.mockFile));
    verify(this.mockFileFilterTwo, times(4)).accept(eq(this.mockFile));
    verifyNoMoreInteractions(this.mockFileFilterOne, this.mockFileFilterTwo);
    verifyNoInteractions(this.mockFile);
  }

  @Test
  public void acceptXor() {

    assertThat(ComposableFileFilter.xor(this.mockFileFilterOne, this.mockFileFilterOne).accept(this.mockFile)).isFalse();
    assertThat(ComposableFileFilter.xor(this.mockFileFilterOne, this.mockFileFilterTwo).accept(this.mockFile)).isTrue();
    assertThat(ComposableFileFilter.xor(this.mockFileFilterTwo, this.mockFileFilterOne).accept(this.mockFile)).isTrue();
    assertThat(ComposableFileFilter.xor(this.mockFileFilterTwo, this.mockFileFilterTwo).accept(this.mockFile)).isFalse();

    verify(this.mockFileFilterOne, times(4)).accept(eq(this.mockFile));
    verify(this.mockFileFilterTwo, times(4)).accept(eq(this.mockFile));
    verifyNoMoreInteractions(this.mockFileFilterOne, this.mockFileFilterTwo);
    verifyNoInteractions(this.mockFile);
  }

  @Test
  public void compositionAndAccept() {

    FileFilter fileFilter = ComposableFileFilter.xor(
        ComposableFileFilter.or(
ComposableFileFilter.and(null, this.mockFileFilterTwo, this.mockFileFilterOne, null, null),
          null, this.mockFileFilterOne),
        ComposableFileFilter.xor(this.mockFileFilterTwo, null)
    );

    assertThat(fileFilter).isInstanceOf(ComposableFileFilter.class);

    ComposableFileFilter compositeFileFilter = (ComposableFileFilter) fileFilter;

    assertThat(compositeFileFilter.getLeftOperand()).isInstanceOf(ComposableFileFilter.class);
    assertThat(compositeFileFilter.getOperator()).isEqualTo(LogicalOperator.XOR);
    assertThat(compositeFileFilter.getRightOperand()).isEqualTo(this.mockFileFilterTwo);

    ComposableFileFilter xorLeftOperandFileFilter = (ComposableFileFilter) compositeFileFilter.getLeftOperand();

    assertThat(xorLeftOperandFileFilter.getLeftOperand()).isInstanceOf(ComposableFileFilter.class);
    assertThat(xorLeftOperandFileFilter.getOperator()).isEqualTo(LogicalOperator.OR);
    assertThat(xorLeftOperandFileFilter.getRightOperand()).isEqualTo(this.mockFileFilterOne);

    ComposableFileFilter orLeftOperandFileFilter = (ComposableFileFilter) xorLeftOperandFileFilter.getLeftOperand();

    assertThat(orLeftOperandFileFilter.getLeftOperand()).isEqualTo(this.mockFileFilterTwo);
    assertThat(orLeftOperandFileFilter.getOperator()).isEqualTo(LogicalOperator.AND);
    assertThat(orLeftOperandFileFilter.getRightOperand()).isEqualTo(this.mockFileFilterOne);

    // Run FileFilter...
    assertThat(fileFilter.accept(this.mockFile)).isTrue();

    verify(this.mockFileFilterOne, times(1)).accept(eq(this.mockFile));
    verify(this.mockFileFilterTwo, times(2)).accept(eq(this.mockFile));
    verifyNoMoreInteractions(this.mockFileFilterOne, this.mockFileFilterTwo);
    verifyNoInteractions(this.mockFile);
  }
}
