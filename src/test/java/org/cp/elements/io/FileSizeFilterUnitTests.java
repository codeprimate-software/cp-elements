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
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.io.File;
import java.io.FileFilter;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.annotation.NotNull;

/**
 * Unit Tests for {@link FileSizeFilter}.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.io.FileSizeFilter
 * @since 1.0.0
 */
@SuppressWarnings("all")
public class FileSizeFilterUnitTests {

  private @NotNull File mockFile(@NotNull String name) {
    return mock(File.class, name);
  }

  private void acceptSizeTest(@NotNull FileFilter fileFilter,
      boolean smaller, boolean onMin, boolean between, boolean onMax, boolean bigger) {

    File fileSmaller = mockFile("fileSmaller");
    File fileOnMin = mockFile("fileOnMin");
    File fileBetween = mockFile("fileBetween");
    File fileOnMax = mockFile("fileOnMax");
    File fileBigger = mockFile("fileBigger");

    doReturn(1024L).when(fileSmaller).length();
    doReturn(2048L).when(fileOnMin).length();
    doReturn(3072L).when(fileBetween).length();
    doReturn(4096L).when(fileOnMax).length();
    doReturn(8192L).when(fileBigger).length();

    assertThat(fileFilter.accept(fileSmaller)).isEqualTo(smaller);
    assertThat(fileFilter.accept(fileOnMin)).isEqualTo(onMin);
    assertThat(fileFilter.accept(fileBetween)).isEqualTo(between);
    assertThat(fileFilter.accept(fileOnMax)).isEqualTo(onMax);
    assertThat(fileFilter.accept(fileBigger)).isEqualTo(bigger);

    verify(fileSmaller, atLeast(1)).length();
    verify(fileOnMin, atLeast(1)).length();
    verify(fileBetween, atLeast(1)).length();
    verify(fileOnMax, atLeast(1)).length();
    verify(fileBigger, atLeast(1)).length();
    verifyNoMoreInteractions(fileSmaller, fileOnMin, fileBetween, fileOnMax, fileBigger);
  }

  @Test
  public void createWithNullRelationalOperator() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> FileSizeFilter.create(null))
      .withMessage("RelationalOperator is required")
      .withNoCause();
  }

  @Test
  public void acceptBetween() {
    acceptSizeTest(FileSizeFilter.between(2048L, 4096L),
      false, true, true, true, false);
  }

  @Test
  public void rejectNotBetween() {
    acceptSizeTest(InverseFileFilter.invert(FileSizeFilter.between(2048l, 4096l)),
      true, false, false, false, true);
  }

  @Test
  public void acceptEqualTo() {
    acceptSizeTest(FileSizeFilter.equalTo(3072L),
      false, false, true, false, false);
  }

  @Test
  public void rejectNotEqualTo() {
    acceptSizeTest(InverseFileFilter.invert(FileSizeFilter.equalTo(3072L)),
      true, true, false, true, true);
  }

  @Test
  public void acceptGreaterThan() {
    acceptSizeTest(FileSizeFilter.greaterThan(3072L),
      false, false, false, true, true);
  }

  @Test
  public void acceptGreaterThanEqualTo() {
    acceptSizeTest(ComposableFileFilter.or(FileSizeFilter.greaterThan(3072L), FileSizeFilter.equalTo(3072L)),
      false, false, true, true, true);
  }

  @Test
  public void rejectGreaterThan() {
    acceptSizeTest(InverseFileFilter.invert(FileSizeFilter.greaterThan(3072L)),
      true, true, true, false, false);
  }

  @Test
  public void acceptLessThan() {
    acceptSizeTest(FileSizeFilter.lessThan(3072L),
      true, true, false, false, false);
  }

  @Test
  public void acceptLessThanEqualTo() {
    acceptSizeTest(ComposableFileFilter.or(FileSizeFilter.lessThan(3072L), FileSizeFilter.equalTo(3072L)),
      true, true, true, false, false);
  }

  @Test
  public void rejectLessThan() {
    acceptSizeTest(new InverseFileFilter(FileSizeFilter.lessThan(3072L)),
      false, false, true, true, true);
  }

  @Test
  public void acceptOutside() {
    acceptSizeTest(FileSizeFilter.outside(2048L, 4096L),
      true, false, false, false, true);
  }

  @Test
  public void rejectOutside() {
    acceptSizeTest(InverseFileFilter.invert(FileSizeFilter.outside(2048L, 4096L)),
      false, true, true, true, false);
  }
}
