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

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.FileFilter;

import org.junit.Test;

/**
 * Test suite of test cases testing the contract and functionality of the {@link FileSizeFilter} class.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.io.FileSizeFilter
 * @since 1.0.0
 */
@SuppressWarnings("all")
public class FileSizeFilterTests {

  protected File mockFile(String name) {
    return mock(File.class, name);
  }

  protected void acceptSizeTest(FileFilter fileFilter,
      boolean smaller, boolean onMin, boolean between, boolean onMax, boolean bigger) {

    File fileSmaller = mockFile("fileSmaller");
    File fileOnMin = mockFile("fileOnMin");
    File fileBetween = mockFile("fileBetween");
    File fileOnMax = mockFile("fileOnMax");
    File fileBigger = mockFile("fileBigger");

    when(fileSmaller.length()).thenReturn(1024l);
    when(fileOnMin.length()).thenReturn(2048l);
    when(fileBetween.length()).thenReturn(3072l);
    when(fileOnMax.length()).thenReturn(4096l);
    when(fileBigger.length()).thenReturn(8192l);

    assertThat(fileFilter.accept(fileSmaller), is(smaller));
    assertThat(fileFilter.accept(fileOnMin), is(onMin));
    assertThat(fileFilter.accept(fileBetween), is(between));
    assertThat(fileFilter.accept(fileOnMax), is(onMax));
    assertThat(fileFilter.accept(fileBigger), is(bigger));

    verify(fileSmaller, atLeast(1)).length();
    verify(fileOnMin, atLeast(1)).length();
    verify(fileBetween, atLeast(1)).length();
    verify(fileOnMax, atLeast(1)).length();
    verify(fileBigger, atLeast(1)).length();
  }

  @Test
  public void acceptBetween() {
    acceptSizeTest(FileSizeFilter.between(2048l, 4096l), false, true, true, true, false);
  }

  @Test
  public void acceptNotBetween() {
    acceptSizeTest(new InverseFileFilter(FileSizeFilter.between(2048l, 4096l)), true, false, false, false, true);
  }

  @Test
  public void acceptEqualTo() {
    acceptSizeTest(FileSizeFilter.equalTo(3072l), false, false, true, false, false);
  }

  @Test
  public void acceptNotEqualTo() {
    acceptSizeTest(new InverseFileFilter(FileSizeFilter.equalTo(3072l)), true, true, false, true, true);
  }

  @Test
  public void acceptGreaterThan() {
    acceptSizeTest(FileSizeFilter.greaterThan(3072l), false, false, false, true, true);
  }

  @Test
  public void acceptGreaterThanEqualTo() {
    acceptSizeTest(ComposableFileFilter.or(FileSizeFilter.greaterThan(3072l), FileSizeFilter.equalTo(3072l)),
      false, false, true, true, true);
  }

  @Test
  public void acceptNotGreaterThan() {
    acceptSizeTest(new InverseFileFilter(FileSizeFilter.greaterThan(3072l)), true, true, true, false, false);
  }

  @Test
  public void acceptLessThan() {
    acceptSizeTest(FileSizeFilter.lessThan(3072l), true, true, false, false, false);
  }

  @Test
  public void acceptLessThanEqualTo() {
    acceptSizeTest(ComposableFileFilter.or(FileSizeFilter.lessThan(3072l), FileSizeFilter.equalTo(3072l)),
      true, true, true, false, false);
  }

  @Test
  public void acceptNotLessThan() {
    acceptSizeTest(new InverseFileFilter(FileSizeFilter.lessThan(3072l)), false, false, true, true, true);
  }

  @Test
  public void acceptOutside() {
    acceptSizeTest(FileSizeFilter.outside(2048l, 4096l), true, true, false, true, true);
  }

  @Test
  public void acceptNotOutside() {
    acceptSizeTest(new InverseFileFilter(FileSizeFilter.outside(2048l, 4096l)), false, false, true, false, false);
  }
}
