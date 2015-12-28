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
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.FileFilter;

import org.junit.Test;

/**
 * The FileSizeFilterTest class is a test suite of test cases testing the contract and functionality
 * of the FileSizeFilter class.
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
public class FileSizeFilterTest {

  @Test
  public void testAcceptBetween() {
    FileSizeFilter fileFilter = FileSizeFilter.between(2048l, 4096l);

    File fileBetween = mock(File.class, "fileBetween");
    File fileBigger = mock(File.class, "fileBigger");
    File fileOnMax = mock(File.class, "fileOnMax");
    File fileOnMin = mock(File.class, "fileOnMin");
    File fileSmaller = mock(File.class, "fileSmaller");

    when(fileSmaller.length()).thenReturn(1024l);
    when(fileOnMin.length()).thenReturn(2048l);
    when(fileBetween.length()).thenReturn(3072l);
    when(fileOnMax.length()).thenReturn(4096l);
    when(fileBigger.length()).thenReturn(8192l);

    assertFalse(fileFilter.accept(fileSmaller));
    assertTrue(fileFilter.accept(fileOnMin));
    assertTrue(fileFilter.accept(fileBetween));
    assertTrue(fileFilter.accept(fileOnMax));
    assertFalse(fileFilter.accept(fileBigger));

    verify(fileSmaller, times(1)).length();
    verify(fileOnMin, times(1)).length();
    verify(fileBetween, times(1)).length();
    verify(fileOnMax, times(1)).length();
    verify(fileBigger, times(1)).length();
  }

  @Test
  public void testAcceptOutside() {
    FileFilter fileFilter = new InverseFileFilter(FileSizeFilter.between(2048l, 4096l));

    File fileBetween = mock(File.class, "fileBetween");
    File fileBigger = mock(File.class, "fileBigger");
    File fileOnMax = mock(File.class, "fileOnMax");
    File fileOnMin = mock(File.class, "fileOnMin");
    File fileSmaller = mock(File.class, "fileSmaller");

    when(fileSmaller.length()).thenReturn(1024l);
    when(fileOnMin.length()).thenReturn(2048l);
    when(fileBetween.length()).thenReturn(3072l);
    when(fileOnMax.length()).thenReturn(4096l);
    when(fileBigger.length()).thenReturn(8192l);

    assertTrue(fileFilter.accept(fileSmaller));
    assertFalse(fileFilter.accept(fileOnMin));
    assertFalse(fileFilter.accept(fileBetween));
    assertFalse(fileFilter.accept(fileOnMax));
    assertTrue(fileFilter.accept(fileBigger));

    verify(fileSmaller, times(1)).length();
    verify(fileOnMin, times(1)).length();
    verify(fileBetween, times(1)).length();
    verify(fileOnMax, times(1)).length();
    verify(fileBigger, times(1)).length();
  }

  @Test
  public void testAcceptEqualTo() {
    FileSizeFilter fileFilter = FileSizeFilter.equalTo(4096l);

    File fileBigger = mock(File.class, "fileBigger");
    File fileOn = mock(File.class, "fileOn");
    File fileSmaller = mock(File.class, "fileSmaller");

    when(fileSmaller.length()).thenReturn(2048l);
    when(fileOn.length()).thenReturn(4096l);
    when(fileBigger.length()).thenReturn(8192l);

    assertFalse(fileFilter.accept(fileSmaller));
    assertTrue(fileFilter.accept(fileOn));
    assertFalse(fileFilter.accept(fileBigger));

    verify(fileSmaller, times(1)).length();
    verify(fileOn, times(1)).length();
    verify(fileBigger, times(1)).length();
  }

  @Test
  public void testAcceptNotEqualTo() {
    FileFilter fileFilter = new InverseFileFilter(FileSizeFilter.equalTo(4096l));

    File fileBigger = mock(File.class, "fileBigger");
    File fileOn = mock(File.class, "fileOn");
    File fileSmaller = mock(File.class, "fileSmaller");

    when(fileSmaller.length()).thenReturn(2048l);
    when(fileOn.length()).thenReturn(4096l);
    when(fileBigger.length()).thenReturn(8192l);

    assertTrue(fileFilter.accept(fileSmaller));
    assertFalse(fileFilter.accept(fileOn));
    assertTrue(fileFilter.accept(fileBigger));

    verify(fileSmaller, times(1)).length();
    verify(fileOn, times(1)).length();
    verify(fileBigger, times(1)).length();
  }

  @Test
  public void testAcceptGreaterThan() {
    FileSizeFilter fileFilter = FileSizeFilter.greaterThan(4096l);

    File fileBigger = mock(File.class, "fileBigger");
    File fileOn = mock(File.class, "fileOn");
    File fileSmaller = mock(File.class, "fileSmaller");

    when(fileSmaller.length()).thenReturn(2048l);
    when(fileOn.length()).thenReturn(4096l);
    when(fileBigger.length()).thenReturn(8192l);

    assertFalse(fileFilter.accept(fileSmaller));
    assertFalse(fileFilter.accept(fileOn));
    assertTrue(fileFilter.accept(fileBigger));

    verify(fileSmaller, times(1)).length();
    verify(fileOn, times(1)).length();
    verify(fileBigger, times(1)).length();
  }

  @Test
  public void testAcceptGreaterThanEqualTo() {
    FileFilter fileFilter = ComposableFileFilter.or(FileSizeFilter.greaterThan(4096l), FileSizeFilter.equalTo(4096l));

    File fileBigger = mock(File.class, "fileBigger");
    File fileOn = mock(File.class, "fileOn");
    File fileSmaller = mock(File.class, "fileSmaller");

    when(fileSmaller.length()).thenReturn(2048l);
    when(fileOn.length()).thenReturn(4096l);
    when(fileBigger.length()).thenReturn(8192l);

    assertFalse(fileFilter.accept(fileSmaller));
    assertTrue(fileFilter.accept(fileOn));
    assertTrue(fileFilter.accept(fileBigger));

    verify(fileSmaller, times(2)).length();
    verify(fileOn, times(2)).length();
    verify(fileBigger, times(2)).length();
  }

  @Test
  public void testAcceptLessThan() {
    FileSizeFilter fileFilter = FileSizeFilter.lessThan(4096l);

    File fileBigger = mock(File.class, "fileBigger");
    File fileOn = mock(File.class, "fileOn");
    File fileSmaller = mock(File.class, "fileSmaller");

    when(fileSmaller.length()).thenReturn(2048l);
    when(fileOn.length()).thenReturn(4096l);
    when(fileBigger.length()).thenReturn(8192l);

    assertTrue(fileFilter.accept(fileSmaller));
    assertFalse(fileFilter.accept(fileOn));
    assertFalse(fileFilter.accept(fileBigger));

    verify(fileSmaller, times(1)).length();
    verify(fileOn, times(1)).length();
    verify(fileBigger, times(1)).length();
  }

  @Test
  public void testAcceptLessThanEqualTo() {
    FileFilter fileFilter = ComposableFileFilter.or(FileSizeFilter.lessThan(4096l), FileSizeFilter.equalTo(4096l));

    File fileBigger = mock(File.class, "fileBigger");
    File fileOn = mock(File.class, "fileOn");
    File fileSmaller = mock(File.class, "fileSmaller");

    when(fileSmaller.length()).thenReturn(2048l);
    when(fileOn.length()).thenReturn(4096l);
    when(fileBigger.length()).thenReturn(8192l);

    assertTrue(fileFilter.accept(fileSmaller));
    assertTrue(fileFilter.accept(fileOn));
    assertFalse(fileFilter.accept(fileBigger));

    verify(fileSmaller, times(2)).length();
    verify(fileOn, times(2)).length();
    verify(fileBigger, times(2)).length();
  }

}
