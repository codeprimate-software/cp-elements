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
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.FileFilter;

import org.cp.elements.lang.LogicalOperator;
import org.junit.Test;

/**
 * The ComposableFileFilterTest class is a test suite of test case testing the contract and functionality
 * of the ComposableFileFilter class.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.io.ComposableFileFilter
 * @since 1.0.0
 */
public class ComposableFileFilterTest {

  @Test
  public void testComposeAnd() {
    FileFilter leftOperand = mock(FileFilter.class, "testComposeAnd.leftOperand");
    FileFilter rightOperand = mock(FileFilter.class, "testComposeAnd.rightOperand");

    assertNull(ComposableFileFilter.and(null, null));
    assertSame(leftOperand, ComposableFileFilter.and(leftOperand, null));
    assertSame(rightOperand, ComposableFileFilter.and(null, rightOperand));

    FileFilter fileFilter = ComposableFileFilter.and(leftOperand, rightOperand);

    assertTrue(fileFilter instanceof ComposableFileFilter);
    assertEquals(leftOperand, ((ComposableFileFilter) fileFilter).getLeftOperand());
    assertEquals(LogicalOperator.AND, ((ComposableFileFilter) fileFilter).getOperator());
    assertEquals(rightOperand, ((ComposableFileFilter) fileFilter).getRightOperand());
  }

  @Test
  public void testComposeOr() {
    FileFilter leftOperand = mock(FileFilter.class, "testComposeOr.leftOperand");
    FileFilter rightOperand = mock(FileFilter.class, "testComposeOr.rightOperand");

    assertNull(ComposableFileFilter.or(null, null));
    assertSame(leftOperand, ComposableFileFilter.or(leftOperand, null));
    assertSame(rightOperand, ComposableFileFilter.or(null, rightOperand));

    FileFilter fileFilter = ComposableFileFilter.or(leftOperand, rightOperand);

    assertTrue(fileFilter instanceof ComposableFileFilter);
    assertEquals(leftOperand, ((ComposableFileFilter) fileFilter).getLeftOperand());
    assertEquals(LogicalOperator.OR, ((ComposableFileFilter) fileFilter).getOperator());
    assertEquals(rightOperand, ((ComposableFileFilter) fileFilter).getRightOperand());
  }

  @Test
  public void testComposeXor() {
    FileFilter leftOperand = mock(FileFilter.class, "testComposeXor.leftOperand");
    FileFilter rightOperand = mock(FileFilter.class, "testComposeXor.rightOperand");

    assertNull(ComposableFileFilter.xor(null, null));
    assertSame(leftOperand, ComposableFileFilter.xor(leftOperand, null));
    assertSame(rightOperand, ComposableFileFilter.xor(null, rightOperand));

    FileFilter fileFilter = ComposableFileFilter.xor(leftOperand, rightOperand);

    assertTrue(fileFilter instanceof ComposableFileFilter);
    assertEquals(leftOperand, ((ComposableFileFilter) fileFilter).getLeftOperand());
    assertEquals(LogicalOperator.XOR, ((ComposableFileFilter) fileFilter).getOperator());
    assertEquals(rightOperand, ((ComposableFileFilter) fileFilter).getRightOperand());
  }

  @Test
  public void testAcceptAnd() {
    File mockFile = mock(File.class);

    FileFilter acceptOperand = mock(FileFilter.class, "acceptOperand");
    FileFilter rejectOperand = mock(FileFilter.class, "rejectOperand");

    when(acceptOperand.accept(any(File.class))).thenReturn(true);
    when(rejectOperand.accept(any(File.class))).thenReturn(false);

    assertTrue(ComposableFileFilter.and(acceptOperand, acceptOperand).accept(mockFile));
    assertFalse(ComposableFileFilter.and(acceptOperand, rejectOperand).accept(mockFile));
    assertFalse(ComposableFileFilter.and(rejectOperand, acceptOperand).accept(mockFile));
    assertFalse(ComposableFileFilter.and(rejectOperand, rejectOperand).accept(mockFile));

    verify(acceptOperand, times(4)).accept(any(File.class));
    verify(rejectOperand, times(4)).accept(any(File.class));
  }

  @Test
  public void testAcceptOr() {
    File mockFile = mock(File.class);

    FileFilter acceptOperand = mock(FileFilter.class, "acceptOperand");
    FileFilter rejectOperand = mock(FileFilter.class, "rejectOperand");

    when(acceptOperand.accept(any(File.class))).thenReturn(true);
    when(rejectOperand.accept(any(File.class))).thenReturn(false);

    assertTrue(ComposableFileFilter.or(acceptOperand, acceptOperand).accept(mockFile));
    assertTrue(ComposableFileFilter.or(acceptOperand, rejectOperand).accept(mockFile));
    assertTrue(ComposableFileFilter.or(rejectOperand, acceptOperand).accept(mockFile));
    assertFalse(ComposableFileFilter.or(rejectOperand, rejectOperand).accept(mockFile));

    verify(acceptOperand, times(4)).accept(any(File.class));
    verify(rejectOperand, times(4)).accept(any(File.class));
  }

  @Test
  public void testAcceptXor() {
    File mockFile = mock(File.class);

    FileFilter acceptOperand = mock(FileFilter.class, "acceptOperand");
    FileFilter rejectOperand = mock(FileFilter.class, "rejectOperand");

    when(acceptOperand.accept(any(File.class))).thenReturn(true);
    when(rejectOperand.accept(any(File.class))).thenReturn(false);

    assertFalse(ComposableFileFilter.xor(acceptOperand, acceptOperand).accept(mockFile));
    assertTrue(ComposableFileFilter.xor(acceptOperand, rejectOperand).accept(mockFile));
    assertTrue(ComposableFileFilter.xor(rejectOperand, acceptOperand).accept(mockFile));
    assertFalse(ComposableFileFilter.xor(rejectOperand, rejectOperand).accept(mockFile));

    verify(acceptOperand, times(4)).accept(any(File.class));
    verify(rejectOperand, times(4)).accept(any(File.class));
  }

}
