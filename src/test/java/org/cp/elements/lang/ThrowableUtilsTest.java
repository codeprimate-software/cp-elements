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

package org.cp.elements.lang;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.lang.reflect.InvocationTargetException;

import org.cp.elements.util.ApplicationException;
import org.cp.elements.util.SystemException;
import org.junit.Test;

/**
 * The ExceptionUtilsTest class is a test suite of test cases testing the contract and functionality
 * of the ExceptionUtils class.
 *
 * @author John J. Blum
 * @see java.lang.Error
 * @see java.lang.Throwable
 * @see ThrowableUtils
 * @see org.junit.Assert
 * @see org.junit.Test
 */
public class ThrowableUtilsTest {

  protected String toString(final Throwable t) {
    final StringBuilder buffer = new StringBuilder(t.getClass().getName());

    if (t.getMessage() != null) {
      buffer.append(":").append(" ").append(t.getMessage());
      buffer.append(System.getProperty("line.separator"));
    }

    for (final StackTraceElement element : t.getStackTrace()) {
      buffer.append("\tat ");
      buffer.append(element);
      buffer.append(System.getProperty("line.separator"));
    }

    return buffer.toString();
  }

  @Test
  public void testGetCauseOfInvocationTargetException() {
    final IllegalArgumentException expectedException = new IllegalArgumentException("Illegal Argument!");
    assertNull(ThrowableUtils.getCauseOfInvocationTargetException(new InvocationTargetException(null)));
    assertEquals(expectedException,
      ThrowableUtils.getCauseOfInvocationTargetException(new InvocationTargetException(expectedException)));
  }

  @Test
  public void testGetCauseOfInvocationTargetExceptionWithNull() {
    assertNull(ThrowableUtils.getCauseOfInvocationTargetException(null));
  }

  @Test
  public void testGetCauseOfInvocationTargetExceptionWithRuntimeException() {
    final RuntimeException expectedException = new RuntimeException("test");
    assertEquals(expectedException, ThrowableUtils.getCauseOfInvocationTargetException(expectedException));
  }

  @Test
  public void testGetMessage() {
    assertEquals("test", ThrowableUtils.getMessage(new RuntimeException("test")));
  }

  @Test
  public void testGetMessageWithNull() {
    assertNull(ThrowableUtils.getMessage(null));
  }

  @Test
  public void testGetRootCause() {
    final IllegalArgumentException expectedException = new IllegalArgumentException("Illegal Argument!");
    assertEquals(expectedException, ThrowableUtils.getRootCause(new SystemException(expectedException)));
  }

  @Test
  public void testGetDeeplyRootedCause() {
    final NullPointerException expectedException = new NullPointerException("Null Pointer!");
    assertEquals(expectedException, ThrowableUtils.getRootCause(new RuntimeException(new SystemException(
      new IllegalArgumentException(expectedException)))));
  }

  @Test
  public void testGetRootCauseWithNull() {
    assertNull(ThrowableUtils.getRootCause(null));
  }

  @Test
  public void testGetRootCauseWithNullCause() {
    final ApplicationException expectedException = new ApplicationException("No Cause!");
    assertEquals(expectedException, ThrowableUtils.getRootCause(expectedException));
  }

  @Test
  public void testGetStackTrace() {
    final RuntimeException e = new RuntimeException("test");
    assertEquals(toString(e), ThrowableUtils.getStackTrace(e));
  }

  @Test
  public void testGetStackTraceWithNull() {
    assertNull(ThrowableUtils.getStackTrace(null));
  }

}
