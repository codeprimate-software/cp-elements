/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.lang;

import static org.junit.Assert.*;

import java.lang.reflect.InvocationTargetException;

import org.cp.elements.util.ApplicationException;
import org.cp.elements.util.SystemException;
import org.junit.Test;

/**
 * The ExceptionUtilsTest class is a test suite of test cases testing the contract and functionality
 * of the ExceptionUtils class.
 * <p/>
 * @author John J. Blum
 * @see java.lang.Error
 * @see java.lang.Throwable
 * @see org.cp.elements.lang.ExceptionUtils
 * @see org.junit.Assert
 * @see org.junit.Test
 */
public class ExceptionUtilsTest {

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
    assertNull(ExceptionUtils.getCauseOfInvocationTargetException(new InvocationTargetException(null)));
    assertEquals(expectedException,
      ExceptionUtils.getCauseOfInvocationTargetException(new InvocationTargetException(expectedException)));
  }

  @Test
  public void testGetCauseOfInvocationTargetExceptionWithNull() {
    assertNull(ExceptionUtils.getCauseOfInvocationTargetException(null));
  }

  @Test
  public void testGetCauseOfInvocationTargetExceptionWithRuntimeException() {
    final RuntimeException expectedException = new RuntimeException("test");
    assertEquals(expectedException, ExceptionUtils.getCauseOfInvocationTargetException(expectedException));
  }

  @Test
  public void testGetMessage() {
    assertEquals("test", ExceptionUtils.getMessage(new RuntimeException("test")));
  }

  @Test
  public void testGetMessageWithNull() {
    assertNull(ExceptionUtils.getMessage(null));
  }

  @Test
  public void testGetRootCause() {
    final IllegalArgumentException expectedException = new IllegalArgumentException("Illegal Argument!");
    assertEquals(expectedException, ExceptionUtils.getRootCause(new SystemException(expectedException)));
  }

  @Test
  public void testGetDeeplyRootedCause() {
    final NullPointerException expectedException = new NullPointerException("Null Pointer!");
    assertEquals(expectedException, ExceptionUtils.getRootCause(new RuntimeException(new SystemException(
      new IllegalArgumentException(expectedException)))));
  }

  @Test
  public void testGetRootCauseWithNull() {
    assertNull(ExceptionUtils.getRootCause(null));
  }

  @Test
  public void testGetRootCauseWithNullCause() {
    final ApplicationException expectedException = new ApplicationException("No Cause!");
    assertEquals(expectedException, ExceptionUtils.getRootCause(expectedException));
  }

  @Test
  public void testGetStackTrace() {
    final RuntimeException e = new RuntimeException("test");
    assertEquals(toString(e), ExceptionUtils.getStackTrace(e));
  }

  @Test
  public void testGetStackTraceWithNull() {
    assertNull(ExceptionUtils.getStackTrace(null));
  }

}
