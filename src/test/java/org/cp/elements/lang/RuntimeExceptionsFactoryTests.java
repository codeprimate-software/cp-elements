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

import static org.assertj.core.api.Assertions.assertThat;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalArgumentException;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalStateException;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newIndexOutOfBoundsException;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newNoSuchElementException;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newNullPointerException;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newRuntimeException;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newTypeNotPresentException;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newUnsupportedOperationException;

import java.util.NoSuchElementException;

import org.junit.Test;
import org.mockito.Mock;

/**
 * Unit tests for {@link RuntimeExceptionsFactory}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see RuntimeExceptionsFactory
 * @since 1.0.0
 */
public class RuntimeExceptionsFactoryTests {

  @Mock
  private Throwable mockCause;

  protected void assertThrowable(Throwable actual, Class<? extends Throwable> type,
    String message, Throwable cause) {

    assertThat(actual).isNotNull();
    assertThat(actual).isInstanceOf(type);
    assertThat(actual).hasCause(cause);
    assertThat(actual).hasMessage(message);
  }

  @Test
  public void newIllegalArgumentExceptionWithMessage() {
    assertThrowable(newIllegalArgumentException("test"), IllegalArgumentException.class, "test", null);
  }

  @Test
  public void newIllegalArgumentExceptionWithFormattedMessageAndCause() {
    assertThrowable(newIllegalArgumentException(mockCause, "%s is a {1}", "This", "test"),
      IllegalArgumentException.class, "This is a test", mockCause);
  }

  @Test
  public void newIllegalStateExceptionWithMessage() {
    assertThrowable(newIllegalStateException("test"), IllegalStateException.class, "test", null);
  }

  @Test
  public void newIllegalStateExceptionWithFormattedMessageAndCause() {
    assertThrowable(newIllegalStateException(mockCause, "%s is a {1}", "This", "test"),
      IllegalStateException.class, "This is a test", mockCause);
  }

  @Test
  public void newIndexOutOfBoundsExceptionWithMessage() {
    assertThrowable(newIndexOutOfBoundsException("test"),
      IndexOutOfBoundsException.class, "test", null);
  }

  @Test
  public void newIndexOutOfBoundsExceptionWithFormattedMessageAndCause() {
    assertThrowable(newIndexOutOfBoundsException(mockCause, "%s is a {1}", "This", "test"),
      IndexOutOfBoundsException.class, "This is a test", mockCause);
  }

  @Test
  public void newNoSuchElementExceptionWithMessage() {
    assertThrowable(newNoSuchElementException("test"), NoSuchElementException.class, "test", null);
  }

  @Test
  public void newNoSuchElementExceptionWithFormattedMessageAndCause() {
    assertThrowable(newNoSuchElementException(mockCause, "%s is a {1}", "This", "test"),
      NoSuchElementException.class, "This is a test", mockCause);
  }

  @Test
  public void newNullPointerExceptionWithMessage() {
    assertThrowable(newNullPointerException("test"), NullPointerException.class, "test", null);
  }

  @Test
  public void newNullPointerExceptionWithFormattedMessageAndCause() {
    assertThrowable(newNullPointerException(mockCause, "%s is a {1}", "This", "test"),
      NullPointerException.class, "This is a test", mockCause);
  }

  @Test
  public void newRuntimeExceptionWithMessage() {
    assertThrowable(newRuntimeException("test"), RuntimeException.class, "test", null);
  }

  @Test
  public void newRuntimeExceptionWithFormattedMessageAndCause() {
    assertThrowable(newRuntimeException(mockCause, "%s is a {1}", "This", "test"),
      RuntimeException.class, "This is a test", mockCause);
  }

  @Test
  public void newTypeNotPresentExceptionWithMessage() {
    assertThrowable(newTypeNotPresentException("java.lang.Object"),
      TypeNotPresentException.class, "Type java.lang.Object not present", null);
  }

  @Test
  public void newTypeNotPresentExceptionWithFormattedMessageAndCause() {
    assertThrowable(newTypeNotPresentException(mockCause, "%s.{1}", "java.lang", "Object"),
      TypeNotPresentException.class, "Type java.lang.Object not present", mockCause);
  }

  @Test
  public void newUnsupportedOperationExceptionWithMessage() {
    assertThrowable(newUnsupportedOperationException("test"),
      UnsupportedOperationException.class, "test", null);
  }

  @Test
  public void newUnsupportedOperationExceptionWithFormattedMessageAndCause() {
    assertThrowable(newUnsupportedOperationException(mockCause, "%s is a {1}", "This", "test"),
      UnsupportedOperationException.class, "This is a test", mockCause);
  }
}
