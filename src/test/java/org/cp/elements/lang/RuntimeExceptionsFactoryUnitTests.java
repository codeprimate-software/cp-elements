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
package org.cp.elements.lang;

import static org.assertj.core.api.Assertions.assertThat;
import static org.cp.elements.lang.CheckedExceptionsFactory.UNKNOWN_REASON_MESSAGE;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalArgumentException;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalStateException;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newIndexOutOfBoundsException;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newNoSuchElementException;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newNullPointerException;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newRuntimeException;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newTypeNotPresentException;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newUnsupportedOperationException;

import java.util.NoSuchElementException;

import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit Tests for {@link RuntimeExceptionsFactory}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.cp.elements.lang.RuntimeExceptionsFactory
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class RuntimeExceptionsFactoryUnitTests {

  @Mock
  private Throwable mockCause;

  private void assertThrowable(Throwable throwable, Class<? extends Throwable> type, String message) {
    assertThrowable(throwable, type, message, null);
  }

  private void assertThrowable(Throwable throwable, Class<? extends Throwable> type,
      String message, Throwable cause) {

    assertThat(throwable).isNotNull();
    assertThat(throwable).isInstanceOf(type);
    assertThat(throwable).hasCause(cause);
    assertThat(throwable).hasMessage(message);
  }

  @Test
  public void newIllegalArgumentExceptionWithCause() {
    assertThrowable(newIllegalArgumentException(this.mockCause), IllegalArgumentException.class,
      UNKNOWN_REASON_MESSAGE, this.mockCause);
  }

  @Test
  public void newIllegalArgumentExceptionWithMessage() {
    assertThrowable(newIllegalArgumentException("%s is a {1}", "This", "test"), IllegalArgumentException.class,
      "This is a test");
  }

  @Test
  public void newIllegalArgumentExceptionWithMessageAndCause() {
    assertThrowable(newIllegalArgumentException(this.mockCause, "test"), IllegalArgumentException.class,
      "test", this.mockCause);
  }

  @SuppressWarnings("all")
  @Test(expected = NullPointerException.class)
  public void newIllegalArgumentExceptionWithNullMessage() {
    newIllegalArgumentException(null, ObjectUtils.EMPTY_OBJECT_ARRAY);
  }

  @Test
  public void newIllegalStateExceptionWithCause() {
    assertThrowable(newIllegalStateException(this.mockCause), IllegalStateException.class,
      UNKNOWN_REASON_MESSAGE, this.mockCause);
  }

  @Test
  public void newIllegalStateExceptionWithMessage() {
    assertThrowable(newIllegalStateException("%s is a {1}", "This", "test"), IllegalStateException.class,
      "This is a test");
  }

  @Test
  public void newIllegalStateExceptionWithMessageAndCause() {
    assertThrowable(newIllegalStateException(this.mockCause, "test"), IllegalStateException.class,
      "test", this.mockCause);
  }

  @SuppressWarnings("all")
  @Test(expected = NullPointerException.class)
  public void newIllegalStateExceptionWithNullMessage() {
    newIllegalStateException(null, ObjectUtils.EMPTY_OBJECT_ARRAY);
  }

  @Test
  public void newIndexOutOfBoundsExceptionWithCause() {
    assertThrowable(newIndexOutOfBoundsException(this.mockCause), IndexOutOfBoundsException.class,
      UNKNOWN_REASON_MESSAGE, this.mockCause);
  }

  @Test
  public void newIndexOutOfBoundsExceptionWithMessage() {
    assertThrowable(newIndexOutOfBoundsException("%s is a {1}", "This", "test"), IndexOutOfBoundsException.class,
      "This is a test");
  }

  @Test
  public void newIndexOutOfBoundsExceptionWithMessageAndCause() {
    assertThrowable(newIndexOutOfBoundsException(this.mockCause, "test"), IndexOutOfBoundsException.class,
      "test", this.mockCause);
  }

  @SuppressWarnings("all")
  @Test(expected = NullPointerException.class)
  public void newIndexOutOfBoundsExceptionWithNullMessage() {
    newIndexOutOfBoundsException(null, ObjectUtils.EMPTY_OBJECT_ARRAY);
  }

  @Test
  public void newNoSuchElementExceptionWithCause() {
    assertThrowable(newNoSuchElementException(this.mockCause), NoSuchElementException.class,
      UNKNOWN_REASON_MESSAGE, this.mockCause);
  }

  @Test
  public void newNoSuchElementExceptionWithMessage() {
    assertThrowable(newNoSuchElementException("%s is a {1}", "This", "test"), NoSuchElementException.class,
      "This is a test");
  }

  @Test
  public void newNoSuchElementExceptionWithMessageAndCause() {
    assertThrowable(newNoSuchElementException(this.mockCause, "test"), NoSuchElementException.class,
      "test", this.mockCause);
  }

  @SuppressWarnings("all")
  @Test(expected = NullPointerException.class)
  public void noSuchElementExceptionWithNullMessage() {
    newNoSuchElementException(null, ObjectUtils.EMPTY_OBJECT_ARRAY);
  }

  @Test
  public void newNullPointerExceptionWithCause() {
    assertThrowable(newNullPointerException(this.mockCause), NullPointerException.class,
      UNKNOWN_REASON_MESSAGE, this.mockCause);
  }

  @Test
  public void newNullPointerExceptionWithMessage() {
    assertThrowable(newNullPointerException("%s is a {1}", "This", "test"), NullPointerException.class,
      "This is a test");
  }

  @Test
  public void newNullPointerExceptionWithMessageAndCause() {
    assertThrowable(newNullPointerException(this.mockCause, "test"), NullPointerException.class,
      "test", this.mockCause);
  }

  @SuppressWarnings("all")
  @Test(expected = NullPointerException.class)
  public void newNullPointerExceptionWithNullMessage() {
    newNullPointerException(null, ObjectUtils.EMPTY_OBJECT_ARRAY);
  }

  @Test
  public void newRuntimeExceptionWithCause() {
    assertThrowable(newRuntimeException(this.mockCause), RuntimeException.class,
      UNKNOWN_REASON_MESSAGE, this.mockCause);
  }

  @Test
  public void newRuntimeExceptionWithMessage() {
    assertThrowable(newRuntimeException("%s is a {1}", "This", "test"), RuntimeException.class,
      "This is a test");
  }

  @Test
  public void newRuntimeExceptionWithMessageAndCause() {
    assertThrowable(newRuntimeException(this.mockCause, "test"), RuntimeException.class,
      "test", this.mockCause);
  }

  @SuppressWarnings("all")
  @Test(expected = NullPointerException.class)
  public void newRuntimeExceptionWithNullMessage() {
    newRuntimeException(null, ObjectUtils.EMPTY_OBJECT_ARRAY);
  }

  @Test
  public void newTypeNotPresentExceptionWithCause() {
    assertThrowable(newTypeNotPresentException(this.mockCause), TypeNotPresentException.class,
      String.format("Type %s not present", UNKNOWN_REASON_MESSAGE), this.mockCause);
  }

  @Test
  public void newTypeNotPresentExceptionWithMessage() {
    assertThrowable(newTypeNotPresentException("java.lang.Object"), TypeNotPresentException.class,
      "Type java.lang.Object not present");
  }

  @Test
  public void newTypeNotPresentExceptionWithMessageAndCause() {
    assertThrowable(newTypeNotPresentException(this.mockCause, "%s.{1}", "java.lang", "Object"),
      TypeNotPresentException.class, "Type java.lang.Object not present", this.mockCause);
  }

  @SuppressWarnings("all")
  @Test(expected = NullPointerException.class)
  public void newTypeNotPresentExceptionWithNullMessage() {
    newTypeNotPresentException(null, ObjectUtils.EMPTY_OBJECT_ARRAY);
  }

  @Test
  public void newUnsupportedOperationExceptionWithCause() {
    assertThrowable(newUnsupportedOperationException(this.mockCause), UnsupportedOperationException.class,
      UNKNOWN_REASON_MESSAGE, this.mockCause);
  }

  @Test
  public void newUnsupportedOperationExceptionWithMessage() {
    assertThrowable(newUnsupportedOperationException("%s is a {1}", "This", "test"), UnsupportedOperationException.class,
      "This is a test");
  }

  @Test
  public void newUnsupportedOperationExceptionWithMessageAndCause() {
    assertThrowable(newUnsupportedOperationException(this.mockCause, "test"), UnsupportedOperationException.class,
      "test", this.mockCause);
  }

  @SuppressWarnings("all")
  @Test(expected = NullPointerException.class)
  public void newUnsupportedOperationExceptionWithNullMessage() {
    newUnsupportedOperationException(null, ObjectUtils.EMPTY_OBJECT_ARRAY);
  }
}
