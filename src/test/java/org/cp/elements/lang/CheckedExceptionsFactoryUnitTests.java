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
import static org.assertj.core.api.Assertions.assertThatNullPointerException;
import static org.cp.elements.lang.CheckedExceptionsFactory.UNKNOWN_REASON_MESSAGE;
import static org.cp.elements.lang.CheckedExceptionsFactory.newCloneNotSupportedException;
import static org.cp.elements.lang.CheckedExceptionsFactory.newIOException;
import static org.cp.elements.lang.CheckedExceptionsFactory.newTimeoutException;

import java.io.IOException;
import java.util.concurrent.TimeoutException;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

/**
 * Unit Tests for {@link CheckedExceptionsFactory}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.CheckedExceptionsFactory
 * @since 1.0.0
 */
@ExtendWith(MockitoExtension.class)
public class CheckedExceptionsFactoryUnitTests {

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
  public void newCloneNotSupportedExceptionWithCause() {
    assertThrowable(newCloneNotSupportedException(this.mockCause), CloneNotSupportedException.class,
      UNKNOWN_REASON_MESSAGE, this.mockCause);
  }

  @Test
  public void newCloneNotSupportedExceptionWithMessage() {
    assertThrowable(newCloneNotSupportedException("%s is a {1}", "This", "test"), CloneNotSupportedException.class,
      "This is a test");
  }

  @Test
  public void newCloneNotSupportedExceptionWithMessageAndCause() {
    assertThrowable(newCloneNotSupportedException(this.mockCause, "test"), CloneNotSupportedException.class,
      "test", this.mockCause);
  }

  @Test
  @SuppressWarnings("all")
  public void newCloneNotSuppoertedExceptionWithNullMessage() {

    assertThatNullPointerException()
      .isThrownBy(() -> newCloneNotSupportedException(null, ObjectUtils.EMPTY_OBJECT_ARRAY))
      .withNoCause();
  }

  @Test
  public void newIOExceptionWithCause() {
    assertThrowable(newIOException(this.mockCause), IOException.class, UNKNOWN_REASON_MESSAGE, this.mockCause);
  }

  @Test
  public void newIOExceptionWithMessage() {
    assertThrowable(newIOException("%s is a {1}", "This", "test"), IOException.class, "This is a test");
  }

  @Test
  public void newIOExceptionWithMessageAndCause() {
    assertThrowable(newIOException(this.mockCause, "test"), IOException.class, "test", this.mockCause);
  }

  @Test
  @SuppressWarnings("all")
  public void newIOExceptionWithNullMessage() {

    assertThatNullPointerException()
      .isThrownBy(() -> newIOException(null, ObjectUtils.EMPTY_OBJECT_ARRAY))
      .withNoCause();
  }

  @Test
  public void newTimeoutExceptionWithCause() {
    assertThrowable(newTimeoutException(this.mockCause), TimeoutException.class,
      UNKNOWN_REASON_MESSAGE, this.mockCause);
  }

  @Test
  public void newTimeoutExceptionWithMessage() {
    assertThrowable(newTimeoutException("%s is a {1}", "This", "test"), TimeoutException.class,
      "This is a test");
  }

  @Test
  public void newTimeoutExceptionWithMessageAndCause() {
    assertThrowable(newTimeoutException(this.mockCause, "test"), TimeoutException.class,
      "test", this.mockCause);
  }

  @Test
  @SuppressWarnings("all")
  public void newTimeoutExceptionWithNullMessage() {

    assertThatNullPointerException()
      .isThrownBy(() -> newTimeoutException(null, ObjectUtils.EMPTY_OBJECT_ARRAY))
      .withNoCause();
  }
}
