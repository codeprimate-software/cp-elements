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
import static org.cp.elements.lang.CheckedExceptionsFactory.newCloneNotSupportedException;
import static org.cp.elements.lang.CheckedExceptionsFactory.newIOException;
import static org.cp.elements.lang.CheckedExceptionsFactory.newTimeoutException;

import java.io.IOException;
import java.util.concurrent.TimeoutException;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit tests for {@link CheckedExceptionsFactory}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.cp.elements.lang.CheckedExceptionsFactory
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class CheckedExceptionsFactoryTests {

  @Mock
  private Throwable mockCause;

  protected void assertThrowable(Throwable throwable, Class<? extends Throwable> type, String message) {
    assertThrowable(throwable, type, message, null);
  }

  protected void assertThrowable(Throwable throwable, Class<? extends Throwable> type,
      String message, Throwable cause) {

    assertThat(throwable).isNotNull();
    assertThat(throwable).isInstanceOf(type);
    assertThat(throwable).hasCause(cause);
    assertThat(throwable).hasMessage(message);
  }

  @Test
  public void newCloneNotSupportedExceptionWithMessage() {
    assertThrowable(newCloneNotSupportedException("test"), CloneNotSupportedException.class, "test");
  }

  @Test
  public void newCloneNotSupportedExceptionWithFormattedMessageAndCause() {
    assertThrowable(newCloneNotSupportedException(mockCause, "{0} is a %2$s", "This", "test"),
      CloneNotSupportedException.class, "This is a test", mockCause);
  }

  @Test
  public void newIOExceptionWithMessage() {
    assertThrowable(newIOException("test"), IOException.class, "test");
  }

  @Test
  public void newIOExceptionWithFormattedMessageAndCause() {
    assertThrowable(newIOException(mockCause, "{0} is a %2$s", "This", "test"),
      IOException.class, "This is a test", mockCause);
  }

  @Test
  public void newTimeoutExceptionWithMessage() {
    assertThrowable(newTimeoutException("test"), TimeoutException.class, "test");
  }

  @Test
  public void newTimeoutExceptionWithFormattedMessageAndCause() {
    assertThrowable(newTimeoutException(mockCause, "{0} is a %2$s", "This", "test"),
      TimeoutException.class, "This is a test", mockCause);
  }
}
