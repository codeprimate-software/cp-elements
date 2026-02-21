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
package org.cp.elements.function;

import static org.assertj.core.api.Assertions.assertThat;
import static org.cp.elements.lang.ThrowableAssertions.assertThatIllegalArgumentException;
import static org.cp.elements.lang.ThrowableAssertions.assertThatThrowableOfType;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.function.Consumer;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.ThrowableOperation;

/**
 * Unit Tests for {@link ThrowingConsumer}.
 *
 * @author John Blum
 * @see ThrowingConsumer
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @since 2.0.0
 */
class ThrowingConsumerUnitTests {

  @Test
  @SuppressWarnings("unchecked")
  void safeConsumerThrowsException() throws Exception {

    Consumer<Exception> mockExceptionHandler = mock(Consumer.class);
    IllegalArgumentException exception = new IllegalArgumentException("TEST");
    ThrowingConsumer<Object> mockConsumer = mock(ThrowingConsumer.class);

    doThrow(exception).when(mockConsumer).acceptThrowingException(any());

    doAnswer(invocation -> {

      Exception theException = invocation.getArgument(0);

      assertThat(theException).isInstanceOf(IllegalArgumentException.class)
        .hasMessage("TEST")
        .hasNoCause();

      return null;

    }).when(mockExceptionHandler).accept(any(Exception.class));

    Consumer<Object> consumer = ThrowingConsumer.safeConsumer(mockConsumer, mockExceptionHandler);

    assertThat(consumer).isNotNull();

    consumer.accept("MOCK");

    verify(mockConsumer, never()).accept(any());
    verify(mockConsumer, times(1)).acceptThrowingException("MOCK");
    verify(mockExceptionHandler, times(1)).accept(isA(IllegalArgumentException.class));
    verifyNoMoreInteractions(mockConsumer, mockExceptionHandler);
  }

  @Test
  @SuppressWarnings("unchecked")
  void safeConsumerDoesNotThrowException() throws Exception {

    Consumer<Exception> mockExceptionHandler = mock(Consumer.class);
    ThrowingConsumer<Object> mockConsumer = mock(ThrowingConsumer.class);

    doNothing().when(mockConsumer).acceptThrowingException(any());

    Consumer<Object> consumer = ThrowingConsumer.safeConsumer(mockConsumer, mockExceptionHandler);

    assertThat(consumer).isNotNull();

    consumer.accept("MOCK");

    verify(mockConsumer, never()).accept(any());
    verify(mockConsumer, times(1)).acceptThrowingException("MOCK");
    verifyNoMoreInteractions(mockConsumer);
    verifyNoInteractions(mockExceptionHandler);
  }

  @Test
  @SuppressWarnings("unchecked")
  void safeConsumerWithNullThrowableConsumer() {

    assertThatIllegalArgumentException()
      .isThrownBy(arg -> ThrowingConsumer.safeConsumer(null, mock(Consumer.class)))
      .havingMessage("Consumer is required")
      .withNoCause();
  }

  @Test
  @SuppressWarnings("unchecked")
  void safeConsumerWithNullExceptionHandler() {

    assertThatIllegalArgumentException()
      .isThrownBy(arg -> ThrowingConsumer.safeConsumer(mock(ThrowingConsumer.class), null))
      .havingMessage("Exception handler is required")
      .withNoCause();
  }

  @Test
  @SuppressWarnings("unchecked")
  void acceptCallsAcceptThrowingException() throws Exception {

    ThrowingConsumer<Object> mockThrowingConsumer = mock(ThrowingConsumer.class);

    doCallRealMethod().when(mockThrowingConsumer).accept(any());

    mockThrowingConsumer.accept("test");

    verify(mockThrowingConsumer, times(1)).accept(eq("test"));
    verify(mockThrowingConsumer, times(1)).acceptThrowingException(eq("test"));
    verifyNoMoreInteractions(mockThrowingConsumer);
  }

  @Test
  @SuppressWarnings("unchecked")
  void throwableConsumerThrowsException() throws Exception {

    ThrowingConsumer<Object> mockThrowingConsumer = mock(ThrowingConsumer.class);

    doCallRealMethod().when(mockThrowingConsumer).accept(any());
    doThrow(new Exception("TEST")).when(mockThrowingConsumer).acceptThrowingException(any());

    assertThatThrowableOfType(IllegalStateException.class)
      .isThrownBy(ThrowableOperation.fromConsumer(target -> mockThrowingConsumer.accept("test")))
      .havingMessage("Failed to consume object [test]")
      .causedBy(Exception.class)
      .havingMessage("TEST")
      .withNoCause();

    verify(mockThrowingConsumer, times(1)).accept(eq("test"));
    verify(mockThrowingConsumer, times(1)).acceptThrowingException(eq("test"));
    verifyNoMoreInteractions(mockThrowingConsumer);
  }
}
