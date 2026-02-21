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
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.cp.elements.lang.ThrowableAssertions.assertThatThrowableOfType;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.function.Function;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.ThrowableOperation;

/**
 * Unit Tests for {@link ThrowingSupplier}.
 *
 * @author John Blum
 * @see ThrowingSupplier
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @since 2.0.0
 */
class ThrowingSupplierUnitTests {

  @Test
  void safeSupplierReturnsSupplier() {

    ThrowingSupplier<?> mockSupplier = mock(ThrowingSupplier.class);

    assertThat(ThrowingSupplier.safeSupplier(mockSupplier)).isSameAs(mockSupplier);

    verifyNoInteractions(mockSupplier);
  }

  @Test
  void safeSupplierWithNullSupplier() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ThrowingSupplier.safeSupplier(null))
      .withMessage("Supplier is required")
      .withNoCause();
  }

  @Test
  @SuppressWarnings("unchecked")
  void getSafelyThrowingException() throws Exception {

    Function<Exception, Object> mockExceptionHandler = mock(Function.class);
    Exception exception = new Exception("TEST");
    ThrowingSupplier<Object> mockSupplier = mock(ThrowingSupplier.class);

    doCallRealMethod().when(mockSupplier).get();
    doThrow(exception).when(mockSupplier).getThrowingException();

    doAnswer(invocation -> {

      Exception theException = invocation.getArgument(0);

      assertThat(theException).isInstanceOf(IllegalStateException.class)
        .extracting(Exception::getCause)
        .isEqualTo(exception);

      return "X";
    }).when(mockExceptionHandler).apply(any(Exception.class));

    assertThat(ThrowingSupplier.getSafely(mockSupplier, mockExceptionHandler)).isEqualTo("X");

    verify(mockSupplier, times(1)).get();
    verify(mockSupplier, times(1)).getThrowingException();
    verify(mockExceptionHandler, times(1)).apply(isA(IllegalStateException.class));
    verifyNoMoreInteractions(mockSupplier, mockExceptionHandler);
  }

  @Test
  @SuppressWarnings("unchecked")
  void getSafelyWithoutThrowingException() throws Exception {

    Function<Exception, Object> mockExceptionHandler = mock(Function.class);
    ThrowingSupplier<Object> mockSupplier = mock(ThrowingSupplier.class);

    doCallRealMethod().when(mockSupplier).get();
    doReturn("X").when(mockSupplier).getThrowingException();

    assertThat(ThrowingSupplier.getSafely(mockSupplier, mockExceptionHandler)).isEqualTo("X");

    verify(mockSupplier, times(1)).get();
    verify(mockSupplier, times(1)).getThrowingException();
    verifyNoMoreInteractions(mockSupplier);
    verifyNoMoreInteractions(mockExceptionHandler);
  }

  @Test
  @SuppressWarnings("unchecked")
  void getCallsGetThrowingException() throws Exception {

    ThrowingSupplier<Object> mockThrowableConsumer = mock(ThrowingSupplier.class);

    doCallRealMethod().when(mockThrowableConsumer).get();
    doReturn("test").when(mockThrowableConsumer).getThrowingException();

    assertThat(mockThrowableConsumer.get()).isEqualTo("test");

    verify(mockThrowableConsumer, times(1)).get();
    verify(mockThrowableConsumer, times(1)).getThrowingException();
    verifyNoMoreInteractions(mockThrowableConsumer);
  }

  @Test
  @SuppressWarnings("unchecked")
  void getThrowsException() throws Exception {

    ThrowingSupplier<Object> mockThrowableConsumer = mock(ThrowingSupplier.class);

    doCallRealMethod().when(mockThrowableConsumer).get();
    doThrow(new Exception("TEST")).when(mockThrowableConsumer).getThrowingException();

    assertThatThrowableOfType(IllegalStateException.class)
      .isThrownBy(ThrowableOperation.fromSupplier(mockThrowableConsumer))
      .havingMessage("Failed to get supplied value")
      .causedBy(Exception.class)
      .havingMessage("TEST")
      .withNoCause();

    verify(mockThrowableConsumer, times(1)).get();
    verify(mockThrowableConsumer, times(1)).getThrowingException();
    verifyNoMoreInteractions(mockThrowableConsumer);
  }
}
