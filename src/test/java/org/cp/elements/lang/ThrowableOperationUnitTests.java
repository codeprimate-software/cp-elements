/*
 * Copyright 2016 Author or Authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */
package org.cp.elements.lang;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import org.junit.Test;
import org.mockito.InOrder;

/**
 * Unit Tests for {@link ThrowableOperation}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.ThrowableOperation
 * @since 1.0.0
 */
@SuppressWarnings("unchecked")
public class ThrowableOperationUnitTests {

  @Test
  public void acceptInvokesRunWithArguments() throws Throwable {

    ThrowableOperation<Object> mockOperation = mock(ThrowableOperation.class);

    doCallRealMethod().when(mockOperation).accept(any());

    mockOperation.accept("mock");

    verify(mockOperation, times(1)).accept(eq("mock"));
    verify(mockOperation, times(1)).run(eq("mock"));
    verifyNoMoreInteractions(mockOperation);
  }

  @Test(expected = RuntimeException.class)
  public void acceptHandlesThrowable() throws Throwable {

    ThrowableOperation<Object> mockOperation = mock(ThrowableOperation.class);

    doCallRealMethod().when(mockOperation).accept(any());
    doThrow(new IllegalArgumentException("test")).when(mockOperation).run(any());

    try {
      mockOperation.accept("mock");
    }
    catch (RuntimeException expected) {

      assertThat(expected).hasMessage("Accept failed to operate on target [mock]");
      assertThat(expected).hasCauseInstanceOf(IllegalArgumentException.class);
      assertThat(expected.getCause()).hasMessage("test");
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockOperation, times(1)).accept(eq("mock"));
      verify(mockOperation, times(1)).run(eq("mock"));
      verifyNoMoreInteractions(mockOperation);
    }
  }

  @Test
  public void callInvokesRunWithArguments() throws Throwable {

    ThrowableOperation<Object> mockOperation = mock(ThrowableOperation.class);

    doCallRealMethod().when(mockOperation).call();
    doReturn("mock").when(mockOperation).run(any());

    assertThat(mockOperation.call()).isEqualTo("mock");

    verify(mockOperation, times(1)).call();
    verify(mockOperation, times(1)).run(any());
    verifyNoMoreInteractions(mockOperation);
  }

  @Test(expected = RuntimeException.class)
  public void callHandlesThrowable() throws Throwable {

    ThrowableOperation<Object> mockOperation = mock(ThrowableOperation.class);

    doCallRealMethod().when(mockOperation).call();
    doThrow(new IllegalStateException("test")).when(mockOperation).run(any());

    try {
      mockOperation.call();
    }
    catch (RuntimeException cause) {

      assertThat(cause).hasMessage("Call failed to complete operation");
      assertThat(cause).hasCauseInstanceOf(IllegalStateException.class);
      assertThat(cause.getCause()).hasMessage("test");
      assertThat(cause.getCause()).hasNoCause();

      throw cause;
    }
    finally {
      verify(mockOperation, times(1)).call();
      verify(mockOperation, times(1)).run(any());
      verifyNoMoreInteractions(mockOperation);
    }
  }

  @Test
  public void getInvokesRunWithArguments() throws Throwable {

    ThrowableOperation<Object> mockOperation = mock(ThrowableOperation.class);

    doCallRealMethod().when(mockOperation).get();
    doReturn("mock").when(mockOperation).run(any());

    assertThat(mockOperation.get()).isEqualTo("mock");

    verify(mockOperation, times(1)).get();
    verify(mockOperation, times(1)).run(any());
    verifyNoMoreInteractions(mockOperation);
  }

  @Test(expected = RuntimeException.class)
  public void getHandlesThrowable() throws Throwable {

    ThrowableOperation<Object> mockOperation = mock(ThrowableOperation.class);

    doCallRealMethod().when(mockOperation).get();
    doThrow(new UnsupportedOperationException("test")).when(mockOperation).run(any());

    try {
      mockOperation.get();
    }
    catch (RuntimeException cause) {

      assertThat(cause).hasMessage("Get failed to return the result of the operation");
      assertThat(cause).hasCauseInstanceOf(UnsupportedOperationException.class);
      assertThat(cause.getCause()).hasMessage("test");
      assertThat(cause.getCause()).hasNoCause();

      throw cause;
    }
    finally {
      verify(mockOperation, times(1)).get();
      verify(mockOperation, times(1)).run(any());
      verifyNoMoreInteractions(mockOperation);
    }
  }

  @Test
  public void runInvokesRunWithArguments() throws Throwable {

    ThrowableOperation<Object> mockOperation = mock(ThrowableOperation.class);

    doCallRealMethod().when(mockOperation).run();

    mockOperation.run();

    verify(mockOperation, times(1)).run();
    verify(mockOperation, times(1)).run(any());
    verifyNoMoreInteractions(mockOperation);
  }

  @Test(expected = RuntimeException.class)
  public void runHandlesThrowable() throws Throwable {

    ThrowableOperation<Object> mockOperation = mock(ThrowableOperation.class);

    doCallRealMethod().when(mockOperation).run();
    doThrow(new RuntimeException("test")).when(mockOperation).run(any());

    try {
      mockOperation.run();
    }
    catch (RuntimeException expected) {

      assertThat(expected).hasMessage("Run failed to complete operation");
      assertThat(expected).hasCauseInstanceOf(RuntimeException.class);
      assertThat(expected.getCause()).hasMessage("test");
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockOperation, times(1)).run();
      verify(mockOperation, times(1)).run(any());
      verifyNoMoreInteractions(mockOperation);
    }
  }

  @Test
  public void andThenWithNonNullThrowableOperation() throws Throwable {

    ThrowableOperation<Object> mockOperationOne = mock(ThrowableOperation.class);
    ThrowableOperation<Object> mockOperationTwo = mock(ThrowableOperation.class);

    doCallRealMethod().when(mockOperationOne).andThen(eq(mockOperationTwo));

    ThrowableOperation<Object> composedOperation = mockOperationOne.andThen(mockOperationTwo);

    doReturn("1").when(mockOperationOne).run(eq("test"));
    doReturn("2").when(mockOperationTwo).run(eq("1"));

    assertThat(composedOperation).isNotNull();
    assertThat(composedOperation.run("test")).isEqualTo("2");

    InOrder order = inOrder(mockOperationOne, mockOperationTwo);

    order.verify(mockOperationOne, times(1)).andThen(eq(mockOperationTwo));
    order.verify(mockOperationOne, times(1)).run(eq("test"));
    order.verify(mockOperationTwo, times(1)).run(eq("1"));
    verifyNoMoreInteractions(mockOperationOne, mockOperationTwo);
  }

  @Test
  public void andThenWithNull() {

    ThrowableOperation<Object> mockOperation = mock(ThrowableOperation.class);

    doCallRealMethod().when(mockOperation).andThen(any());

    assertThat(mockOperation.andThen(null)).isSameAs(mockOperation);
  }
}
