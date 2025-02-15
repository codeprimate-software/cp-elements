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
import static org.cp.elements.lang.ThrowableAssertions.assertThatThrowableOfType;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link ThrowableFunction}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.function.ThrowableFunction
 * @since 2.0.0
 */
class ThrowableFunctionUnitTests {

  @Test
  @SuppressWarnings("unchecked")
  void throwableFunctionReturnsValue() throws Throwable {

    ThrowableFunction<Object, Object> mockFunction = mock(ThrowableFunction.class);

    doCallRealMethod().when(mockFunction).apply(any());
    doAnswer(invocation -> invocation.getArgument(0)).when(mockFunction).applyThrowingException(any());

    assertThat(mockFunction.apply("test")).isEqualTo("test");

    verify(mockFunction, times(1)).apply(eq("test"));
    verify(mockFunction, times(1)).applyThrowingException(eq("test"));
    verifyNoMoreInteractions(mockFunction);
  }

  @Test
  @SuppressWarnings("unchecked")
  void throwableFunctionThrowsException() throws Throwable {

    ThrowableFunction<Object, Object> mockFunction = mock(ThrowableFunction.class);

    doCallRealMethod().when(mockFunction).apply(any());
    doThrow(new Exception("ERROR")).when(mockFunction).applyThrowingException(any());

    assertThatThrowableOfType(FunctionExecutionException.class)
      .isThrownBy(args -> mockFunction.apply("test"))
      .havingMessage("Failed to execute Function [%s]", mockFunction.getClass().getSimpleName())
      .causedBy(Exception.class)
      .havingMessage("ERROR")
      .withNoCause();

    verify(mockFunction, times(1)).apply(eq("test"));
    verify(mockFunction, times(1)).applyThrowingException(eq("test"));
    verifyNoMoreInteractions(mockFunction);
  }
}
