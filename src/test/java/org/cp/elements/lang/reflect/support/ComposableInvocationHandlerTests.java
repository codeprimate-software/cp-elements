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
package org.cp.elements.lang.reflect.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.cp.elements.util.ArrayUtils.asArray;
import static org.cp.elements.util.ArrayUtils.asIterable;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.cp.elements.lang.reflect.UnhandledMethodInvocationException;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import lombok.Data;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;

/**
 * Unit Tests for {@link ComposableInvocationHandler}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.reflect.support.ComposableInvocationHandler
 * @see lombok
 * @since 1.0.0
 */
@ExtendWith(MockitoExtension.class)
public class ComposableInvocationHandlerTests {

  @Mock
  private InvocationHandler mockInvocationHandlerOne;

  @Mock
  private InvocationHandler mockInvocationHandlerTwo;

  @Test
  public void composeArrayOfInvocationHandlers() {

    ComposableInvocationHandler invocationHandlers =
      ComposableInvocationHandler.compose(mockInvocationHandlerOne, mockInvocationHandlerTwo);

    assertThat(invocationHandlers).isNotNull();
    assertThat(invocationHandlers.getInvocationHandlers()).hasSize(2);
    assertThat(invocationHandlers.getInvocationHandlers()).contains(mockInvocationHandlerOne, mockInvocationHandlerTwo);
  }

  @Test
  public void composeIterableOfInvocationHandlers() {

    ComposableInvocationHandler invocationHandlers =
      ComposableInvocationHandler.compose(asIterable(mockInvocationHandlerOne));

    assertThat(invocationHandlers).isNotNull();
    assertThat(invocationHandlers.getInvocationHandlers()).hasSize(1);
    assertThat(invocationHandlers.getInvocationHandlers()).contains(mockInvocationHandlerOne);
  }

  @Test
  public void invokeHandledByFirstInvocationHandler() throws Throwable {

    Object proxy = new Object();
    Method getName = Contact.class.getMethod("getName");
    Object[] arguments = asArray("argOne", "argTwo");

    when(mockInvocationHandlerOne.invoke(any(), any(Method.class), any(Object[].class))).thenReturn("one");

    ComposableInvocationHandler invocationHandler =
      ComposableInvocationHandler.compose(mockInvocationHandlerOne, mockInvocationHandlerTwo);

    assertThat(invocationHandler).isNotNull();
    assertThat(invocationHandler.invoke(proxy, getName, arguments)).isEqualTo("one");

    verify(mockInvocationHandlerOne, times(1)).invoke(eq(proxy), eq(getName), eq(arguments));
    verify(mockInvocationHandlerTwo, never()).invoke(any(), any(Method.class), any(Object[].class));
  }

  @Test
  public void invokeHandledBySecondInvocationHandler() throws Throwable {

    Object proxy = new Object();
    Method getName = Contact.class.getMethod("getName");
    Object[] arguments = asArray("argOne", "argTwo");

    when(mockInvocationHandlerOne.invoke(any(), any(Method.class), any(Object[].class)))
      .thenThrow(new UnhandledMethodInvocationException("test"));
    when(mockInvocationHandlerTwo.invoke(any(), any(Method.class), any(Object[].class))).thenReturn("two");

    ComposableInvocationHandler invocationHandler =
      ComposableInvocationHandler.compose(mockInvocationHandlerOne, mockInvocationHandlerTwo);

    assertThat(invocationHandler).isNotNull();
    assertThat(invocationHandler.invoke(proxy, getName, arguments)).isEqualTo("two");

    verify(mockInvocationHandlerOne, times(1)).invoke(eq(proxy), eq(getName), eq(arguments));
    verify(mockInvocationHandlerTwo, times(1)).invoke(eq(proxy), eq(getName), eq(arguments));
  }

  @Test
  public void invokeThrowsUnhandledMethodInvocationException() throws Throwable {

    Object proxy = new Object();
    Method getName = Contact.class.getMethod("getName");
    Object[] arguments = asArray("argOne", "argTwo");

    when(this.mockInvocationHandlerOne.invoke(any(), any(Method.class), any(Object[].class)))
      .thenThrow(new UnhandledMethodInvocationException("test"));
    when(this.mockInvocationHandlerTwo.invoke(any(), any(Method.class), any(Object[].class)))
      .thenThrow(new UnhandledMethodInvocationException("test"));

    ComposableInvocationHandler invocationHandler =
      ComposableInvocationHandler.compose(this.mockInvocationHandlerOne, this.mockInvocationHandlerTwo);

    assertThat(invocationHandler).isNotNull();

    assertThatExceptionOfType(UnhandledMethodInvocationException.class)
      .isThrownBy(() -> invocationHandler.invoke(proxy, getName, arguments))
      .withMessage("Method [getName] was not handled")
      .withNoCause();

    verify(this.mockInvocationHandlerOne, times(1)).invoke(eq(proxy), eq(getName), eq(arguments));
    verify(this.mockInvocationHandlerTwo, times(1)).invoke(eq(proxy), eq(getName), eq(arguments));
  }

  @Test
  public void iteratorIsSuccessful() {

    InvocationHandler mockInvocationHandlerThree = mock(InvocationHandler.class);
    ComposableInvocationHandler invocationHandler = ComposableInvocationHandler.compose(
      mockInvocationHandlerOne, mockInvocationHandlerTwo, mockInvocationHandlerThree);

    assertThat(invocationHandler).isNotNull();
    assertThat(invocationHandler.getInvocationHandlers()).contains(
      mockInvocationHandlerOne, mockInvocationHandlerTwo, mockInvocationHandlerThree);
    assertThat(invocationHandler).contains(mockInvocationHandlerOne, mockInvocationHandlerTwo,
      mockInvocationHandlerThree);
  }

  @Data
  @RequiredArgsConstructor(staticName = "newContact")
  @SuppressWarnings("all")
  static class Contact {
    @NonNull String name;
  }
}
