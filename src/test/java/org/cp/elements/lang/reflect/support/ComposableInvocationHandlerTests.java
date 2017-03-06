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

package org.cp.elements.lang.reflect.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.cp.elements.util.ArrayUtils.asArray;
import static org.cp.elements.util.ArrayUtils.asIterable;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyObject;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;

import org.cp.elements.lang.reflect.UnhandledMethodInvocationException;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

import lombok.Data;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;

/**
 * Unit tests for {@link ComposableInvocationHandler}.
 *
 * @author John Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see lombok
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.runners.MockitoJUnitRunner
 * @see org.cp.elements.lang.reflect.support.ComposableInvocationHandler
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class ComposableInvocationHandlerTests {

  @Rule
  public ExpectedException exception = ExpectedException.none();

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

    when(mockInvocationHandlerOne.invoke(anyObject(), any(Method.class), any(Object[].class))).thenReturn("one");
    when(mockInvocationHandlerTwo.invoke(anyObject(), any(Method.class), any(Object[].class))).thenReturn("two");

    ComposableInvocationHandler invocationHandler =
      ComposableInvocationHandler.compose(mockInvocationHandlerOne, mockInvocationHandlerTwo);

    assertThat(invocationHandler).isNotNull();
    assertThat(invocationHandler.invoke(proxy, getName, arguments)).isEqualTo("one");

    verify(mockInvocationHandlerOne, times(1)).invoke(eq(proxy), eq(getName), eq(arguments));
    verify(mockInvocationHandlerTwo, never()).invoke(anyObject(), any(Method.class), any(Object[].class));
  }

  @Test
  public void invokeHandledBySecondInvocationHandler() throws Throwable {
    Object proxy = new Object();
    Method getName = Contact.class.getMethod("getName");
    Object[] arguments = asArray("argOne", "argTwo");

    when(mockInvocationHandlerOne.invoke(anyObject(), any(Method.class), any(Object[].class)))
      .thenThrow(new UnhandledMethodInvocationException("test"));
    when(mockInvocationHandlerTwo.invoke(anyObject(), any(Method.class), any(Object[].class))).thenReturn("two");

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

    when(mockInvocationHandlerOne.invoke(anyObject(), any(Method.class), any(Object[].class)))
      .thenThrow(new UnhandledMethodInvocationException("test"));
    when(mockInvocationHandlerTwo.invoke(anyObject(), any(Method.class), any(Object[].class)))
      .thenThrow(new UnhandledMethodInvocationException("test"));

    ComposableInvocationHandler invocationHandler =
      ComposableInvocationHandler.compose(mockInvocationHandlerOne, mockInvocationHandlerTwo);

    assertThat(invocationHandler).isNotNull();

    try {
      exception.expect(UnhandledMethodInvocationException.class);
      exception.expectCause(is(nullValue(Throwable.class)));
      exception.expectMessage("Method [getName] was not handled");

      invocationHandler.invoke(proxy, getName, arguments);
    }
    finally {
      verify(mockInvocationHandlerOne, times(1)).invoke(eq(proxy), eq(getName), eq(arguments));
      verify(mockInvocationHandlerTwo, times(1)).invoke(eq(proxy), eq(getName), eq(arguments));
    }
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
  static class Contact {
    @NonNull String name;
  }
}
