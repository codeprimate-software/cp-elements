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
import static org.cp.elements.lang.reflect.support.DelegatingMethodInterceptor.newDelegatingMethodInterceptor;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Optional;

import org.cp.elements.lang.reflect.MethodInterceptor;
import org.cp.elements.lang.reflect.MethodInvocation;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit tests for {@link DelegatingMethodInterceptor}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.cp.elements.lang.reflect.MethodInterceptor
 * @see org.cp.elements.lang.reflect.MethodInvocation
 * @see org.cp.elements.lang.reflect.support.DelegatingMethodInterceptor
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class DelegatingMethodInterceptorTests {

  @Mock
  private MethodInterceptor<Object> mockDelegate;

  @Mock
  private MethodInvocation mockMethodInvocation;

  @Test
  public void newDelegatingMethodInterceptorWithDelegateIsSuccessful() {

    DelegatingMethodInterceptor<?> methodInterceptor = newDelegatingMethodInterceptor(mockDelegate);

    assertThat(methodInterceptor).isNotNull();
    assertThat(methodInterceptor.getDelegate()).isSameAs(mockDelegate);
  }

  @Test(expected = IllegalStateException.class)
  public void newDelegatingMethodInterceptorWithoutDelegateIsSuccessful() {

    DelegatingMethodInterceptor<?> methodInterceptor = newDelegatingMethodInterceptor();

    assertThat(methodInterceptor).isNotNull();

    methodInterceptor.getDelegate();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void setAndGetDelegateIsSuccessful() {

    MethodInterceptor<?> mockMethodInterceptor = mock(MethodInterceptor.class);

    DelegatingMethodInterceptor methodInterceptor = newDelegatingMethodInterceptor(mockMethodInterceptor);

    assertThat(methodInterceptor).isNotNull();
    assertThat(methodInterceptor.getDelegate()).isSameAs(mockMethodInterceptor);

    methodInterceptor.setDelegate(mockDelegate);

    assertThat(methodInterceptor.getDelegate()).isSameAs(mockDelegate);

    methodInterceptor.setDelegate(mockMethodInterceptor);

    assertThat(methodInterceptor.getDelegate()).isSameAs(mockMethodInterceptor);
  }

  @Test(expected = IllegalStateException.class)
  public void getUninitializedDelegateThrowsIllegalStateException() {

    try {
      newDelegatingMethodInterceptor().getDelegate();
    }
    catch (IllegalStateException expected) {

      assertThat(expected).hasMessage("The delegate MethodInterceptor was not properly initialized");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void getTargetDelegatesToDelegateGetTarget() {

    when(mockDelegate.getTarget()).thenReturn("test");

    assertThat(newDelegatingMethodInterceptor(mockDelegate).getTarget()).isEqualTo("test");

    verify(mockDelegate, times(1)).getTarget();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void interceptDelegatesToDelegateInvoke() {

    when(mockDelegate.intercept(any(MethodInvocation.class))).thenReturn(Optional.of("test"));

    assertThat(newDelegatingMethodInterceptor(mockDelegate).intercept(mockMethodInvocation).orElse(null))
      .isEqualTo("test");

    verify(mockDelegate, times(1)).intercept(eq(mockMethodInvocation));
  }
}
