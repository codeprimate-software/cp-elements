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

package org.cp.elements.lang.reflect;

import static org.assertj.core.api.Assertions.assertThat;

import java.lang.reflect.Method;
import java.time.LocalDate;
import java.time.Month;
import java.time.Period;

import org.junit.Test;

import lombok.Data;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;

/**
 * Unit tests for the {@link MethodInterceptor} interface.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see lombok
 * @see org.cp.elements.lang.reflect.MethodInterceptor
 * @since 1.0.0
 */
public class MethodInterceptorTests {

  @Test
  public void invocationHandlerInvokeCallsMethodInterceptorIntercept() throws Throwable {
    AgeCalculator ageCalculator = AgeCalculator.newAgeCalculator(LocalDate.of(1974, Month.MAY, 27));

    Method ageMethod = ageCalculator.getClass().getDeclaredMethod("getAge");

    Object[] expectedArguments = { "argOne", "argTwo" };

    Object proxy = new Object();

    Object methodInvocationReference = new TestMethodInterceptor(ageCalculator)
      .invoke(proxy, ageMethod, expectedArguments);

    assertThat(methodInvocationReference).isInstanceOf(MethodInvocation.class);

    MethodInvocation methodInvocation = (MethodInvocation) methodInvocationReference;

    assertThat(methodInvocation.getArguments()).isEqualTo(expectedArguments);
    assertThat(methodInvocation.getMethod()).isEqualTo(ageMethod);
    assertThat(methodInvocation.getTarget()).isSameAs(ageCalculator);
  }

  @Data
  @RequiredArgsConstructor(staticName = "newAgeCalculator")
  @SuppressWarnings("unused")
  static class AgeCalculator {

    @NonNull
    private final LocalDate birthDate;

    public int getAge() {
      return Period.between(LocalDate.now(), getBirthDate()).getYears();
    }
  }

  class TestMethodInterceptor implements MethodInterceptor {

    private final Object target;

    TestMethodInterceptor(Object target) {
      this.target = target;
    }

    /**
     * @inheritDoc
     */
    @Override
    public Object getTarget() {
      return this.target;
    }

    /**
     * @inheritDoc
     */
    @Override
    public Object intercept(MethodInvocation methodInvocation) {
      return methodInvocation;
    }
  }
}
