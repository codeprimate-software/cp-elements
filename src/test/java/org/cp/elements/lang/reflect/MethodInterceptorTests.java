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

import org.cp.elements.lang.Assert;
import org.junit.Test;

import lombok.NoArgsConstructor;

/**
 * Unit tests for the {@link MethodInterceptor} interface.
 *
 * @author John Blum
 * @see java.lang.reflect.Method
 * @see org.junit.Test
 * @see lombok
 * @see org.cp.elements.lang.reflect.MethodInterceptor
 * @see org.cp.elements.lang.reflect.MethodInvocation
 * @since 1.0.0
 */
public class MethodInterceptorTests {

  @Test
  public void invocationHandlerInvokeCallsMethodInterceptorIntercept() throws Throwable {
    AgeCalculator ageCalculator = AgeCalculator.newAgeCalculator();

    Method ageMethod = ageCalculator.getClass().getDeclaredMethod("getAge", LocalDate.class, LocalDate.class);

    Object[] expectedArguments = { LocalDate.of(1974, Month.MAY, 27), LocalDate.of(2016, Month.JULY, 1) };

    Object proxy = new Object();

    Object methodInvocationReference =
      new TestMethodInterceptor(ageCalculator).invoke(proxy, ageMethod, expectedArguments);

    assertThat(methodInvocationReference).isInstanceOf(MethodInvocation.class);

    MethodInvocation methodInvocation = (MethodInvocation) methodInvocationReference;

    assertThat(methodInvocation.getArguments()).isEqualTo(expectedArguments);
    assertThat(methodInvocation.getMethod()).isEqualTo(ageMethod);
    assertThat(methodInvocation.getTarget()).isSameAs(ageCalculator);
  }

  @NoArgsConstructor(staticName = "newAgeCalculator")
  @SuppressWarnings("all")
  static class AgeCalculator {

    public int getAge(LocalDate birthDate) {
      return getAge(birthDate, LocalDate.now());
    }

    public int getAge(LocalDate birthDate, LocalDate presentDate) {
      return Period.between(birthDate, presentDate).getYears();
    }
  }

  class TestMethodInterceptor implements MethodInterceptor {

    private final Object target;

    TestMethodInterceptor(Object target) {
      Assert.notNull(target, "Target object must not be null");
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
