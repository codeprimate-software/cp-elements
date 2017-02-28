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
import static org.cp.elements.lang.ObjectUtils.EMPTY_OBJECT_ARRAY;
import static org.cp.elements.lang.reflect.MethodInvocation.newMethodInvocation;
import static org.cp.elements.lang.reflect.support.MethodInvokingMethodInterceptor.newMethodInvokingMethodInterceptor;

import java.time.LocalDate;
import java.time.Month;
import java.time.Period;

import org.cp.elements.lang.reflect.MethodInvocation;
import org.junit.Test;

import lombok.Data;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;

/**
 * Unit tests for {@link MethodInvokingMethodInterceptor}.
 *
 * @author John Blum
 * @see java.lang.reflect.Method
 * @see org.junit.Test
 * @see lombok
 * @see org.cp.elements.lang.reflect.MethodInterceptor
 * @see org.cp.elements.lang.reflect.MethodInvocation
 * @see org.cp.elements.lang.reflect.support.MethodInvokingMethodInterceptor
 * @since 1.0.0
 */
public class MethodInvokingMethodInterceptorTests {

  @Test
  public void constructMethodInvokingMethodInterceptorWithNull() {
    MethodInvokingMethodInterceptor methodInterceptor = newMethodInvokingMethodInterceptor(null);

    assertThat(methodInterceptor).isNotNull();
    assertThat(methodInterceptor.getTarget()).isNull();
  }

  @Test
  public void constructMethodInvokingMethodInterceptorWithTarget() {
    Object target = new Object();

    MethodInvokingMethodInterceptor methodInterceptor = newMethodInvokingMethodInterceptor(target);

    assertThat(methodInterceptor).isNotNull();
    assertThat(methodInterceptor.getTarget()).isSameAs(target);
  }

  @Test
  public void interceptInvokesMethod() {
    AgeCalculator johnAgeCalculator = AgeCalculator.newAgeCalculator(LocalDate.of(1974, Month.MAY, 27));
    MethodInvocation ageMethodInvocation = newMethodInvocation(johnAgeCalculator, "getAge");

    assertThat(newMethodInvokingMethodInterceptor(johnAgeCalculator).intercept(ageMethodInvocation)).isEqualTo(42);
  }

  @Test
  public void interceptInvokesMethodUsingInterceptorTarget() {
    AgeCalculator ellieAgeCalculator = AgeCalculator.newAgeCalculator(LocalDate.of(2008, Month.AUGUST, 25));
    AgeCalculator johnAgeCalculator = AgeCalculator.newAgeCalculator(LocalDate.of(1974, Month.MAY, 27));

    MethodInvocation ageMethodInvocation = newMethodInvocation(johnAgeCalculator, "getAge");

    assertThat(newMethodInvokingMethodInterceptor(ellieAgeCalculator).intercept(ageMethodInvocation)).isEqualTo(8);
  }

  @Test
  public void invokeCallsIntercept() throws Throwable {
    AgeCalculator saraAgeCalculator = AgeCalculator.newAgeCalculator(LocalDate.of(1975, Month.JANUARY, 22));

    assertThat(newMethodInvokingMethodInterceptor(saraAgeCalculator)
      .invoke(new Object(), saraAgeCalculator.getClass().getMethod("getAge"), EMPTY_OBJECT_ARRAY))
        .isEqualTo(42);
  }

  @Data
  @RequiredArgsConstructor(staticName = "newAgeCalculator")
  @SuppressWarnings("unused")
  static class AgeCalculator {

    @NonNull
    private final LocalDate birthDate;

    public int getAge() {
      return Period.between(getBirthDate(), LocalDate.of(2017, Month.FEBRUARY, 13)).getYears();
    }
  }
}
