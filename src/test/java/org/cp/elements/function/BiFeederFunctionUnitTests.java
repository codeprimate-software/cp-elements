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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.function.BiFunction;

import org.junit.jupiter.api.Test;

import org.mockito.ArgumentMatchers;

/**
 * Unit Tests for {@link BiFeederFunction}.
 *
 * @author John Blum
 * @see java.util.function.BiFunction
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.function.BiFeederFunction
 * @since 1.0.0
 */
public class BiFeederFunctionUnitTests {

  @Test
  @SuppressWarnings("unchecked")
  public void andThenComposesBiFeederFunctions() {

    BiFeederFunction<String, String> functionOne = mock(BiFeederFunction.class, "FUNONE");
    BiFeederFunction<String, String> functionTwo = mock(BiFeederFunction.class, "FUNTWO");

    doCallRealMethod().when(functionOne).andThen(any(BiFunction.class));
    doCallRealMethod().when(functionOne).merge(any(), any());
    doReturn("ONE").when(functionOne).apply(any(), any());
    doReturn("TWO").when(functionTwo).apply(any(), eq("ONE"));

    BiFeederFunction<String, String> composedFunction = functionOne.andThen(functionTwo);

    assertThat(composedFunction).isNotNull();
    assertThat(composedFunction).isNotSameAs(functionOne);
    assertThat(composedFunction).isNotSameAs(functionTwo);
    assertThat(composedFunction.apply("TEST", "ZERO")).isEqualTo("TWO");

    verify(functionOne, times(1)).andThen(eq(functionTwo));
    verify(functionOne, times(1)).apply(eq("TEST"), eq("ZERO"));
    verify(functionOne, times(1)).merge(eq("ZERO"), eq("ONE"));
    verify(functionTwo, times(1)).apply(eq("TEST"), eq("ONE"));
    verifyNoMoreInteractions(functionOne, functionTwo);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void andThenWithNullBiFunction() {

    BiFeederFunction<Object, Object> mockFunction = mock(BiFeederFunction.class);

    doCallRealMethod().when(mockFunction).andThen(ArgumentMatchers.<BiFunction<Object, Object, Object>>any());

    assertThatIllegalArgumentException()
      .isThrownBy(() ->mockFunction.andThen((BiFunction<Object, Object, Object>) null))
      .withMessage("The BiFunction to compose with and apply after this BiFeederFunction [%s] is required",
        mockFunction.getClass().getName())
      .withNoCause();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void mergeReturnsThisFunctionsReturnValue() {

    BiFeederFunction<Object, Object> function = mock(BiFeederFunction.class);

    doCallRealMethod().when(function).merge(any(), any());

    assertThat(function.merge("ONE", "TWO")).isEqualTo("TWO");
  }
}
