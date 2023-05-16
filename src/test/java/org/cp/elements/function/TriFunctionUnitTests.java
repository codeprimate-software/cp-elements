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
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.time.Instant;
import java.util.function.Function;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.StringUtils;
import org.cp.elements.security.model.User;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.ToString;

/**
 * Unit Tests for {@link TriFunction}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.function.TriFunction
 * @since 1.0.0
 */
public class TriFunctionUnitTests {

  @Test
  @SuppressWarnings("unchecked")
  public void andThenComposesFunctionWithTriFunction() {

    Function<User<Integer>, User<Integer>> mockFunction = mock(Function.class);

    TriFunction<String, Object, Object, TestUser> triFunction = mock(TriFunction.class);

    doAnswer(invocation -> invocation.<User<Integer>>getArgument(0).identifiedBy(2))
      .when(mockFunction).apply(any(User.class));

    doAnswer(invocation -> {
      User<Integer> user = TestUser.as(StringUtils.capitalize(invocation.getArgument(0)));
      assertThat(user.getId()).isNull();
      return user;
    }).when(triFunction).apply(anyString(), any(), any());

    doCallRealMethod().when(triFunction).andThen(any(Function.class));

    TriFunction<String, Object, Object, User<Integer>> composedTriFunction = triFunction.andThen(mockFunction);

    assertThat(composedTriFunction).isNotNull();
    assertThat(composedTriFunction).isNotEqualTo(mockFunction);
    assertThat(composedTriFunction).isNotSameAs(triFunction);

    User<Integer> user = composedTriFunction.apply("jonDoe", Instant.now(), "Test User");

    assertThat(user).isNotNull();
    assertThat(user.getId()).isEqualTo(2);
    assertThat(user.getName()).isEqualTo("JonDoe");

    verify(triFunction, times(1)).andThen(eq(mockFunction));
    verify(triFunction, times(1))
      .apply(eq("jonDoe"), isA(Instant.class), eq("Test User"));
    verify(mockFunction, times(1)).apply(isA(TestUser.class));
    verifyNoMoreInteractions(mockFunction, triFunction);
  }

  @Test
  public void andThenWithNullFunction() {

    TriFunction<?, ?, ?, ?> triFunction = mock(TriFunction.class);

    doCallRealMethod().when(triFunction).andThen(any());

    assertThatIllegalArgumentException()
      .isThrownBy(() -> triFunction.andThen(null))
      .withMessage("The Function to apply to the result of this TriFunction is required")
      .withNoCause();

    verify(triFunction, times(1)).andThen(eq(null));
    verifyNoMoreInteractions(triFunction);
  }

  @Getter
  @ToString(of = "name")
  @EqualsAndHashCode(of = "name")
  @RequiredArgsConstructor(staticName = "as")
  static class TestUser implements User<Integer> {

    @Setter
    private Integer id;

    @lombok.NonNull
    private final String name;

  }
}
