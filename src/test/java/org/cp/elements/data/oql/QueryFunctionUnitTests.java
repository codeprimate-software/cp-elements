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
package org.cp.elements.data.oql;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.Test;

import org.mockito.ArgumentMatchers;

/**
 * Unit Tests for {@link QueryFunction}.
 *
 * @author John Blum
 * @see org.cp.elements.data.oql.QueryFunction
 * @see org.junit.jupiter.api.Test
 * @since 2.0.0
 */
public class QueryFunctionUnitTests {

  @Test
  @SuppressWarnings("unchecked")
  void applyArrayCallsApplyIterable() {

    Object[] array = { 1, 2, 3, 4, 5 };

    QueryFunction<Object, Object> function = mock(QueryFunction.class);

    doAnswer(invocation -> {
      Iterable<Object> iterable = invocation.getArgument(0);
      assertThat(iterable).isNotNull();
      assertThat(iterable).containsExactly(array);
      return "TEST";
    }).when(function).apply(isA(Iterable.class));

    doCallRealMethod().when(function).apply(any(Object[].class));

    assertThat(function.apply(array)).isEqualTo("TEST");

    verify(function, times(1)).apply(eq(array));
    verify(function, times(1)).apply(isA(Iterable.class));
  }

  @Test
  @SuppressWarnings("unchecked")
  void applyArrayIsNullSafe() {

    QueryFunction<Object, Object> function = mock(QueryFunction.class);

    doAnswer(invocation -> {
      Iterable<Object> iterable = invocation.getArgument(0);
      assertThat(iterable).isNotNull();
      assertThat(iterable).isEmpty();
      return "MOCK";
    }).when(function).apply(isA(Iterable.class));

    doCallRealMethod().when(function).apply(ArgumentMatchers.<Object[]>any());

    assertThat(function.apply((Object[]) null)).isEqualTo("MOCK");

    verify(function, times(1)).apply(ArgumentMatchers.<Object[]>isNull());
    verify(function, times(1)).apply(isA(Iterable.class));
  }
}
