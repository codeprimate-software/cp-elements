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
package org.cp.elements.data.oql.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.function.BiFunction;
import java.util.function.BiPredicate;
import java.util.function.Function;
import java.util.function.Predicate;

import org.junit.jupiter.api.Test;

import org.cp.elements.data.oql.QueryArguments;
import org.cp.elements.data.oql.QueryContext;

/**
 * Unit Tests for {@link OqlUtils}.
 *
 * @author John Blum
 * @see org.cp.elements.data.oql.support.OqlUtils
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @since 2.0.0
 */
@SuppressWarnings("unchecked")
class OqlUtilsUnitTests {

  @Test
  void asBiFunction() {

    Function<Object, Object> mockFunction = mock(Function.class);
    QueryContext<Object, Object> mockQueryContext = mock(QueryContext.class);

    doReturn("MOCK").when(mockFunction).apply(any());

    BiFunction<QueryContext<Object, Object>, Object, Object> function = OqlUtils.asBiFunction(mockFunction);

    assertThat(function).isNotNull();
    assertThat(function.apply(mockQueryContext, "TEST")).isEqualTo("MOCK");

    verify(mockFunction, times(1)).apply(eq("TEST"));
    verifyNoMoreInteractions(mockFunction);
    verifyNoInteractions(mockQueryContext);
  }

  @Test
  void asBiFunctionWithNullFunction() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> OqlUtils.asBiFunction(null))
      .withMessage("Function is required")
      .withNoCause();
  }

  @Test
  void asFunction() {

    BiFunction<QueryContext<Object, Object>, Object, Object> mockFunction = mock(BiFunction.class);
    QueryContext<Object, Object> mockQueryContext = mock(QueryContext.class);

    doReturn("MOCK").when(mockFunction).apply(eq(mockQueryContext), any());

    Function<Object, Object> function = OqlUtils.asFunction(mockFunction, mockQueryContext);

    assertThat(function).isNotNull();
    assertThat(function.apply("TEST")).isEqualTo("MOCK");

    verify(mockFunction, times(1)).apply(eq(mockQueryContext), eq("TEST"));
    verifyNoMoreInteractions(mockFunction);
    verifyNoInteractions(mockQueryContext);
  }

  @Test
  void asFunctionWithNullBiFunction() {

    QueryContext<?, ?> mockQueryContext = mock(QueryContext.class);

    assertThatIllegalArgumentException()
      .isThrownBy(() -> OqlUtils.asFunction(null, mockQueryContext))
      .withMessage("Function is required")
      .withNoCause();

    verifyNoInteractions(mockQueryContext);
  }

  @Test
  void asFunctionWithNullQueryContext() {

    BiFunction<QueryContext<Object, Object>, Object, Object> mockFunction = mock(BiFunction.class);

    assertThatIllegalArgumentException()
      .isThrownBy(() -> OqlUtils.asFunction(mockFunction, null))
      .withMessage("QueryContext is required")
      .withNoCause();

    verifyNoInteractions(mockFunction);
  }

  @Test
  void asBiPredicate() {

    QueryArguments mockQueryArguments = mock(QueryArguments.class);
    Predicate<Object> mockPredicate = mock(Predicate.class);

    doReturn(true).when(mockPredicate).test(any());

    BiPredicate<QueryArguments, Object> predicate = OqlUtils.asBiPredicate(mockPredicate);

    assertThat(predicate).isNotNull();
    assertThat(predicate.test(mockQueryArguments, "TEST")).isTrue();

    verify(mockPredicate, times(1)).test(eq("TEST"));
    verifyNoMoreInteractions(mockPredicate);
    verifyNoInteractions(mockQueryArguments);
  }

  @Test
  void asBiPredicateWithNullPredicate() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> OqlUtils.asBiPredicate(null))
      .withMessage("Predicate is required")
      .withNoCause();
  }

  @Test
  void asPredicate() {

    BiPredicate<QueryArguments, Object> mockPredicate = mock(BiPredicate.class);

    doReturn(true).when(mockPredicate).test(any(QueryArguments.class), any());

    Predicate<Object> predicate = OqlUtils.asPredicate(mockPredicate);

    assertThat(predicate).isNotNull();
    assertThat(predicate.test("MOCK")).isTrue();

    verify(mockPredicate, times(1)).test(isA(QueryArguments.class), eq("MOCK"));
    verifyNoMoreInteractions(mockPredicate);
  }

  @Test
  void asPredicateWithNullBiPredicate() {

    QueryArguments mockQueryArguments = mock(QueryArguments.class);

    assertThatIllegalArgumentException()
      .isThrownBy(() -> OqlUtils.asPredicate(null, mockQueryArguments))
      .withMessage("Predicate is required")
      .withNoCause();

    verifyNoInteractions(mockQueryArguments);
  }

  @Test
  void asPredicateWithNullQueryArguments() {

    BiPredicate<QueryArguments, Object> mockPredicate = mock(BiPredicate.class);

    assertThatIllegalArgumentException()
      .isThrownBy(() -> OqlUtils.asPredicate(mockPredicate, null))
      .withMessage("QueryArguments are required")
      .withNoCause();

    verifyNoInteractions(mockPredicate);
  }

  @Test
  void nullSafePredicateReturnsPredicate() {

    BiPredicate<QueryArguments, Object> mockPredicate = mock(BiPredicate.class);

    assertThat(OqlUtils.nullSafePredicate(mockPredicate)).isSameAs(mockPredicate);

    verifyNoInteractions(mockPredicate);
  }

  @Test
  void nullSafePredicateReturnsNonNullPredicate() {

    QueryArguments mockQueryArguments = mock(QueryArguments.class);

    BiPredicate<QueryArguments, Object> predicate = OqlUtils.nullSafePredicate(null);

    assertThat(predicate).isNotNull();
    assertThat(predicate.test(mockQueryArguments, false)).isTrue();
    assertThat(predicate.test(mockQueryArguments, 'x')).isTrue();
    assertThat(predicate.test(mockQueryArguments, 2)).isTrue();
    assertThat(predicate.test(mockQueryArguments, Math.PI)).isTrue();
    assertThat(predicate.test(mockQueryArguments, "MOCK")).isTrue();
    assertThat(predicate.test(mockQueryArguments, "TEST")).isTrue();
    assertThat(predicate.test(mockQueryArguments, null)).isTrue();

    verifyNoInteractions(mockQueryArguments);
  }
}
