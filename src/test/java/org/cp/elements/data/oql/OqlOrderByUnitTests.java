/*
 * Copyright 2017-Present Author or Authors.
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
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Comparator;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;

import org.cp.elements.data.oql.Oql.OrderBy;
import org.cp.elements.lang.Constants;
import org.mockito.ArgumentMatchers;

/**
 * Unit Tests for {@link Oql.OrderBy}.
 *
 * @author John Blum
 * @see Oql.OrderBy
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @since 2.0.0
 */
@SuppressWarnings("unchecked")
public class OqlOrderByUnitTests {

  @Test
  void orderByNoComparators() {

    Stream<Comparator<?>> mockComparators = mock(Stream.class);
    Oql.OrderBy<?, ?> mockOrderBy = mock(Oql.OrderBy.class);

    doReturn(mockComparators).when(mockOrderBy).stream();
    doCallRealMethod().when(mockOrderBy).getOrder();

    assertThatExceptionOfType(IllegalStateException.class)
      .isThrownBy(mockOrderBy::getOrder)
      .withMessage("No Order Defined")
      .withNoCause();

    verify(mockOrderBy, times(1)).getOrder();
    verify(mockOrderBy, times(1)).stream();
    verifyNoMoreInteractions(mockOrderBy);
  }

  @Test
  void orderBySpecGetOrderByIsEmpty() {

    Oql.OrderBySpec<Object, Object> mockOrderBySpec = mock(Oql.OrderBySpec.class);

    doCallRealMethod().when(mockOrderBySpec).getOrderBy();

    Optional<OrderBy<Object, Object>> orderBy = mockOrderBySpec.getOrderBy();

    assertThat(orderBy).isNotNull();
    assertThat(orderBy).isNotPresent();

    verify(mockOrderBySpec, times(1)).getOrderBy();
    verifyNoMoreInteractions(mockOrderBySpec);
  }

  @Test
  void orderBySpecDefaultOrderByComparatorIsUnsupported() {

    Comparator<Object> mockComparator = mock(Comparator.class);
    Oql.OrderBySpec<Object, Object> mockOrderBySpec = mock(Oql.OrderBySpec.class);

    doCallRealMethod().when(mockOrderBySpec).orderBy(mockComparator);

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(() -> mockOrderBySpec.orderBy(mockComparator))
      .withMessage(Constants.UNSUPPORTED_OPERATION)
      .withNoCause();

    verify(mockOrderBySpec, times(1)).orderBy(eq(mockComparator));
    verifyNoInteractions(mockComparator);
  }

  @Test
  void orderBySpecDefaultOrderByFunctionIsUnsupported() {

    Function<Object, String> mockFunction = mock(Function.class);
    Oql.OrderBySpec<Object, Object> mockOrderBySpec = mock(Oql.OrderBySpec.class);

    doCallRealMethod().when(mockOrderBySpec).orderBy(isA(Function.class));
    doCallRealMethod().when(mockOrderBySpec).orderBy(isA(Comparator.class));

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(() -> mockOrderBySpec.orderBy(mockFunction))
      .withMessage(Constants.UNSUPPORTED_OPERATION)
      .withNoCause();

    verify(mockOrderBySpec, times(1)).orderBy(eq(mockFunction));
    verify(mockOrderBySpec, times(1)).orderBy(isA(Comparator.class));
    verifyNoMoreInteractions(mockOrderBySpec);
    verifyNoInteractions(mockFunction);
  }

  @Test
  void orderBySpecDefaultOrderByNullFunction() {

    Oql.OrderBySpec<Object, Object> mockOrderBySpec = mock(Oql.OrderBySpec.class);

    doCallRealMethod().when(mockOrderBySpec).orderBy(ArgumentMatchers.<Function<Object, String>>any());

    assertThatExceptionOfType(IllegalArgumentException.class)
      .isThrownBy(() -> mockOrderBySpec.orderBy((Function<Object, String>) null))
      .withMessage("Function defining order is required")
      .withNoCause();

    verify(mockOrderBySpec, times(1)).orderBy(ArgumentMatchers.<Function<Object, String>>any());
    verify(mockOrderBySpec, never()).orderBy(any(Comparator.class));
    verifyNoMoreInteractions(mockOrderBySpec);
  }
}
