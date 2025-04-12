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
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

import java.util.Comparator;
import java.util.Set;

import org.junit.jupiter.api.Test;

import org.cp.elements.data.oql.Oql;

/**
 * Unit Tests for {@link OrderByClause}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @since 3.0.0
 */
public class OrderByClauseUnitTests {

  @Test
  @SuppressWarnings("unchecked")
  void constructOrderByClause() {

    Oql.From<Object, Object> mockFrom = mock(Oql.From.class);
    Comparator<Object> mockComparator = mock(Comparator.class);

    OrderByClause<?, ?> orderBy = new OrderByClause<>(mockFrom, mockComparator);

    assertThat(orderBy).isNotNull();
    assertThat(orderBy.getFrom()).isEqualTo(mockFrom);
    assertThat(orderBy.getOrder()).isEqualTo(mockComparator);

    verifyNoInteractions(mockFrom, mockComparator);
  }

  @Test
  @SuppressWarnings("unchecked")
  void constructOrderByClauseWithNullFrom() {

    Comparator<?> mockComparator = mock(Comparator.class);

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new OrderByClause<>(null, mockComparator))
      .withMessage("From clause is required")
      .withNoCause();

    verifyNoInteractions(mockComparator);
  }

  @Test
  @SuppressWarnings("unchecked")
  void constructOrderByClauseWithNullOrder() {

    Oql.From<Object, Object> mockFrom = mock(Oql.From.class);

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new OrderByClause<>(mockFrom, (Comparator<Object>[]) null))
      .withMessage("Comparators used to sort are required")
      .withNoCause();

    verifyNoInteractions(mockFrom);
  }

  @Test
  @SuppressWarnings("unchecked")
  void copyOrderBy() {

    Oql.From<Object, Object> mockFrom = mock(Oql.From.class);
    Comparator<Object> mockComparator = mock(Comparator.class);
    Oql.OrderBy<Object, Object> mockOrderBy = mock(Oql.OrderBy.class);

    doReturn(mockFrom).when(mockOrderBy).getFrom();
    doReturn(Set.of(mockComparator).spliterator()).when(mockOrderBy).spliterator();

    OrderByClause<Object, Object> orderBy = OrderByClause.copy(mockOrderBy);

    assertThat(orderBy).isNotNull();
    assertThat(orderBy.getFrom()).isEqualTo(mockFrom);
    assertThat(orderBy.getOrder()).isEqualTo(mockComparator);

    verify(mockOrderBy, times(1)).getFrom();
    verifyNoInteractions(mockFrom, mockComparator);
  }

  @Test
  void copyNullOrderBy() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> OrderByClause.copy(null))
      .withMessage("OrderBy clause to copy is required")
      .withNoCause();
  }

  @Test
  @SuppressWarnings("unchecked")
  void noOrder() {

    Oql.From<Object, Object> mockFrom = mock(Oql.From.class);

    OrderByClause<Object, Object> orderBy = OrderByClause.noOrder(mockFrom);

    assertThat(orderBy).isNotNull();
    assertThat(orderBy.getFrom()).isEqualTo(mockFrom);

    Comparator<Object> comparator = orderBy.getOrder();

    assertThat(comparator).isInstanceOf(OrderByClause.NoOrder.class);
    assertThat(comparator.compare("TEST", "MOCK")).isZero();
    assertThat(comparator.compare("MOCK", "TEST")).isZero();
    assertThat(comparator.compare("MOCK", null)).isZero();
    assertThat(comparator.compare(null, null)).isZero();
    assertThat(comparator.toString()).isEqualTo("No Order");

    verifyNoInteractions(mockFrom);
  }

  @Test
  @SuppressWarnings("unchecked")
  void of() {

    Oql.From<Object, Object> mockFrom = mock(Oql.From.class);
    Comparator<Object> mockComparator = mock(Comparator.class);

    OrderByClause<?, ?> orderBy = OrderByClause.of(mockFrom, mockComparator);

    assertThat(orderBy).isNotNull();
    assertThat(orderBy.getFrom()).isEqualTo(mockFrom);
    assertThat(orderBy.getOrder()).isEqualTo(mockComparator);

    verifyNoInteractions(mockFrom, mockComparator);
  }
}
