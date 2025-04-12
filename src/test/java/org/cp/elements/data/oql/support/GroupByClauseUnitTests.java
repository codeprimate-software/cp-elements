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
import java.util.function.BiPredicate;

import org.junit.jupiter.api.Test;

import org.cp.elements.data.oql.Oql;
import org.cp.elements.data.oql.QueryArguments;

/**
 * Unit Tests for {@link GroupByClause}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @since 3.0.0
 */
public class GroupByClauseUnitTests {

  @Test
  @SuppressWarnings("unchecked")
  void constructNewGroupByClause() {

    Oql.From<Object, Object> mockFrom = mock(Oql.From.class);
    Grouping<Object> mockGrouping = mock(Grouping.class);

    GroupByClause<? ,?> groupBy = new GroupByClause<>(mockFrom, mockGrouping);

    assertThat(groupBy).isNotNull();
    assertThat(groupBy.getFrom()).isEqualTo(mockFrom);
    assertThat(groupBy.getGrouping()).isEqualTo(mockGrouping);
    assertThat(groupBy.getPredicate()).isNotNull();

    verifyNoInteractions(mockFrom, mockGrouping);
  }

  @Test
  void constructNewGroupByWithNullFrom() {

    Grouping<?> mockGrouping = mock(Grouping.class);

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new GroupByClause<>(null, mockGrouping))
      .withMessage("From clause is required")
      .withNoCause();

    verifyNoInteractions(mockGrouping);
  }

  @Test
  void constructNewGroupByWithNullGrouping() {

    Oql.From<?, ?> mockFrom = mock(Oql.From.class);

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new GroupByClause<>(mockFrom, null))
      .withMessage("Grouping is required")
      .withNoCause();

    verifyNoInteractions(mockFrom);
  }

  @Test
  void copyExistingGroupBy() {

    Oql.From<?, ?> mockFrom = mock(Oql.From.class);
    Grouping<?> mockGrouping = mock(Grouping.class);
    BiPredicate<?, ?> mockPredicate = mock(BiPredicate.class);

    GroupByClause<?, ?> mockGroupBy = mock(GroupByClause.class);

    doReturn(mockFrom).when(mockGroupBy).getFrom();
    doReturn(mockGrouping).when(mockGroupBy).getGrouping();
    doReturn(mockPredicate).when(mockGroupBy).getPredicate();

    GroupByClause<?, ?> copy = GroupByClause.copy(mockGroupBy);

    assertThat(copy).isNotNull();
    assertThat(copy.getFrom()).isEqualTo(mockFrom);
    assertThat(copy.getGrouping()).isEqualTo(mockGrouping);
    assertThat(copy.getPredicate()).isEqualTo(mockPredicate);

    verify(mockGroupBy, times(1)).getFrom();
    verify(mockGroupBy, times(1)).getGrouping();
    verify(mockGroupBy, times(1)).getPredicate();
    verifyNoInteractions(mockFrom, mockGrouping, mockPredicate);
  }

  @Test
  void copyNullGroupBy() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> GroupByClause.copy(null))
      .withMessage("GroupBy clause is required")
      .withNoCause();
  }

  @Test
  @SuppressWarnings("unchecked")
  void ofGroupBy() {

    Oql.From<Object, Object> mockFrom = mock(Oql.From.class);
    Grouping<Object> mockGrouping = mock(Grouping.class);

    GroupByClause<?, ?> groupBy = GroupByClause.of(mockFrom, mockGrouping);

    assertThat(groupBy).isNotNull();
    assertThat(groupBy.getFrom()).isEqualTo(mockFrom);
    assertThat(groupBy.getGrouping()).isEqualTo(mockGrouping);
    assertThat(groupBy.getPredicate()).isNotNull();

    verifyNoInteractions(mockFrom, mockGrouping);
  }

  @Test
  @SuppressWarnings("unchecked")
  void havingPredicate() {

    Oql.From<Object, Object> mockFrom = mock(Oql.From.class);
    Grouping<Object> mockGrouping = mock(Grouping.class);
    BiPredicate<QueryArguments, Object> mockPredicate = mock(BiPredicate.class);

    GroupByClause<Object, Object> groupBy = GroupByClause.of(mockFrom, mockGrouping);

    assertThat(groupBy).isNotNull();
    assertThat(groupBy.getFrom()).isEqualTo(mockFrom);
    assertThat(groupBy.getGrouping()).isEqualTo(mockGrouping);
    assertThat(groupBy.getPredicate()).isNotNull();
    assertThat(groupBy.having(mockPredicate)).isSameAs(groupBy);
    assertThat(groupBy.getPredicate()).isEqualTo(mockPredicate);
    assertThat(groupBy.having((BiPredicate<QueryArguments, Object>) null)).isSameAs(groupBy);
    assertThat(groupBy.getPredicate()).isNotNull();

    verifyNoInteractions(mockFrom, mockGrouping);
  }

  @Test
  @SuppressWarnings("unchecked")
  void orderBy() {

    Oql.From<Object, Object> mockFrom = mock(Oql.From.class);
    Grouping<Object> mockGrouping = mock(Grouping.class);
    Comparator<Object> mockComparator = mock(Comparator.class);

    GroupByClause<Object, Object> groupBy = GroupByClause.of(mockFrom, mockGrouping);

    assertThat(groupBy).isNotNull();
    assertThat(groupBy.getFrom()).isEqualTo(mockFrom);
    assertThat(groupBy.getGrouping()).isEqualTo(mockGrouping);
    assertThat(groupBy.getPredicate()).isNotNull();

    Oql.OrderBy<Object, Object> orderBy = groupBy.orderBy(mockComparator);

    assertThat(orderBy).isNotNull();
    assertThat(orderBy.getFrom()).isEqualTo(mockFrom);
    assertThat(orderBy.getOrder()).isEqualTo(mockComparator);

    verifyNoInteractions(mockFrom, mockGrouping, mockComparator);
  }
}
