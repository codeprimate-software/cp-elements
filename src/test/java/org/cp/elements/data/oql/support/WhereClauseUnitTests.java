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
package org.cp.elements.data.oql.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.function.BiPredicate;

import org.junit.jupiter.api.Test;

import org.cp.elements.data.oql.Oql.Where;
import org.cp.elements.data.oql.QueryArguments;

/**
 * Unit Tests for {@link WhereClause}.
 *
 * @author John Blum
 * @see org.cp.elements.data.oql.support.WhereClause
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @since 2.0.0
 */
@SuppressWarnings("unchecked")
class WhereClauseUnitTests {

  @Test
  void copyWhere() {

    BiPredicate<QueryArguments, Object> mockPredicate = mock(BiPredicate.class);
    FromClause<Object, Object> mockFrom = mock(FromClause.class);
    Where<Object, Object> mockWhere = mock(Where.class);

    doReturn(mockFrom).when(mockWhere).getFrom();
    doReturn(mockPredicate).when(mockWhere).getPredicate();

    Where<Object, Object> where = WhereClause.copy(mockWhere);

    assertThat(where).isNotNull();
    assertThat(where.getFrom()).isEqualTo(mockFrom);
    assertThat(where.getPredicate()).isEqualTo(mockPredicate);

    verify(mockWhere, times(1)).getFrom();
    verify(mockWhere, times(1)).getPredicate();
    verify(mockFrom, times(1)).withWhere(eq(where));
    verifyNoMoreInteractions(mockFrom, mockWhere);
    verifyNoMoreInteractions(mockPredicate);
  }
}
