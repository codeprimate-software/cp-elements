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
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Optional;
import java.util.function.BiPredicate;
import java.util.function.Predicate;

import org.junit.jupiter.api.Test;

import org.cp.elements.data.oql.Oql.Where;
import org.cp.elements.lang.Constants;
import org.cp.elements.security.model.User;

/**
 * Unit Tests for {@link Oql.From}
 *
 * @author John Blum
 * @see Oql.From
 * @see org.junit.jupiter.api.Test
 * @since 2.0.0
 */
@SuppressWarnings("unchecked")
class OqlFromUnitTests {

  @Test
  void fromClauseHasNoWhereClause() {

    Oql.From<Object, Object> from = mock(Oql.From.class);

    doCallRealMethod().when(from).getWhere();

    Optional<Where<Object, Object>> where = from.getWhere();

    assertThat(where).isNotNull();
    assertThat(where).isNotPresent();
  }

  @Test
  void fromDefaultNonNullEmptyCollection() {

    Oql.From<? ,?> mockFrom = mock(Oql.From.class);

    doCallRealMethod().when(mockFrom).getCollection();

    Iterable<?> collection = mockFrom.getCollection();

    assertThat(collection).isNotNull();
    assertThat(collection).isEmpty();

    verify(mockFrom, times(1)).getCollection();
    verifyNoMoreInteractions(mockFrom);
  }

  @Test
  void fromSelfReturnsSelf() {

    Oql.From<?, ?> mockFrom = mock(Oql.From.class);

    doCallRealMethod().when(mockFrom).getFrom();

    assertThat(mockFrom.getFrom()).isSameAs(mockFrom);

    verify(mockFrom, times(1)).getFrom();
    verifyNoMoreInteractions(mockFrom);
  }

  @Test
  void fromTypeReturnsSelectionProjectionFromType() {

    Oql.Projection<?, ?> projection = mock(Oql.Projection.class);
    Oql.Select<?, ?> select = mock(Oql.Select.class);
    Oql.From<? ,?> from = mock(Oql.From.class);

    doReturn(User.class).when(projection).getFromType();
    doReturn(projection).when(select).getProjection();
    doReturn(select).when(from).getSelection();
    doCallRealMethod().when(from).getType();

    assertThat(from.getType()).isEqualTo(User.class);

    verify(from, times(1)).getType();
    verify(from, times(1)).getSelection();
    verify(select, times(1)).getProjection();
    verify(projection, times(1)).getFromType();
    verifyNoMoreInteractions(projection, select, from);
  }

  @Test
  void fromWhereWithBiPredicateIsUnsupported() {

    BiPredicate<QueryArguments, Object> mockPredicate = mock(BiPredicate.class);
    Oql.From<Object, ?> mockFrom = mock(Oql.From.class);

    doCallRealMethod().when(mockFrom).where(any(BiPredicate.class));

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(() -> mockFrom.where(mockPredicate))
      .withMessage(Constants.UNSUPPORTED_OPERATION)
      .withNoCause();

    verify(mockFrom, times(1)).where(eq(mockPredicate));
    verifyNoMoreInteractions(mockPredicate);
    verifyNoInteractions(mockPredicate);
  }

  @Test
  void fromWhereWithPredicateIsUnsupported() {

    Predicate<Object> mockPredicate = mock(Predicate.class);
    Oql.From<Object, ?> mockFrom = mock(Oql.From.class);

    doCallRealMethod().when(mockFrom).where(any(BiPredicate.class));
    doCallRealMethod().when(mockFrom).where(any(Predicate.class));

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(() -> mockFrom.where(mockPredicate))
      .withMessage(Constants.UNSUPPORTED_OPERATION)
      .withNoCause();

    verify(mockFrom, times(1)).where(eq(mockPredicate));
    verify(mockFrom, times(1)).where(isA(BiPredicate.class));
    verifyNoMoreInteractions(mockPredicate);
    verifyNoInteractions(mockPredicate);
  }

  @Test
  void fromDefaultLimit() {

    Oql.From<Object, ?> mockFrom = mock(Oql.From.class);

    doCallRealMethod().when(mockFrom).limit(anyLong());

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(() -> mockFrom.limit(1))
      .withMessage(Constants.UNSUPPORTED_OPERATION)
      .withNoCause();

    verify(mockFrom, times(1)).limit(eq(1L));
    verifyNoMoreInteractions(mockFrom);
  }
}
