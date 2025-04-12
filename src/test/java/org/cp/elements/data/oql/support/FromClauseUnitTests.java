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
import static org.assertj.core.api.Assertions.assertThatIllegalStateException;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.stream.IntStream;

import org.junit.jupiter.api.Test;

import org.cp.elements.data.oql.Oql;
import org.cp.elements.security.model.User;

/**
 * Unit Tests for {@link FromClause}.
 *
 * @author John Blum
 * @see org.cp.elements.data.oql.support.FromClause
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @since 2.0.0
 */
class FromClauseUnitTests {

  @Test
  void fromCollection() {

    List<Number> list = List.of(1, 2, 3);

    FromClause.Builder<Number, Object> fromClauseBuilder = FromClause.collection(list);

    assertThat(fromClauseBuilder).isNotNull();
    assertThat(fromClauseBuilder.collection()).isEqualTo(list);

    FromClause<Number, Object> fromClause = fromClauseBuilder.build();

    assertThat(fromClause).isNotNull();
    assertThat(fromClause.getCollection()).containsExactlyElementsOf(list);
    assertThat(fromClause.getFrom()).isSameAs(fromClause);
    assertThat(fromClause.getType()).isEqualTo(Integer.class);
  }

  @Test
  @SuppressWarnings("all")
  void fromCollectionWithElementType() {

    List<Number> list = List.of(0, 1, 2);

    FromClause<Integer, Object> fromClause = FromClause.collection(list).of(Integer.class);

    assertThat(fromClause).isNotNull();
    assertThat(fromClause.getCollection()).containsExactly(list.toArray(new Integer[0]));
    assertThat(fromClause.getType()).isEqualTo(Integer.class);
  }

  @Test
  void fromNullCollection() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> FromClause.collection(null))
      .withMessage("Collection is required")
      .withNoCause();
  }

  @Test
  @SuppressWarnings({ "rawtypes", "unchecked" })
  void fromCollectionWithConfiguredSelection() {

    Iterable<Object> mockCollection = mock(Collection.class);

    SelectClause.ProjectionWrapper<User, Object> mockProjection = mock(SelectClause.ProjectionWrapper.class);
    Oql.Select<User, Object> mockSelect = mock(Oql.Select.class);

    doReturn(Object.class).when(mockProjection).getFromType();
    doReturn(mockProjection).when(mockSelect).getProjection();

    Oql.From<User, ?> from = FromClause.collection(mockCollection).of(User.class).withSelection(mockSelect).getFrom();

    assertThat(from).isInstanceOf(FromClause.class);
    assertThat(from.getType()).isEqualTo(User.class);

    verify(mockSelect, times(1)).getProjection();
    verify(mockProjection, times(1)).getFromType();
    verify(mockProjection, times(1)).usingFromType(eq(User.class));
    verifyNoMoreInteractions(mockSelect, mockProjection);
    verifyNoInteractions(mockCollection);
  }

  @Test
  void fromCollectionWithNoSelection() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> FromClause.collection(Collections.emptyList()).build().withSelection(null))
      .withMessage("Selection is required")
      .withNoCause();
  }

  @Test
  void fromCollectionWithNoSelectionThenGetSelection() {

    assertThatIllegalStateException()
      .isThrownBy(() -> FromClause.collection(Collections.emptyList()).build().getSelection())
      .withMessage("Selection was not initialized")
      .withNoCause();
  }

  @Test
  void fromCollectionWithConfiguredLimit() {

    Iterable<?> mockCollection = mock(Iterable.class);

    Oql.From<?, ?> fromClause = FromClause.collection(mockCollection).build().limit(2L).getFrom();

    assertThat(fromClause).isInstanceOf(FromClause.class);
    assertThat(fromClause.getLimit()).isEqualTo(2L);

    verifyNoInteractions(mockCollection);
  }

  @Test
  void fromCollectionWithZeroLimit() {

    IntStream.range(-2, 1).forEach(limit ->
        assertThatIllegalArgumentException()
          .isThrownBy(() -> FromClause.collection(Collections.emptySet()).build().limit(limit))
          .withMessage("Limit [%d] must be greater than 0", limit)
          .withNoCause());
  }
}
