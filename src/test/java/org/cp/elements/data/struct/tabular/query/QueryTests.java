/*
 * Copyright 2016 Author or Authors.
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

package org.cp.elements.data.struct.tabular.query;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.function.Predicate;

import org.cp.elements.data.struct.tabular.Column;
import org.cp.elements.data.struct.tabular.Row;
import org.cp.elements.data.struct.tabular.View;
import org.cp.elements.util.CollectionUtils;
import org.junit.Test;

/**
 * Unit tests for {@link Query}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.struct.tabular.query.Query
 * @since 1.0.0
 */
public class QueryTests {

  @Test
  public void querySelectArrayOfColumnsReturnsColumns() {

    Column mockColumnOne = mock(Column.class);
    Column mockColumnTwo = mock(Column.class);

    Query query = Query.select(mockColumnOne, mockColumnTwo);

    assertThat(query).isNotNull();
    assertThat(query.getProjection()).containsExactly(mockColumnOne, mockColumnTwo);
  }

  @Test
  public void querySelectedColumnReturnsColumn() {

    Column mockColumn = mock(Column.class);

    Query query = Query.select(mockColumn);

    assertThat(query).isNotNull();
    assertThat(query.getProjection()).containsExactly(mockColumn);
  }

  @Test
  public void querySelectIterableOfColumnsReturnsColumns() {

    Column mockColumnOne = mock(Column.class);
    Column mockColumnTwo = mock(Column.class);

    Query query = Query.select(CollectionUtils.asSet(mockColumnOne, mockColumnTwo));

    assertThat(query).isNotNull();
    assertThat(query.getProjection()).containsExactlyInAnyOrder(mockColumnOne, mockColumnTwo);
  }

  @Test(expected = IllegalArgumentException.class)
  public void queryWithNoColumnsThrowsIllegalArgumentException() {

    try {
      Query.select();
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("The projection must contain columns");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void queryWithNullArrayOfColumnsThrowsIllegalArgumentException() {

    try {
      Query.select((Column[]) null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("The projection must contain columns");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void queryWithNullIterableOfColumnsThrowsIllegalArgumentException() {

    try {
      Query.select((Iterable<Column>) null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("The projection must contain columns");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void querySelectedColumnsFromView() {

    Column mockColumnOne = mock(Column.class);
    Column mockColumnTwo = mock(Column.class);

    View mockView = mock(View.class);

    Query query = Query.select(mockColumnOne, mockColumnTwo)
      .from(mockView);

    assertThat(query).isNotNull();
    assertThat(query.getProjection()).containsExactly(mockColumnOne, mockColumnTwo);
    assertThat(query.getFrom()).isEqualTo(mockView);
    assertThat(query.getPredicate().orElse(null)).isNull();
    assertThat(query.getOrderBy().orElse(null)).isNull();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void querySelectedColumnsFromViewWherePredicateOrderedByComparator() {

    Column mockColumn = mock(Column.class);

    Comparator<Row> mockComparator = mock(Comparator.class);

    Predicate<Row> mockPredicate = mock(Predicate.class);

    View mockView = mock(View.class);

    Query query = Query.select(mockColumn)
      .from(mockView)
      .where(mockPredicate)
      .orderBy(mockComparator);

    assertThat(query).isNotNull();
    assertThat(query.getProjection()).containsExactly(mockColumn);
    assertThat(query.getFrom()).isEqualTo(mockView);
    assertThat(query.getPredicate().orElse(null)).isEqualTo(mockPredicate);
    assertThat(query.getOrderBy().orElse(null)).isEqualTo(mockComparator);
  }

  @Test(expected = IllegalStateException.class)
  public void querySelectedColumnsFromNullViewThrowsIllegalStateException() {

    try {
      Query.select(mock(Column.class)).getFrom();
    }
    catch (IllegalStateException expected) {

      assertThat(expected).hasMessage("From clause is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void querySelectedColumnsWithNullViewThrowsIllegalArgumentException() {

    try {
      Query.select(mock(Column.class)).from(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("View is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void queryExecuteReturnsAllRowsWithSelectedColumn() {

    Column mockColumn = mock(Column.class);

    Row mockRowOne = mock(Row.class);
    Row mockRowTwo = mock(Row.class);

    View mockView = mock(View.class);

    when(mockView.contains(any(Column.class))).thenReturn(true);
    when(mockView.rows()).thenReturn(Arrays.asList(mockRowOne, mockRowTwo));

    Query query = Query.select(mockColumn)
      .from(mockView);

    assertThat(query).isNotNull();
    assertThat(query.getFrom()).isEqualTo(mockView);
    assertThat(query.getProjection()).containsExactly(mockColumn);
    assertThat(query.getPredicate().orElse(null)).isNull();
    assertThat(query.getOrderBy().orElse(null)).isNull();

    View resultSet = query.execute();

    assertThat(resultSet).isNotNull();
    assertThat(resultSet.columns()).containsExactly(mockColumn);
    assertThat(resultSet.rows()).containsExactly(mockRowOne, mockRowTwo);

    verify(mockView, times(1)).contains(eq(mockColumn));
    verify(mockView, times(1)).rows();
    verifyNoMoreInteractions(mockView);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void queryExecuteReturnsSelectRowsWithSelectedColumn() {

    Column mockColumn = mock(Column.class);

    Comparator<Row> mockComparator = mock(Comparator.class);

    Predicate<Row> mockPredicate = mock(Predicate.class);

    Row mockRowOne = mock(Row.class);
    Row mockRowTwo = mock(Row.class);
    Row mockRowThree = mock(Row.class);

    View mockView = mock(View.class);

    when(mockView.contains(any(Column.class))).thenReturn(true);
    when(mockView.rows()).thenReturn(Arrays.asList(mockRowOne, mockRowTwo, mockRowThree));
    when(mockPredicate.test(any(Row.class))).thenReturn(true).thenReturn(false).thenReturn(true);
    when(mockComparator.compare(eq(mockRowOne), eq(mockRowThree))).thenReturn(-1);

    Query query = Query.select(mockColumn)
      .from(mockView)
      .where(mockPredicate)
      .orderBy(mockComparator);

    assertThat(query).isNotNull();
    assertThat(query.getFrom()).isEqualTo(mockView);
    assertThat(query.getProjection()).containsExactly(mockColumn);
    assertThat(query.getPredicate().orElse(null)).isEqualTo(mockPredicate);
    assertThat(query.getOrderBy().orElse(null)).isEqualTo(mockComparator);

    View resultSet = query.execute();

    assertThat(resultSet).isNotNull();
    assertThat(resultSet.columns()).containsExactly(mockColumn);
    assertThat(resultSet.rows()).containsExactly(mockRowOne, mockRowThree);

    verify(mockView, times(1)).contains(eq(mockColumn));
    verify(mockView, times(1)).rows();
    verify(mockPredicate, times(3)).test(isA(Row.class));
    verify(mockPredicate, times(1)).test(eq(mockRowOne));
    verify(mockPredicate, times(1)).test(eq(mockRowTwo));
    verify(mockPredicate, times(1)).test(eq(mockRowThree));
    verify(mockComparator, times(1)).compare(isA(Row.class), isA(Row.class));
    verifyNoMoreInteractions(mockView);
  }

  @Test(expected = IllegalStateException.class)
  public void queryExecuteWithNullFromClauseThrowsIllegalStateException() {

    try {
      Query.select(mock(Column.class)).execute();
    }
    catch (IllegalStateException expected) {

      assertThat(expected).hasMessage("From clause is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void queryExecuteWithInvalidProjectionThrowsIllegalArgumentException() {

    Column mockColumnOne = mock(Column.class);
    Column mockColumnTwo = mock(Column.class);

    View mockFrom = mock(View.class);

    when(mockColumnOne.toString()).thenReturn("MockColumnOne");
    when(mockColumnTwo.toString()).thenReturn("MockColumnTwo");
    when(mockFrom.columns()).thenReturn(Collections.singletonList(mockColumnOne));
    when(mockFrom.contains(eq(mockColumnOne))).thenReturn(true);
    when(mockFrom.contains(eq(mockColumnTwo))).thenReturn(false);

    Query query = Query.select(mockColumnOne, mockColumnTwo)
      .from(mockFrom);

    assertThat(query).isNotNull();
    assertThat(query.getProjection()).containsExactly(mockColumnOne, mockColumnTwo);
    assertThat(query.getFrom()).isEqualTo(mockFrom);

    try {
      query.execute();
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected)
        .hasMessage("The View of Columns [MockColumnOne] does not contain all the selected, or projected Columns [MockColumnOne, MockColumnTwo]");

      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockFrom, times(1)).contains(eq(mockColumnOne));
      verify(mockFrom, times(1)).contains(eq(mockColumnTwo));
      verify(mockFrom, times(1)).columns();
    }
  }

  @Test
  public void queryResultsCallsRun() {

    Query query = spy(Query.select(mock(Column.class)));

    doNothing().when(query).run();

    assertThat(query.results()).isNull();

    verify(query, times(1)).run();
  }

  @Test
  public void queryRunCallsExecute() {

    Query query = spy(Query.select(mock(Column.class)));

    doReturn(null).when(query).execute();

    query.run();

    verify(query, times(1)).execute();
  }

  @Test
  public void queryToString() {

    Column mockColumn = mock(Column.class);

    View mockView = mock(View.class);

    when(mockColumn.toString()).thenReturn("MockColumn");
    when(mockView.getName()).thenReturn("MockView");

    Query query = Query.select(mockColumn).from(mockView);

    assertThat(query).isNotNull();
    assertThat(query.getProjection()).containsExactly(mockColumn);
    assertThat(query.getFrom()).isEqualTo(mockView);
    assertThat(query.getPredicate().orElse(null)).isNull();
    assertThat(query.getOrderBy().orElse(null)).isNull();
    assertThat(query.toString()).isEqualTo("SELECT [MockColumn] FROM MockView");
  }

  @Test
  @SuppressWarnings("unchecked")
  public void queryWithOrderToString() {

    Column mockColumn = mock(Column.class);

    Comparator mockComparator = mock(Comparator.class);

    View mockView = mock(View.class);

    when(mockColumn.toString()).thenReturn("name");
    when(mockComparator.toString()).thenReturn("age DESC");
    when(mockView.getName()).thenReturn("People");

    Query query = Query.select(mockColumn)
      .from(mockView)
      .orderBy(mockComparator);

    assertThat(query).isNotNull();
    assertThat(query.getProjection()).containsExactly(mockColumn);
    assertThat(query.getFrom()).isEqualTo(mockView);
    assertThat(query.getPredicate().orElse(null)).isNull();
    assertThat(query.getOrderBy().orElse(null)).isEqualTo(mockComparator);
    assertThat(query.toString()).isEqualTo("SELECT [name] FROM People ORDER BY age DESC");
  }

  @Test
  @SuppressWarnings("unchecked")
  public void queryWithPredicateToString() {

    Column mockColumn = mock(Column.class);

    Predicate mockPredicate = mock(Predicate.class);

    View mockView = mock(View.class);

    when(mockColumn.toString()).thenReturn("name");
    when(mockPredicate.toString()).thenReturn("age >= 21");
    when(mockView.getName()).thenReturn("People");

    Query query = Query.select(mockColumn)
      .from(mockView)
      .where(mockPredicate);

    assertThat(query).isNotNull();
    assertThat(query.getProjection()).containsExactly(mockColumn);
    assertThat(query.getFrom()).isEqualTo(mockView);
    assertThat(query.getPredicate().orElse(null)).isEqualTo(mockPredicate);
    assertThat(query.getOrderBy().orElse(null)).isNull();
    assertThat(query.toString()).isEqualTo("SELECT [name] FROM People WHERE age >= 21");
  }

  @Test
  @SuppressWarnings("unchecked")
  public void queryWithPredicateOrderedByToString() {

    Column mockColumn = mock(Column.class);

    Comparator mockComparator = mock(Comparator.class);

    Predicate mockPredicate = mock(Predicate.class);

    View mockView = mock(View.class);

    when(mockColumn.toString()).thenReturn("name");
    when(mockComparator.toString()).thenReturn("birthDate ASC");
    when(mockPredicate.toString()).thenReturn("gender = FEMALE");
    when(mockView.getName()).thenReturn("People");

    Query query = Query.select(mockColumn)
      .from(mockView)
      .where(mockPredicate)
      .orderBy(mockComparator);

    assertThat(query).isNotNull();
    assertThat(query.getProjection()).containsExactly(mockColumn);
    assertThat(query.getFrom()).isEqualTo(mockView);
    assertThat(query.getPredicate().orElse(null)).isEqualTo(mockPredicate);
    assertThat(query.getOrderBy().orElse(null)).isEqualTo(mockComparator);
    assertThat(query.toString()).isEqualTo("SELECT [name] FROM People WHERE gender = FEMALE ORDER BY birthDate ASC");
  }
}
