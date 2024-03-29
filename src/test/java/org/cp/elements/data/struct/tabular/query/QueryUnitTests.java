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
package org.cp.elements.data.struct.tabular.query;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.assertj.core.api.Assertions.assertThatIllegalStateException;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.function.Predicate;

import org.junit.jupiter.api.Test;

import org.cp.elements.data.struct.tabular.Column;
import org.cp.elements.data.struct.tabular.Row;
import org.cp.elements.data.struct.tabular.View;
import org.cp.elements.util.CollectionUtils;

/**
 * Unit Tests for {@link Query}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.struct.tabular.query.Query
 * @since 1.0.0
 */
public class QueryUnitTests {

  @Test
  public void querySelectArrayOfColumnsReturnsColumns() {

    Column<?> mockColumnOne = mock(Column.class);
    Column<?> mockColumnTwo = mock(Column.class);

    Query query = Query.select(mockColumnOne, mockColumnTwo);

    assertThat(query).isNotNull();
    assertThat(query.getProjection()).containsExactly(mockColumnOne, mockColumnTwo);

    verifyNoInteractions(mockColumnOne, mockColumnTwo);
  }

  @Test
  public void querySelectedColumnReturnsColumn() {

    Column<?> mockColumn = mock(Column.class);

    Query query = Query.select(mockColumn);

    assertThat(query).isNotNull();
    assertThat(query.getProjection()).containsExactly(mockColumn);

    verifyNoInteractions(mockColumn);
  }

  @Test
  public void querySelectIterableOfColumnsReturnsColumns() {

    Column<?> mockColumnOne = mock(Column.class);
    Column<?> mockColumnTwo = mock(Column.class);

    Query query = Query.select(CollectionUtils.asList(mockColumnOne, mockColumnTwo));

    assertThat(query).isNotNull();
    assertThat(query.getProjection()).containsExactly(mockColumnOne, mockColumnTwo);

    verifyNoInteractions(mockColumnOne, mockColumnTwo);
  }

  @Test
  public void querySingleColumnReturnsColumn() {

    Column<?> mockColumn = mock(Column.class);

    Query query = Query.select(Collections.singleton(mockColumn));

    assertThat(query).isNotNull();
    assertThat(query.getProjection()).containsExactly(mockColumn);

    verifyNoInteractions(mockColumn);
  }

  @Test
  public void queryWithEmptyArrayOfColumnsThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(Query::select)
      .withMessage("The projection must contain columns")
      .withNoCause();
  }

  @Test
  public void queryWithEmptyIterableOfColumnsThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Query.select(CollectionUtils.emptyIterable()))
      .withMessage("The projection must contain columns")
      .withNoCause();
  }

  @Test
  public void queryWithNullArrayOfColumnsThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Query.select((Column<?>[]) null))
      .withMessage("The projection must contain columns")
      .withNoCause();
  }

  @Test
  public void queryWithNullIterableOfColumnsThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Query.select((Iterable<Column<?>>) null))
      .withMessage("The projection must contain columns")
      .withNoCause();
  }

  @Test
  public void querySelectedColumnsFromView() {

    Column<?> mockColumnOne = mock(Column.class);
    Column<?> mockColumnTwo = mock(Column.class);

    View mockView = mock(View.class);

    Query query = Query.select(mockColumnOne, mockColumnTwo)
      .from(mockView);

    assertThat(query).isNotNull();
    assertThat(query.getSelection()).containsExactly(mockColumnOne, mockColumnTwo);
    assertThat(query.getFrom()).isEqualTo(mockView);
    assertThat(query.getPredicate()).isNotPresent();
    assertThat(query.getOrderBy()).isNotPresent();

    verifyNoInteractions(mockColumnOne, mockColumnTwo, mockView);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void querySelectedColumnsFromViewWherePredicateOrderedByComparator() {

    Column<?> mockColumn = mock(Column.class);

    Comparator<Row> mockComparator = mock(Comparator.class);

    Predicate<Row> mockPredicate = mock(Predicate.class);

    View mockView = mock(View.class);

    Query query = Query.select(mockColumn)
      .from(mockView)
      .where(mockPredicate)
      .orderBy(mockComparator);

    assertThat(query).isNotNull();
    assertThat(query.getSelection()).containsExactly(mockColumn);
    assertThat(query.getFrom()).isEqualTo(mockView);
    assertThat(query.getPredicate().orElse(null)).isEqualTo(mockPredicate);
    assertThat(query.getOrderBy().orElse(null)).isEqualTo(mockComparator);

    verifyNoInteractions(mockColumn, mockComparator, mockPredicate, mockView);
  }

  @Test
  public void querySelectedColumnsFromNoViewThrowsIllegalStateException() {

    Column<?> mockColumn = mock(Column.class);

    assertThatIllegalStateException()
      .isThrownBy(() -> Query.select(mockColumn).getFrom())
      .withMessage("From clause is required")
      .withNoCause();

    verifyNoInteractions(mockColumn);
  }

  @Test
  public void querySelectedColumnsWithNullViewThrowsIllegalArgumentException() {

    Column<?> mockColumn = mock(Column.class);

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Query.select(mockColumn).from(null))
      .withMessage("View is required")
      .withNoCause();

    verifyNoInteractions(mockColumn);
  }

  @Test
  public void queryGetSelectionCallsGetProjection() {

    Column<?> mockColumn = mock(Column.class);

    Query query = spy(Query.select(mockColumn));

    assertThat(query).isNotNull();
    assertThat(query.getSelection()).containsExactly(mockColumn);

    verify(query, times(1)).getSelection();
    verify(query, times(1)).getProjection();
    verifyNoMoreInteractions(query);
  }

  @Test
  public void queryExecuteReturnsAllRowsWithSelectedColumn() {

    Column<?> mockColumn = mock(Column.class);

    Row mockRowOne = mock(Row.class);
    Row mockRowTwo = mock(Row.class);

    View mockView = mock(View.class);

    doReturn(true).when(mockView).contains(any(Column.class));
    doReturn(Arrays.asList(mockRowOne, mockRowTwo)).when(mockView).rows();

    Query query = Query.select(mockColumn).from(mockView);

    assertThat(query).isNotNull();
    assertThat(query.getFrom()).isEqualTo(mockView);
    assertThat(query.getProjection()).containsExactly(mockColumn);
    assertThat(query.getPredicate()).isNotPresent();
    assertThat(query.getOrderBy()).isNotPresent();

    View resultSet = query.execute();

    assertThat(resultSet).isNotNull();
    assertThat(resultSet.columns()).containsExactly(mockColumn);
    assertThat(resultSet.rows()).containsExactly(mockRowOne, mockRowTwo);

    verify(mockView, times(1)).contains(eq(mockColumn));
    verify(mockView, times(1)).rows();
    verifyNoMoreInteractions(mockView);
    verifyNoInteractions(mockColumn);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void queryExecuteReturnsSelectRowsOrderedWithSelectedColumn() {

    Column<?> mockColumn = mock(Column.class);

    Comparator<Row> mockComparator = mock(Comparator.class);

    Predicate<Row> mockPredicate = mock(Predicate.class);

    Row mockRowOne = mock(Row.class);
    Row mockRowTwo = mock(Row.class);
    Row mockRowThree = mock(Row.class);

    View mockView = mock(View.class);

    doReturn(true).when(mockView).contains(any(Column.class));
    doReturn(Arrays.asList(mockRowOne, mockRowTwo, mockRowThree)).when(mockView).rows();
    doReturn(true).doReturn(false).doReturn(true)
      .when(mockPredicate).test(any(Row.class));
    doReturn(-1).when(mockComparator).compare(eq(mockRowOne), eq(mockRowThree));

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
    verifyNoInteractions(mockColumn);
  }

  @Test
  public void queryExecuteWithNullFromClauseThrowsIllegalStateException() {

    Column<?> mockColumn = mock(Column.class);

    assertThatIllegalStateException()
      .isThrownBy(() -> Query.select(mock(Column.class)).execute())
      .withMessage("From clause is required")
      .withNoCause();

    verifyNoInteractions(mockColumn);
  }

  @Test
  public void queryExecuteWithInvalidProjectionThrowsIllegalArgumentException() {

    Column<?> mockColumnOne = mock(Column.class);
    Column<?> mockColumnTwo = mock(Column.class);

    View mockFrom = mock(View.class);

    doReturn("MockColumnOne").when(mockColumnOne).toString();
    doReturn("MockColumnTwo").when(mockColumnTwo).toString();
    doReturn(Collections.singletonList(mockColumnOne)).when(mockFrom).columns();
    doReturn(true).when(mockFrom).contains(eq(mockColumnOne));
    doReturn(false).when(mockFrom).contains(eq(mockColumnTwo));

    Query query = Query.select(mockColumnOne, mockColumnTwo).from(mockFrom);

    assertThat(query).isNotNull();
    assertThat(query.getProjection()).containsExactly(mockColumnOne, mockColumnTwo);
    assertThat(query.getFrom()).isEqualTo(mockFrom);

    assertThatIllegalArgumentException()
      .isThrownBy(query::execute)
      .withMessage("The View of Columns [MockColumnOne] does not contain all the selected,"
        + " or projected Columns [MockColumnOne, MockColumnTwo]")
      .withNoCause();

    verify(mockFrom, times(1)).contains(eq(mockColumnOne));
    verify(mockFrom, times(1)).contains(eq(mockColumnTwo));
    verify(mockFrom, times(1)).columns();
    verifyNoInteractions(mockColumnOne, mockColumnTwo);
    verifyNoMoreInteractions(mockFrom);
  }

  @Test
  public void queryResultsCallsRun() {

    Column<?> mockColumn = mock(Column.class);

    Query query = spy(Query.select(mockColumn));

    doNothing().when(query).run();

    assertThat(query.results()).isNull();

    verify(query, times(1)).results();
    verify(query, times(1)).run();
    verifyNoMoreInteractions(query);
    verifyNoInteractions(mockColumn);
  }

  @Test
  public void queryRunCallsExecute() {

    Column<?> mockColumn = mock(Column.class);

    Query query = spy(Query.select(mockColumn));

    doReturn(null).when(query).execute();

    query.run();

    verify(query, times(1)).run();
    verify(query, times(1)).execute();
    verifyNoInteractions(mockColumn);
    verifyNoMoreInteractions(query);
  }

  @Test
  public void queryToString() {

    Column<?> mockColumn = mock(Column.class);

    View mockView = mock(View.class);

    doReturn("MockColumn").when(mockColumn).toString();
    doReturn("MockView").when(mockView).getName();

    Query query = Query.select(mockColumn).from(mockView);

    assertThat(query).isNotNull();
    assertThat(query.getProjection()).containsExactly(mockColumn);
    assertThat(query.getFrom()).isEqualTo(mockView);
    assertThat(query.getPredicate()).isNotPresent();
    assertThat(query.getOrderBy()).isNotPresent();
    assertThat(query.toString()).isEqualTo("SELECT [MockColumn] FROM MockView");

    verify(mockView, times(1)).getName();
    verifyNoMoreInteractions(mockView);
    verifyNoInteractions(mockColumn);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void queryWithOrderToString() {

    Column<?> mockColumn = mock(Column.class);

    Comparator<Row> mockComparator = mock(Comparator.class);

    View mockView = mock(View.class);

    doReturn("name").when(mockColumn).toString();
    doReturn("age DESC").when(mockComparator).toString();
    doReturn("People").when(mockView).getName();

    Query query = Query.select(mockColumn)
      .from(mockView)
      .orderBy(mockComparator);

    assertThat(query).isNotNull();
    assertThat(query.getProjection()).containsExactly(mockColumn);
    assertThat(query.getFrom()).isEqualTo(mockView);
    assertThat(query.getPredicate()).isNotPresent();
    assertThat(query.getOrderBy().orElse(null)).isEqualTo(mockComparator);
    assertThat(query.toString()).isEqualTo("SELECT [name] FROM People ORDER BY age DESC");

    verify(mockView, times(1)).getName();
    verifyNoInteractions(mockColumn, mockComparator);
    verifyNoMoreInteractions(mockView);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void queryWithPredicateToString() {

    Column<?> mockColumn = mock(Column.class);

    Predicate<Row> mockPredicate = mock(Predicate.class);

    View mockView = mock(View.class);

    doReturn("name").when(mockColumn).toString();
    doReturn("age >= 21").when(mockPredicate).toString();
    doReturn("People").when(mockView).getName();

    Query query = Query.select(mockColumn)
      .from(mockView)
      .where(mockPredicate);

    assertThat(query).isNotNull();
    assertThat(query.getProjection()).containsExactly(mockColumn);
    assertThat(query.getFrom()).isEqualTo(mockView);
    assertThat(query.getPredicate().orElse(null)).isEqualTo(mockPredicate);
    assertThat(query.getOrderBy()).isNotPresent();
    assertThat(query.toString()).isEqualTo("SELECT [name] FROM People WHERE age >= 21");

    verify(mockView, times(1)).getName();
    verifyNoInteractions(mockColumn, mockPredicate);
    verifyNoMoreInteractions(mockView);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void queryWithPredicateOrderedByToString() {

    Column<?> mockColumn = mock(Column.class);

    Comparator<Row> mockComparator = mock(Comparator.class);

    Predicate<Row> mockPredicate = mock(Predicate.class);

    View mockView = mock(View.class);

    doReturn("name").when(mockColumn).toString();
    doReturn("birthDate ASC").when(mockComparator).toString();
    doReturn("gender = FEMALE").when(mockPredicate).toString();
    doReturn("People").when(mockView).getName();

    Query query = Query.select(mockColumn)
      .from(mockView)
      .where(mockPredicate)
      .orderBy(mockComparator);

    assertThat(query).isNotNull();
    assertThat(query.getSelection()).containsExactly(mockColumn);
    assertThat(query.getFrom()).isEqualTo(mockView);
    assertThat(query.getPredicate().orElse(null)).isEqualTo(mockPredicate);
    assertThat(query.getOrderBy().orElse(null)).isEqualTo(mockComparator);
    assertThat(query.toString()).isEqualTo("SELECT [name] FROM People WHERE gender = FEMALE ORDER BY birthDate ASC");

    verify(mockView, times(1)).getName();
    verifyNoInteractions(mockColumn, mockComparator, mockPredicate);
    verifyNoMoreInteractions(mockView);
  }
}
