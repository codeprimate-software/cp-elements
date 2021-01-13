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
package org.cp.elements.data.struct.tabular;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.function.Predicate;

import org.cp.elements.data.struct.tabular.query.Query;
import org.junit.Test;
import org.mockito.ArgumentMatchers;

/**
 * Unit Tests for {@link View}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.struct.tabular.View
 * @since 1.0.0
 */
public class ViewTests {

  private Column<?> mockColumn(String name) {

    Column<?> mockColumn = mock(Column.class, name);

    when(mockColumn.getName()).thenReturn(name);

    return mockColumn;
  }

  @Test
  public void containsExistingColumnReturnsTrue() {

    Column<?> mockColumn = mock(Column.class);

    View mockView = mock(View.class);

    when(mockView.contains(anyString())).thenReturn(true);
    when(mockView.contains(any(Column.class))).thenCallRealMethod();
    when(mockColumn.getName()).thenReturn("MockColumn");

    assertThat(mockView.contains(mockColumn)).isTrue();

    verify(mockColumn, times(1)).getName();
    verify(mockView, times(1)).contains(eq("MockColumn"));
  }

  @Test
  public void containsNonExistingColumnReturnsFalse() {

    Column<?> mockColumn = mock(Column.class);

    View mockView = mock(View.class);

    when(mockView.contains(anyString())).thenReturn(false);
    when(mockView.contains(any(Column.class))).thenCallRealMethod();
    when(mockColumn.getName()).thenReturn("MockColumn");

    assertThat(mockView.contains(mockColumn)).isFalse();

    verify(mockColumn, times(1)).getName();
    verify(mockView, times(1)).contains(eq("MockColumn"));
  }

  @Test
  public void containsNullColumnReturnsFalse() {

    View mockView = mock(View.class);

    when(mockView.contains(any(Column.class))).thenCallRealMethod();

    assertThat(mockView.contains((Column<?>) null)).isFalse();

    verify(mockView, never()).contains(anyString());
  }

  @Test
  public void containsColumnNameReturnsTrue() {

    Column<?> mockColumnOne = mockColumn("One");
    Column<?> mockColumnTwo = mockColumn("Two");
    Column<?> mockColumnThree = mockColumn("Three");

    View mockView = mock(View.class);

    when(mockView.columns()).thenReturn(Arrays.asList(mockColumnOne, mockColumnTwo, mockColumnThree));
    when(mockView.contains(anyString())).thenCallRealMethod();

    assertThat(mockView.contains("One")).isTrue();
    assertThat(mockView.contains("Two")).isTrue();
    assertThat(mockView.contains("Three")).isTrue();

    verify(mockView, times(3)).columns();
    verify(mockColumnOne, times(3)).getName();
    verify(mockColumnTwo, times(2)).getName();
    verify(mockColumnThree, times(1)).getName();
  }

  @Test
  public void containsColumnNameReturnsFalse() {

    Column<?> mockColumnOne = mockColumn("One");
    Column<?> mockColumnTwo = mockColumn("Two");

    View mockView = mock(View.class);

    when(mockView.columns()).thenReturn(Arrays.asList(mockColumnOne, mockColumnTwo));
    when(mockView.contains(anyString())).thenCallRealMethod();

    assertThat(mockView.contains("negativeOne")).isFalse();
    assertThat(mockView.contains("neo")).isFalse();
    assertThat(mockView.contains("2")).isFalse();
    assertThat(mockView.contains("Three")).isFalse();

    verify(mockView, times(4)).columns();
    verify(mockColumnOne, times(4)).getName();
    verify(mockColumnTwo, times(4)).getName();
  }

  @Test
  public void containsBlankColumnNameReturnsFalse() {

    Column<?> mockColumnOne = mockColumn("One");
    Column<?> mockColumnTwo = mockColumn("Two");

    View mockView = mock(View.class);

    when(mockView.columns()).thenReturn(Arrays.asList(mockColumnOne, mockColumnTwo));
    when(mockView.contains(anyString())).thenCallRealMethod();

    assertThat(mockView.contains("  ")).isFalse();

    verify(mockView, never()).columns();
    verify(mockColumnOne, never()).getName();
    verify(mockColumnTwo, never()).getName();
  }

  @Test
  public void containsEmptyColumnNameReturnsFalse() {

    Column<?> mockColumnOne = mockColumn("One");
    Column<?> mockColumnTwo = mockColumn("Two");

    View mockView = mock(View.class);

    when(mockView.columns()).thenReturn(Arrays.asList(mockColumnOne, mockColumnTwo));
    when(mockView.contains(anyString())).thenCallRealMethod();

    assertThat(mockView.contains("")).isFalse();

    verify(mockView, never()).columns();
    verify(mockColumnOne, never()).getName();
    verify(mockColumnTwo, never()).getName();
  }

  @Test
  public void containsNullColumnNameReturnsFalse() {

    Column<?> mockColumnOne = mockColumn("One");
    Column<?> mockColumnTwo = mockColumn("Two");

    View mockView = mock(View.class);

    when(mockView.columns()).thenReturn(Arrays.asList(mockColumnOne, mockColumnTwo));
    when(mockView.contains(any(String.class))).thenCallRealMethod();

    assertThat(mockView.contains((String) null)).isFalse();

    verify(mockView, never()).columns();
    verify(mockColumnOne, never()).getName();
    verify(mockColumnTwo, never()).getName();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void countReturnsZero() {

    Row mockRowOne = mock(Row.class);
    Row mockRowTwo = mock(Row.class);

    View mockView = mock(View.class);

    when(mockView.rows()).thenReturn(Arrays.asList(mockRowOne, mockRowTwo));
    when(mockView.count(any(Predicate.class))).thenCallRealMethod();

    assertThat(mockView.count(row -> false)).isEqualTo(0);

    verify(mockView, times(1)).rows();
    verifyNoInteractions(mockRowOne);
    verifyNoInteractions(mockRowTwo);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void countReturnsSize() {

    Row mockRowOne = mock(Row.class);
    Row mockRowTwo = mock(Row.class);

    View mockView = mock(View.class);

    when(mockView.rows()).thenReturn(Arrays.asList(mockRowOne, mockRowTwo));
    when(mockView.count(any(Predicate.class))).thenCallRealMethod();

    assertThat(mockView.count(row -> true)).isEqualTo(2);

    verify(mockView, times(1)).rows();
    verifyNoInteractions(mockRowOne);
    verifyNoInteractions(mockRowTwo);
  }

  @Test
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public void countReturnsNumberOfMatchingRows() {

    Predicate mockPredicate = mock(Predicate.class);

    Row mockRowOne = mock(Row.class);
    Row mockRowTwo = mock(Row.class);
    Row mockRowThree = mock(Row.class);

    View mockView = mock(View.class);

    when(mockView.rows()).thenReturn(Arrays.asList(mockRowOne, mockRowTwo, mockRowThree));
    when(mockView.count(any(Predicate.class))).thenCallRealMethod();

    when(mockPredicate.test(any(Row.class))).thenAnswer(invocation ->
      Arrays.asList(mockRowOne, mockRowTwo).contains(invocation.<Row>getArgument(0)));

    assertThat(mockView.count(mockPredicate)).isEqualTo(2);

    verify(mockView, times(1)).rows();
    verify(mockPredicate, times(1)).test(eq(mockRowOne));
    verify(mockPredicate, times(1)).test(eq(mockRowTwo));
    verify(mockPredicate, times(1)).test(eq(mockRowThree));
    verifyNoInteractions(mockRowOne);
    verifyNoInteractions(mockRowTwo);
    verifyNoInteractions(mockRowThree);
  }

  @Test(expected = IllegalArgumentException.class)
  public void countWithNullPredicateThrowsIllegalArgumentException() {

    View mockView = mock(View.class);

    when(mockView.count(any())).thenCallRealMethod();

    try {
      mockView.count(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Predicate is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally  {
      verify(mockView, never()).rows();
    }
  }

  @Test
  public void getColumnWithIndexReturnsColumn() {

    Column<?> mockColumnOne = mockColumn("One");
    Column<?> mockColumnTwo = mockColumn("Two");

    View mockView = mock(View.class);

    when(mockView.columns()).thenReturn(Arrays.asList(mockColumnOne, mockColumnTwo));
    when(mockView.getColumn(anyInt())).thenCallRealMethod();

    assertThat(mockView.getColumn(0)).isEqualTo(mockColumnOne);
    assertThat(mockView.getColumn(1)).isEqualTo(mockColumnTwo);

    verify(mockView, times(2)).columns();
    verifyNoInteractions(mockColumnOne);
    verifyNoInteractions(mockColumnTwo);
  }

  @Test(expected = IllegalArgumentException.class)
  public void getColumnWithNegativeIndexThrowsException() {

    View mockView = mock(View.class);

    when(mockView.getColumn(anyInt())).thenCallRealMethod();

    try {
      mockView.getColumn(-1);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Index [-1] is not valid");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockView, never()).columns();
    }
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void getColumnWithOutOfBoundsIndexThrowsException() {

    Column<?> mockColumnOne = mockColumn("One");
    Column<?> mockColumnTwo = mockColumn("Two");

    View mockView = mock(View.class);

    when(mockView.columns()).thenReturn(Arrays.asList(mockColumnOne, mockColumnTwo));
    when(mockView.getColumn(anyInt())).thenCallRealMethod();

    try {
      mockView.getColumn(2);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Index [-1] is not valid");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockView, times(1)).columns();
      verifyNoInteractions(mockColumnOne);
      verifyNoInteractions(mockColumnTwo);
    }
  }

  @Test
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public void getColumnWithNameReturnsColumn() {

    Column mockColumnOne = mockColumn("One");
    Column mockColumnTwo = mockColumn("Two");

    View mockView = mock(View.class);

    when(mockView.getColumn(eq(0))).thenReturn(mockColumnOne);
    when(mockView.getColumn(eq(1))).thenReturn(mockColumnTwo);
    when(mockView.getColumn(anyString())).thenCallRealMethod();
    when(mockView.indexOf(eq("One"))).thenReturn(0);
    when(mockView.indexOf(eq("Two"))).thenReturn(1);

    assertThat(mockView.getColumn("One").orElse(null)).isEqualTo(mockColumnOne);
    assertThat(mockView.getColumn("Two").orElse(null)).isEqualTo(mockColumnTwo);

    verify(mockView, times(1)).getColumn(eq(0));
    verify(mockView, times(1)).getColumn(eq(1));
    verify(mockView, times(1)).indexOf(eq("One"));
    verify(mockView, times(1)).indexOf(eq("Two"));
  }

  @Test
  public void getColumnWithNonExistingNameReturnsEmptyOptional() {

    View mockView = mock(View.class);

    when(mockView.indexOf(anyString())).thenReturn(-1);
    when(mockView.getColumn(anyString())).thenCallRealMethod();

    assertThat(mockView.getColumn("test").orElse(null)).isNull();

    verify(mockView, times(1)).indexOf(eq("test"));
    verify(mockView, never()).getColumn(anyInt());
  }

  @Test
  public void getRowWithIndexReturnsRow() {

    Row mockRowOne = mock(Row.class);
    Row mockRowTwo = mock(Row.class);

    List<Row> rows = Arrays.asList(mockRowOne, mockRowTwo);

    View mockView = mock(View.class);

    when(mockView.rows()).thenReturn(rows);
    when(mockView.size()).thenReturn(rows.size());
    when(mockView.getRow(anyInt())).thenCallRealMethod();

    assertThat(mockView.getRow(0)).isEqualTo(mockRowOne);
    assertThat(mockView.getRow(1)).isEqualTo(mockRowTwo);

    verify(mockView, times(2)).rows();
    verifyNoInteractions(mockRowOne);
    verifyNoInteractions(mockRowTwo);
  }

  @Test(expected = IllegalArgumentException.class)
  public void getRowWithNegativeIndexThrowsException() {

    View mockView = mock(View.class);

    when(mockView.getRow(anyInt())).thenCallRealMethod();
    when(mockView.size()).thenReturn(10);

    try {
      mockView.getRow(-1);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Index [-1] is not valid; Index must be greater than -1 and less than 10");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockView, never()).rows();
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void getRowWithOutOfBoundsIndexThrowsException() {

    View mockView = mock(View.class);

    when(mockView.getRow(anyInt())).thenCallRealMethod();
    when(mockView.size()).thenReturn(10);

    try {
      mockView.getRow(10);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Index [10] is not valid; Index must be greater than -1 and less than 10");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockView, never()).rows();
    }
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void getRowWithOverflowIndexThrowsException() {

    Row mockRowOne = mock(Row.class);
    Row mockRowTwo = mock(Row.class);

    View mockView = mock(View.class);

    when(mockView.rows()).thenReturn(Arrays.asList(mockRowOne, mockRowTwo));
    when(mockView.getRow(anyInt())).thenCallRealMethod();
    when(mockView.size()).thenReturn(10);

    try {
      mockView.getRow(2);
    }
    catch (IndexOutOfBoundsException expected) {

      assertThat(expected).hasMessage("Index [2] is greater than the number of rows [2]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockView, times(1)).rows();
      verifyNoInteractions(mockRowOne);
      verifyNoInteractions(mockRowTwo);
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void getRowWithPredicateReturnsRow() {

    Row mockRowOne = mock(Row.class);
    Row mockRowTwo = mock(Row.class);

    View mockView = mock(View.class);

    when(mockView.rows()).thenReturn(Arrays.asList(mockRowOne, mockRowTwo));
    when(mockView.getRow(any(Predicate.class))).thenCallRealMethod();

    assertThat(mockView.getRow(row -> row.equals(mockRowOne)).orElse(null)).isEqualTo(mockRowOne);
    assertThat(mockView.getRow(row -> row.equals(mockRowTwo)).orElse(null)).isEqualTo(mockRowTwo);

    verify(mockView, times(2)).rows();
    verifyNoInteractions(mockRowOne);
    verifyNoInteractions(mockRowTwo);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void getRowWithPredicateReturnsEmptyOptional() {

    Row mockRowOne = mock(Row.class);
    Row mockRowTwo = mock(Row.class);

    View mockView = mock(View.class);

    when(mockView.rows()).thenReturn(Arrays.asList(mockRowOne, mockRowTwo));
    when(mockView.getRow(any(Predicate.class))).thenCallRealMethod();

    assertThat(mockView.getRow(row -> false).orElse(null)).isNull();

    verify(mockView, times(1)).rows();
    verifyNoInteractions(mockRowOne);
    verifyNoInteractions(mockRowTwo);
  }

  @Test(expected = IllegalArgumentException.class)
  public void getRowWithNullPredicateThrowsException() {

    View mockView = mock(View.class);

    when(mockView.getRow(any())).thenCallRealMethod();

    try {
      mockView.getRow(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Predicate is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockView, never()).rows();
    }
  }

  @Test
  public void getValueFromRowIndexColumnIndexReturnsValue() {

    Row mockRow = mock(Row.class);

    View mockView = mock(View.class);

    when(mockRow.getValue(anyInt())).thenReturn("test");
    when(mockView.getRow(anyInt())).thenReturn(mockRow);
    when(mockView.getValue(anyInt(), anyInt())).thenCallRealMethod();

    assertThat(mockView.<Object>getValue(0, 2)).isEqualTo("test");

    verify(mockRow, times(1)).getValue(eq(2));
    verify(mockView, times(1)).getRow(eq(0));
  }

  @Test
  public void getValueFromRowIndexColumnNameReturnsValue() {

    View mockView = mock(View.class);

    when(mockView.getValue(anyInt(), anyInt())).thenReturn("test");
    when(mockView.getValue(anyInt(), anyString())).thenCallRealMethod();
    when(mockView.indexOf(anyString())).thenReturn(2);

    assertThat(mockView.<Object>getValue(0, "MockColumn")).isEqualTo("test");

    verify(mockView, times(1)).getValue(eq(0), eq(2));
    verify(mockView, times(1)).indexOf(eq("MockColumn"));
  }

  @Test
  public void getValueFromRowIndexAndColumnReturnsValue() {

    Column<?> mockColumn = mockColumn("One");

    View mockView = mock(View.class);

    when(mockView.getValue(anyInt(), anyInt())).thenReturn("test");
    when(mockView.getValue(anyInt(), any(Column.class))).thenCallRealMethod();
    when(mockView.indexOf(any(Column.class))).thenReturn(2);

    assertThat(mockView.<Object>getValue(0, mockColumn)).isEqualTo("test");

    verify(mockView, times(1)).getValue(eq(0), eq(2));
    verify(mockView, times(1)).indexOf(eq(mockColumn));
  }

  @Test
  public void indexOfColumnReturnsIndex() {

    Column<?> mockColumn = mock(Column.class);

    View mockView = mock(View.class);

    when(mockColumn.getName()).thenReturn("TwentyTwo");
    when(mockView.indexOf(anyString())).thenReturn(21);
    when(mockView.indexOf(any(Column.class))).thenCallRealMethod();

    assertThat(mockView.indexOf(mockColumn)).isEqualTo(21);

    verify(mockColumn, times(1)).getName();
    verify(mockView, times(1)).indexOf(eq("TwentyTwo"));
  }

  @Test
  public void indexOfNonExistingColumnReturnsMinusOne() {

    Column<?> mockColumn = mock(Column.class);

    View mockView = mock(View.class);

    when(mockColumn.getName()).thenReturn("One");
    when(mockView.indexOf(anyString())).thenReturn(-1);
    when(mockView.indexOf(any(Column.class))).thenCallRealMethod();

    assertThat(mockView.indexOf(mockColumn)).isEqualTo(-1);

    verify(mockColumn, times(1)).getName();
    verify(mockView, times(1)).indexOf(eq("One"));
  }

  @Test
  public void indexOfNullColumnReturnsMinusOne() {

    View mockView = mock(View.class);

    when(mockView.indexOf(ArgumentMatchers.<Column<?>>any())).thenCallRealMethod();

    assertThat(mockView.indexOf((Column<?>) null)).isEqualTo(-1);

    verify(mockView, never()).indexOf(anyString());
  }

  @Test
  public void indexOfColumnNameReturnsIndex() {

    Column<?> mockColumnOne = mockColumn("One");
    Column<?> mockColumnTwo = mockColumn("Two");

    View mockView = mock(View.class);

    when(mockView.columns()).thenReturn(Arrays.asList(mockColumnOne, mockColumnTwo));
    when(mockView.indexOf(anyString())).thenCallRealMethod();

    assertThat(mockView.indexOf("One")).isEqualTo(0);
    assertThat(mockView.indexOf("Two")).isEqualTo(1);

    verify(mockView, times(2)).columns();
    verify(mockColumnOne, times(2)).getName();
    verify(mockColumnTwo, times(1)).getName();
  }

  @Test
  public void indexOfNonExistingColumnNameReturnsMinusOne() {

    Column<?> mockColumnOne = mockColumn("One");
    Column<?> mockColumnTwo = mockColumn("Two");

    View mockView = mock(View.class);

    when(mockView.columns()).thenReturn(Arrays.asList(mockColumnOne, mockColumnTwo));
    when(mockView.indexOf(anyString())).thenCallRealMethod();

    assertThat(mockView.indexOf("Neo")).isEqualTo(-1);
    assertThat(mockView.indexOf("2")).isEqualTo(-1);
    assertThat(mockView.indexOf("Three")).isEqualTo(-1);

    verify(mockView, times(3)).columns();
    verify(mockColumnOne, times(3)).getName();
    verify(mockColumnTwo, times(3)).getName();
  }

  @Test
  public void indexOfNullColumnNameReturnsMinusOne() {

    Column<?> mockColumnOne = mockColumn("One");
    Column<?> mockColumnTwo = mockColumn("Two");

    View mockView = mock(View.class);

    when(mockView.columns()).thenReturn(Arrays.asList(mockColumnOne, mockColumnTwo));
    when(mockView.indexOf(ArgumentMatchers.<String>any())).thenCallRealMethod();

    assertThat(mockView.indexOf((String) null)).isEqualTo(-1);

    verify(mockView, times(1)).columns();
    verify(mockColumnOne, times(1)).getName();
    verify(mockColumnTwo, times(1)).getName();
  }

  @Test
  @SuppressWarnings("all")
  public void indexOfRowReturnsIndex() {

    Row mockRowOne = mock(Row.class);
    Row mockRowTwo = mock(Row.class);

    View mockView = mock(View.class);

    when(mockView.rows()).thenReturn(Arrays.asList(mockRowOne, mockRowTwo));
    when(mockView.indexOf(any(Row.class))).thenCallRealMethod();

    assertThat(mockView.indexOf(mockRowOne)).isEqualTo(0);
    assertThat(mockView.indexOf(mockRowTwo)).isEqualTo(1);

    verify(mockView, times(2)).rows();
  }

  @Test
  @SuppressWarnings("all")
  public void indexOfNonExitingRowReturnsMinusOne() {

    Row mockRowOne = mock(Row.class);
    Row mockRowTwo = mock(Row.class);

    View mockView = mock(View.class);

    when(mockView.rows()).thenReturn(Collections.singletonList(mockRowOne));
    when(mockView.indexOf(any(Row.class))).thenCallRealMethod();

    assertThat(mockView.indexOf(mockRowTwo)).isEqualTo(-1);

    verify(mockView, times(1)).rows();
  }

  @Test
  public void indexOfNullRowReturnsMinusOne() {

    View mockView = mock(View.class);

    when(mockView.indexOf(ArgumentMatchers.<Row>any())).thenCallRealMethod();

    assertThat(mockView.indexOf((Row) null)).isEqualTo(-1);

    verify(mockView, times(1)).rows();
  }

  @Test
  public void isEmptyReturnsTrue() {

    View mockView = mock(View.class);

    when(mockView.size()).thenReturn(0);
    when(mockView.isEmpty()).thenCallRealMethod();

    assertThat(mockView.isEmpty()).isTrue();

    verify(mockView, times(1)).size();
  }

  @Test
  public void isEmptyReturnsFalse() {

    View mockView = mock(View.class);

    when(mockView.size()).thenReturn(1);
    when(mockView.isEmpty()).thenCallRealMethod();

    assertThat(mockView.isEmpty()).isFalse();

    verify(mockView, times(1)).size();
  }

  @Test
  public void queryReturnsResults() {

    Column<?> mockColumnOne = mockColumn("One");
    Column<?> mockColumnTwo = mockColumn("Two");

    View mockViewOne = mock(View.class);
    View mockViewTwo = mock(View.class);

    Query query = spy(Query.select(mockColumnOne, mockColumnTwo)
      .from(mockViewOne));

    when(mockViewOne.query(any(Query.class))).thenCallRealMethod();
    doReturn(mockViewTwo).when(query).execute();

    assertThat(mockViewOne.query(query)).isEqualTo(mockViewTwo);

    verify(query, times(1)).execute();
  }

  @Test(expected = IllegalArgumentException.class)
  public void queryWithNullQueryThrowsIllegalArgumenException() {

    View mockView = mock(View.class);

    when(mockView.query(any())).thenCallRealMethod();

    try {
      mockView.query(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Query is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void rowsReturnsThis() {

    View mockView = mock(View.class);

    when(mockView.rows()).thenCallRealMethod();

    assertThat(mockView.rows()).isSameAs(mockView);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void sizeReturnsCount() {

    View mockView = mock(View.class);

    when(mockView.count(any(Predicate.class))).thenReturn(2);
    when(mockView.size()).thenCallRealMethod();

    assertThat(mockView.size()).isEqualTo(2);

    verify(mockView, times(1)).count(isA(Predicate.class));
  }
}
