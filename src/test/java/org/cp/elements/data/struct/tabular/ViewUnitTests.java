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
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.assertj.core.api.Assertions.assertThatIndexOutOfBoundsException;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.ArgumentMatchers.isNotNull;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.function.Predicate;

import org.junit.jupiter.api.Test;

import org.cp.elements.data.struct.tabular.query.Query;

import org.mockito.ArgumentMatchers;

/**
 * Unit Tests for {@link View}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.struct.tabular.View
 * @since 1.0.0
 */
public class ViewUnitTests {

  private Column<?> mockColumn(String name) {

    Column<?> mockColumn = mock(Column.class, name);

    doReturn(name).when(mockColumn).getName();

    return mockColumn;
  }

  private Row mockRow(String name) {
    return mock(Row.class, name);
  }

  private View mockView(String name) {

    View mockView = mock(View.class, name);

    doReturn(name).when(mockView).getName();

    return mockView;
  }

  @Test
  public void containsExistingColumnReturnsTrue() {

    Column<?> mockColumn = mockColumn("MockColumn");

    View mockView = mock(View.class);

    doReturn(true).when(mockView).contains(anyString());
    doCallRealMethod().when(mockView).contains(any(Column.class));

    assertThat(mockView.contains(mockColumn)).isTrue();

    verify(mockColumn, times(1)).getName();
    verify(mockView, times(1)).contains(eq(mockColumn));
    verify(mockView, times(1)).contains(eq("MockColumn"));
    verifyNoMoreInteractions(mockColumn, mockView);
  }

  @Test
  public void containsNonExistingColumnReturnsFalse() {

    Column<?> mockColumn = mockColumn("MockColumn");

    View mockView = mock(View.class);

    doReturn(false).when(mockView).contains(anyString());
    doCallRealMethod().when(mockView).contains(any(Column.class));

    assertThat(mockView.contains(mockColumn)).isFalse();

    verify(mockColumn, times(1)).getName();
    verify(mockView, times(1)).contains(eq(mockColumn));
    verify(mockView, times(1)).contains(eq("MockColumn"));
    verifyNoMoreInteractions(mockColumn, mockView);
  }

  @Test
  public void containsNullColumnIsNullSafeReturnsFalse() {

    View mockView = mock(View.class);

    doCallRealMethod().when(mockView).contains(any(Column.class));

    assertThat(mockView.contains((Column<?>) null)).isFalse();

    verify(mockView, times(1)).contains(ArgumentMatchers.<Column<?>>isNull());
    verify(mockView, never()).contains(anyString());
    verifyNoMoreInteractions(mockView);
  }

  @Test
  public void containsColumnWithNameReturnsTrue() {

    Column<?> mockColumnOne = mockColumn("A");
    Column<?> mockColumnTwo = mockColumn("B");
    Column<?> mockColumnThree = mockColumn("C");

    View mockView = mock(View.class);

    doReturn(Arrays.asList(mockColumnOne, mockColumnTwo, mockColumnThree)).when(mockView).columns();
    doCallRealMethod().when(mockView).contains(anyString());

    assertThat(mockView.contains("A")).isTrue();
    assertThat(mockView.contains("B")).isTrue();
    assertThat(mockView.contains("C")).isTrue();

    verify(mockView, times(1)).contains(eq("A"));
    verify(mockView, times(1)).contains(eq("B"));
    verify(mockView, times(1)).contains(eq("C"));
    verify(mockView, times(3)).columns();
    verify(mockColumnOne, times(3)).getName();
    verify(mockColumnTwo, times(2)).getName();
    verify(mockColumnThree, times(1)).getName();
    verifyNoMoreInteractions(mockView, mockColumnOne, mockColumnTwo, mockColumnThree);
  }

  @Test
  public void containsColumnWithNonExistingNameReturnsFalse() {

    Column<?> mockColumnOne = mockColumn("A");
    Column<?> mockColumnTwo = mockColumn("B");

    View mockView = mock(View.class);

    doReturn(Arrays.asList(mockColumnOne, mockColumnTwo)).when(mockView).columns();
    doCallRealMethod().when(mockView).contains(anyString());

    List<String> columnNames = Arrays.asList("X", "a", "BB", "Bee", "one", "2");

    columnNames.forEach(columnName -> {
      assertThat(mockView.contains(columnName)).isFalse();
      verify(mockView, times(1)).contains(eq(columnName));
    });

    verify(mockView, times(6)).columns();
    verify(mockColumnOne, times(6)).getName();
    verify(mockColumnTwo, times(6)).getName();
    verifyNoMoreInteractions(mockView, mockColumnOne, mockColumnTwo);
  }

  @Test
  public void containsColumnWithInvalidNameIsSafeReturnsFalse() {

    Column<?> mockColumnOne = mockColumn("A");
    Column<?> mockColumnTwo = mockColumn("B");

    View mockView = mock(View.class);

    doReturn(Arrays.asList(mockColumnOne, mockColumnTwo)).when(mockView).columns();
    doCallRealMethod().when(mockView).contains(any(String.class));

    Arrays.asList("  ", "", null).forEach(columnName -> {
      assertThat(mockView.contains(columnName)).isFalse();
      verify(mockView, times(1)).contains(eq(columnName));
    });

    verify(mockView, never()).columns();
    verifyNoMoreInteractions(mockView);
    verifyNoInteractions(mockColumnOne, mockColumnTwo);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void countReturnsZero() {

    Row mockRowOne = mockRow("ONE");
    Row mockRowTwo = mockRow("TWO");

    View mockView = mock(View.class);

    doReturn(Arrays.asList(mockRowOne, mockRowTwo)).when(mockView).rows();
    doCallRealMethod().when(mockView).count(any(Predicate.class));

    assertThat(mockView.count(row -> false)).isZero();

    verify(mockView, times(1)).count(isA(Predicate.class));
    verify(mockView, times(1)).rows();
    verifyNoInteractions(mockRowOne, mockRowTwo);
    verifyNoMoreInteractions(mockView);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void countReturnsSize() {

    Row mockRowOne = mockRow("ONE");
    Row mockRowTwo = mockRow("TWO");

    View mockView = mock(View.class);

    doReturn(Arrays.asList(mockRowOne, mockRowTwo)).when(mockView).rows();
    doCallRealMethod().when(mockView).count(any(Predicate.class));

    assertThat(mockView.count(row -> true)).isEqualTo(2);

    verify(mockView, times(1)).count(isA(Predicate.class));
    verify(mockView, times(1)).rows();
    verifyNoInteractions(mockRowOne, mockRowTwo);
    verifyNoMoreInteractions(mockView);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void countReturnsOne() {

    Row mockRowOne = mockRow("ONE");
    Row mockRowTwo = mockRow("TWO");

    View mockView = mock(View.class);

    doReturn(Arrays.asList(mockRowOne, mockRowTwo)).when(mockView).rows();
    doCallRealMethod().when(mockView).count(any(Predicate.class));

    assertThat(mockView.count(mockRowOne::equals)).isOne();

    verify(mockView, times(1)).count(isA(Predicate.class));
    verify(mockView, times(1)).rows();
    verifyNoInteractions(mockRowOne, mockRowTwo);
    verifyNoMoreInteractions(mockView);
  }

  @Test
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public void countReturnsNumberOfMatchingRows() {

    Predicate mockPredicate = mock(Predicate.class);

    Row mockRowOne = mockRow("ONE");
    Row mockRowTwo = mockRow("TWO");
    Row mockRowThree = mockRow("THREE");

    View mockView = mock(View.class);

    doAnswer(invocation -> Arrays.asList(mockRowOne, mockRowTwo).contains(invocation.<Row>getArgument(0)))
      .when(mockPredicate).test(any(Row.class));

    doReturn(Arrays.asList(mockRowOne, mockRowTwo, mockRowThree)).when(mockView).rows();
    doCallRealMethod().when(mockView).count(any(Predicate.class));

    assertThat(mockView.count(mockPredicate)).isEqualTo(2);

    verify(mockView, times(1)).count(eq(mockPredicate));
    verify(mockView, times(1)).rows();
    verify(mockPredicate, times(1)).test(eq(mockRowOne));
    verify(mockPredicate, times(1)).test(eq(mockRowTwo));
    verify(mockPredicate, times(1)).test(eq(mockRowThree));
    verifyNoInteractions(mockRowOne, mockRowTwo, mockRowThree);
    verifyNoMoreInteractions(mockView);
  }

  @Test
  public void countWithNullPredicateThrowsIllegalArgumentException() {

    View mockView = mock(View.class);

    doCallRealMethod().when(mockView).count(any());

    assertThatIllegalArgumentException()
      .isThrownBy(() -> mockView.count(null))
      .withMessage("Predicate is required")
      .withNoCause();

    verify(mockView, times(1)).count(isNull());
    verify(mockView, never()).rows();
    verifyNoMoreInteractions(mockView);
  }

  @Test
  public void getColumnAtIndexReturnsColumn() {

    Column<?> mockColumnOne = mockColumn("A");
    Column<?> mockColumnTwo = mockColumn("B");

    View mockView = mock(View.class);

    doReturn(Arrays.asList(mockColumnOne, mockColumnTwo)).when(mockView).columns();
    doCallRealMethod().when(mockView).getColumn(anyInt());

    assertThat(mockView.getColumn(0)).isEqualTo(mockColumnOne);
    assertThat(mockView.getColumn(1)).isEqualTo(mockColumnTwo);
    assertThat(mockView.getColumn(0)).isEqualTo(mockColumnOne);

    verify(mockView, times(2)).getColumn(eq(0));
    verify(mockView, times(1)).getColumn(eq(1));
    verify(mockView, times(3)).columns();
    verifyNoInteractions(mockColumnOne, mockColumnTwo);
    verifyNoMoreInteractions(mockView);
  }

  @Test
  public void getColumnAtNegativeIndexThrowsException() {

    View mockView = mock(View.class);

    doCallRealMethod().when(mockView).getColumn(anyInt());

    assertThatIllegalArgumentException()
      .isThrownBy(() -> mockView.getColumn(-1))
      .withMessage("Column index [-1] is not valid")
      .withNoCause();

    verify(mockView, times(1)).getColumn(eq(-1));
    verify(mockView, never()).columns();
    verifyNoMoreInteractions(mockView);
  }

  @Test
  public void getColumnAtOutOfBoundsIndexThrowsException() {

    Column<?> mockColumnOne = mockColumn("A");
    Column<?> mockColumnTwo = mockColumn("B");

    View mockView = mockView("MockView");

    doReturn(Arrays.asList(mockColumnOne, mockColumnTwo)).when(mockView).columns();
    doCallRealMethod().when(mockView).getColumn(anyInt());

    assertThatIndexOutOfBoundsException()
      .isThrownBy(() -> mockView.getColumn(2))
      .withMessage("Index [2] is greater than the number of columns [2] in this View [MockView]")
      .withNoCause();

    verify(mockView, times(1)).getColumn(eq(2));
    verify(mockView, times(1)).columns();
    verify(mockView, times(1)).getName();
    verifyNoInteractions(mockColumnOne, mockColumnTwo);
    verifyNoMoreInteractions(mockView);
  }

  @Test
  public void getColumnWithNameReturnsColumn() {

    Column<?> mockColumnOne = mockColumn("A");
    Column<?> mockColumnTwo = mockColumn("B");

    View mockView = mock(View.class);

    doReturn(Arrays.asList(mockColumnOne, mockColumnTwo)).when(mockView).columns();
    doCallRealMethod().when(mockView).getColumn(anyString());

    assertThat(mockView.getColumn("A").orElse(null)).isEqualTo(mockColumnOne);
    assertThat(mockView.getColumn("B").orElse(null)).isEqualTo(mockColumnTwo);
    assertThat(mockView.getColumn("A").orElse(null)).isEqualTo(mockColumnOne);

    verify(mockView, times(2)).getColumn(eq("A"));
    verify(mockView, times(1)).getColumn(eq("B"));
    verify(mockView, times(3)).columns();
    verify(mockColumnOne, times(3)).getName();
    verify(mockColumnTwo, times(1)).getName();
    verifyNoMoreInteractions(mockView, mockColumnOne, mockColumnTwo);
  }

  @Test
  public void getColumnWithInvalidNamesIsSafeReturnsEmptyOptional() {

    Column<?> mockColumn = mockColumn("MockColumn");

    View mockView = mock(View.class);

    doReturn(Collections.singletonList(mockColumn)).when(mockView).columns();
    doCallRealMethod().when(mockView).getColumn(any());

    Arrays.asList("  ", "", null).forEach(columnName -> {
      assertThat(mockView.getColumn(columnName)).isNotPresent();
      verify(mockView, times(1)).getColumn(eq(columnName));
    });

    verify(mockView, times(3)).columns();
    verify(mockColumn, times(3)).getName();
    verifyNoMoreInteractions(mockView, mockColumn);
  }

  @Test
  public void getColumnWithNonExistingNameReturnsEmptyOptional() {

    Column<?> mockColumn = mockColumn("MockColumn");

    View mockView = mock(View.class);

    doReturn(Collections.singletonList(mockColumn)).when(mockView).columns();
    doCallRealMethod().when(mockView).getColumn(anyString());

    assertThat(mockView.getColumn("TestColumn")).isNotPresent();

    verify(mockView, times(1)).getColumn(eq("TestColumn"));
    verify(mockView, times(1)).columns();
    verify(mockColumn, times(1)).getName();
    verifyNoMoreInteractions(mockView, mockColumn);
  }

  @Test
  public void getRowAtIndexReturnsRow() {

    Row mockRowOne = mockRow("ONE");
    Row mockRowTwo = mockRow("TWO");

    List<Row> rows = Arrays.asList(mockRowOne, mockRowTwo);

    View mockView = mock(View.class);

    doReturn(rows).when(mockView).rows();
    doReturn(rows.size()).when(mockView).size();
    doCallRealMethod().when(mockView).getRow(anyInt());

    assertThat(mockView.getRow(0)).isEqualTo(mockRowOne);
    assertThat(mockView.getRow(1)).isEqualTo(mockRowTwo);
    assertThat(mockView.getRow(0)).isEqualTo(mockRowOne);

    verify(mockView, times(2)).getRow(eq(0));
    verify(mockView, times(1)).getRow(eq(1));
    verify(mockView, times(3)).rows();
    verify(mockView, times(3)).size();
    verifyNoInteractions(mockRowOne, mockRowTwo);
    verifyNoMoreInteractions(mockView);
  }

  @Test
  public void getRowAtNegativeIndexThrowsException() {

    Row mockRow = mockRow("ONE");

    View mockView = mock(View.class);

    doReturn(Collections.singletonList(mockRow)).when(mockView).rows();
    doCallRealMethod().when(mockView).getRow(anyInt());
    doReturn(100).when(mockView).size();

    assertThatIllegalArgumentException()
      .isThrownBy(() -> mockView.getRow(-1))
      .withMessage("Row index [-1] is not valid; Row index must be greater than [-1] and less than [100]")
      .withNoCause();

    verify(mockView, times(1)).getRow(eq(-1));
    verify(mockView, times(1)).size();
    verifyNoMoreInteractions(mockView);
    verifyNoInteractions(mockRow);
  }

  @Test
  public void getRowAtOutOfBoundsIndexThrowsException() {

    Row mockRow = mockRow("ONE");

    View mockView = mock(View.class);

    doReturn(Collections.singletonList(mockRow)).when(mockView).rows();
    doReturn(1).when(mockView).size();
    doCallRealMethod().when(mockView).getRow(anyInt());

    assertThatIllegalArgumentException()
      .isThrownBy(() -> mockView.getRow(1))
      .withMessage("Row index [1] is not valid; Row index must be greater than [-1] and less than [1]")
      .withNoCause();

    verify(mockView, times(1)).getRow(eq(1));
    verify(mockView, never()).rows();
    verify(mockView, times(1)).size();
    verifyNoMoreInteractions(mockView);
    verifyNoInteractions(mockRow);
  }

  @Test
  public void getRowAtOverflowIndexThrowsException() {

    Row mockRowOne = mockRow("ONE");
    Row mockRowTwo = mockRow("TWO");

    View mockView = mockView("MockView");

    doReturn(Arrays.asList(mockRowOne, mockRowTwo)).when(mockView).rows();
    doReturn(100).when(mockView).size();
    doCallRealMethod().when(mockView).getRow(anyInt());

    assertThatIndexOutOfBoundsException()
      .isThrownBy(() -> mockView.getRow(2))
      .withMessage("Row index [2] is greater than the number of rows [2] in this View [MockView]")
      .withNoCause();

    verify(mockView, times(1)).getRow(eq(2));
    verify(mockView, times(1)).rows();
    verify(mockView, times(1)).size();
    verify(mockView, times(1)).getName();
    verifyNoInteractions(mockRowOne, mockRowTwo);
    verifyNoMoreInteractions(mockView);
  }

  @Test
  public void getRowWithPredicateReturnsRow() {

    Row mockRowOne = mockRow("ONE");
    Row mockRowTwo = mockRow("TWO");

    View mockView = mock(View.class);

    doReturn(Arrays.asList(mockRowOne, mockRowTwo)).when(mockView).rows();
    doCallRealMethod().when(mockView).getRow(any());

    assertThat(mockView.getRow(mockRowOne::equals).orElse(null)).isEqualTo(mockRowOne);
    assertThat(mockView.getRow(mockRowTwo::equals).orElse(null)).isEqualTo(mockRowTwo);

    verify(mockView, times(2)).getRow(isNotNull());
    verify(mockView, times(2)).rows();
    verifyNoInteractions(mockRowOne, mockRowTwo);
  }

  @Test
  public void getRowWithPredicateReturnsEmptyOptional() {

    Row mockRowOne = mockRow("ONE");
    Row mockRowTwo = mockRow("TWO");

    View mockView = mock(View.class);

    doReturn(Arrays.asList(mockRowOne, mockRowTwo)).when(mockView).rows();
    doCallRealMethod().when(mockView).getRow(any());

    assertThat(mockView.getRow(row -> false)).isNotPresent();

    verify(mockView, times(1)).getRow(isNotNull());
    verify(mockView, times(1)).rows();
    verifyNoInteractions(mockRowOne, mockRowTwo);
  }

  @Test
  public void getRowWithNullPredicateIsNullSafeReturnsEmptyOptional() {

    Row mockRowOne = mockRow("ONE");
    Row mockRowTwo = mockRow("TWO");

    View mockView = mock(View.class);

    doReturn(Arrays.asList(mockRowOne, mockRowTwo)).when(mockView).rows();
    doCallRealMethod().when(mockView).getRow(any());

    assertThat(mockView.getRow(null)).isNotPresent();

    verify(mockView, times(1)).getRow(isNull());
    verify(mockView, times(1)).rows();
    verifyNoInteractions(mockRowOne, mockRowTwo);
  }

  @Test
  public void getValueFromRowIndexAndColumnIndexReturnsValue() {

    Row mockRow = mock(Row.class);

    View mockView = mock(View.class);

    doCallRealMethod().when(mockView).getValue(anyInt(), anyInt());
    doReturn(mockRow).when(mockView).getRow(anyInt());
    doReturn("test").when(mockRow).getValue(anyInt());

    assertThat(mockView.<Object>getValue(0, 2)).isEqualTo("test");

    verify(mockView, times(1)).getValue(eq(0), eq(2));
    verify(mockView, times(1)).getRow(eq(0));
    verify(mockRow, times(1)).getValue(eq(2));
    verifyNoMoreInteractions(mockView, mockRow);
  }

  @Test
  public void getValueFromRowIndexAndColumnNameReturnsValue() {

    View mockView = mock(View.class);

    doCallRealMethod().when(mockView).getValue(anyInt(), anyString());
    doReturn(2).when(mockView).indexOf(anyString());
    doReturn("test").when(mockView).getValue(anyInt(), anyInt());

    assertThat(mockView.<Object>getValue(1, "MockColumn")).isEqualTo("test");

    verify(mockView, times(1)).getValue(eq(1), eq("MockColumn"));
    verify(mockView, times(1)).indexOf(eq("MockColumn"));
    verify(mockView, times(1)).getValue(eq(1), eq(2));
    verifyNoMoreInteractions(mockView);
  }

  @Test
  public void getValueFromRowIndexAndInvalidColumnNameThrowsException() {

    View mockView = mockView("MockView");

    doCallRealMethod().when(mockView).getValue(anyInt(), anyString());
    doReturn(-1).when(mockView).indexOf(anyString());

    assertThatIllegalArgumentException()
      .isThrownBy(() ->  mockView.getValue(1, "InvalidColumnName"))
      .withMessage("Column with name [InvalidColumnName] does not exist in this View [MockView]")
      .withNoCause();

    verify(mockView, times(1)).getValue(eq(1), eq("InvalidColumnName"));
    verify(mockView, times(1)).indexOf(eq("InvalidColumnName"));
    verify(mockView, times(1)).getName();
    verify(mockView, never()).getValue(anyInt(), anyInt());
    verifyNoMoreInteractions(mockView);
  }

  @Test
  public void getValueFromRowIndexAndColumnReturnsValue() {

    Column<?> mockColumn = mock(Column.class);

    View mockView = mock(View.class);

    doCallRealMethod().when(mockView).getValue(anyInt(), any(Column.class));
    doReturn(2).when(mockView).indexOf(eq(mockColumn));
    doReturn("test").when(mockView).getValue(anyInt(), anyInt());

    assertThat(mockView.<Object>getValue(2, mockColumn)).isEqualTo("test");

    verify(mockView, times(1)).getValue(eq(2), eq( mockColumn));
    verify(mockView, times(1)).indexOf(eq(mockColumn));
    verify(mockView, times(1)).getValue(eq(2), eq(2));
    verifyNoMoreInteractions(mockView);
    verifyNoInteractions(mockColumn);
  }

  @Test
  public void getValueFromRowIndexAndInvalidColumnThrowsException() {

    Column<?> mockColumn = mock(Column.class);

    View mockView = mockView("MockView");

    doCallRealMethod().when(mockView).getValue(anyInt(), any(Column.class));
    doReturn(-1).when(mockView).indexOf(eq(mockColumn));

    assertThatIllegalArgumentException()
      .isThrownBy(() -> mockView.getValue(4, mockColumn))
      .withMessage("Column [%s] does not exist in this View [MockView]", mockColumn)
      .withNoCause();

    verify(mockView, times(1)).getValue(eq(4), eq(mockColumn));
    verify(mockView, times(1)).indexOf(eq(mockColumn));
    verify(mockView, times(1)).getName();
    verifyNoMoreInteractions(mockView);
    verifyNoInteractions(mockColumn);
  }

  @Test
  public void indexOfColumnReturnsIndex() {

    Column<?> mockColumn = mockColumn("MockColumn");

    View mockView = mock(View.class);

    doCallRealMethod().when(mockView).indexOf(eq(mockColumn));
    doReturn(4).when(mockView).indexOf(eq("MockColumn"));

    assertThat(mockView.indexOf(mockColumn)).isEqualTo(4);

    verify(mockView, times(1)).indexOf(eq(mockColumn));
    verify(mockView, times(1)).indexOf(eq("MockColumn"));
    verify(mockColumn, times(1)).getName();
    verifyNoMoreInteractions(mockView, mockColumn);
  }

  @Test
  public void indexOfNonExistingColumnReturnsMinusOne() {

    Column<?> mockColumn = mockColumn("NonExistingColumn");

    View mockView = mock(View.class);

    doCallRealMethod().when(mockView).indexOf(any(Column.class));
    doReturn(-1).when(mockView).indexOf(eq("NonExistingColumn"));

    assertThat(mockView.indexOf(mockColumn)).isEqualTo(-1);

    verify(mockView, times(1)).indexOf(eq(mockColumn));
    verify(mockView, times(1)).indexOf(eq("NonExistingColumn"));
    verify(mockColumn, times(1)).getName();
    verifyNoMoreInteractions(mockView, mockColumn);
  }

  @Test
  public void indexOfNullColumnIsNullSafeReturnsMinusOne() {

    View mockView = mock(View.class);

    doCallRealMethod().when(mockView).indexOf(ArgumentMatchers.<Column<?>>any());

    assertThat(mockView.indexOf((Column<?>) null)).isEqualTo(-1);

    verify(mockView, times(1)).indexOf(ArgumentMatchers.<Column<?>>isNull());
    verify(mockView, never()).indexOf(anyString());
    verifyNoMoreInteractions(mockView);
  }

  @Test
  public void indexOfColumnNameReturnsIndex() {

    Column<?> mockColumnOne = mockColumn("A");
    Column<?> mockColumnTwo = mockColumn("B");
    Column<?> mockColumnThree = mockColumn("C");

    View mockView = mock(View.class);

    doCallRealMethod().when(mockView).indexOf(anyString());
    doReturn(Arrays.asList(mockColumnOne, mockColumnTwo, mockColumnThree)).when(mockView).columns();

    assertThat(mockView.indexOf("A")).isZero();
    assertThat(mockView.indexOf("B")).isOne();
    assertThat(mockView.indexOf("C")).isEqualTo(2);
    assertThat(mockView.indexOf("B")).isOne();

    verify(mockView, times(4)).indexOf(anyString());
    verify(mockView, times(4)).columns();
    verify(mockColumnOne, times(4)).getName();
    verify(mockColumnTwo, times(3)).getName();
    verify(mockColumnThree, times(1)).getName();
    verifyNoMoreInteractions(mockView, mockColumnOne, mockColumnTwo, mockColumnThree);
  }

  @Test
  public void indexOfColumnNameWithNoColumnsIsSafeReturnsMinusOne() {

    View mockView = mock(View.class);

    doCallRealMethod().when(mockView).indexOf(anyString());
    doReturn(Collections.emptyList()).when(mockView).columns();

    assertThat(mockView.indexOf("MockColumn")).isEqualTo(-1);

    verify(mockView, times(1)).indexOf(eq("MockColumn"));
    verify(mockView, times(1)).columns();
    verifyNoMoreInteractions(mockView);
  }

  @Test
  public void indexOfNonExistingColumnNameReturnsMinusOne() {

    Column<?> mockColumnOne = mockColumn("A");
    Column<?> mockColumnTwo = mockColumn("B");

    View mockView = mock(View.class);

    doCallRealMethod().when(mockView).indexOf(anyString());
    doReturn(Arrays.asList(mockColumnOne, mockColumnTwo)).when(mockView).columns();

    assertThat(mockView.indexOf("a")).isEqualTo(-1);
    assertThat(mockView.indexOf("BB")).isEqualTo(-1);
    assertThat(mockView.indexOf("Bee")).isEqualTo(-1);
    assertThat(mockView.indexOf("2")).isEqualTo(-1);

    verify(mockView, times(4)).indexOf(anyString());
    verify(mockView, times(4)).columns();
    verify(mockColumnOne, times(4)).getName();
    verify(mockColumnTwo, times(4)).getName();
    verifyNoMoreInteractions(mockView, mockColumnOne, mockColumnTwo);
  }

  @Test
  public void indexOfNullColumnNameIsNullSafeReturnsMinusOne() {

    Column<?> mockColumnOne = mockColumn("A");
    Column<?> mockColumnTwo = mockColumn("B");

    View mockView = mock(View.class);

    doCallRealMethod().when(mockView).indexOf(ArgumentMatchers.<String>isNull());
    doReturn(Arrays.asList(mockColumnOne, mockColumnTwo)).when(mockView).columns();

    assertThat(mockView.indexOf((String) null)).isEqualTo(-1);

    verify(mockView, times(1)).indexOf(ArgumentMatchers.<String>isNull());
    verify(mockView, times(1)).columns();
    verify(mockColumnOne, times(1)).getName();
    verify(mockColumnTwo, times(1)).getName();
    verifyNoMoreInteractions(mockView, mockColumnOne, mockColumnTwo);
  }

  @Test
  @SuppressWarnings("all")
  public void indexOfRowReturnsIndex() {

    Row mockRowOne = mock(Row.class);
    Row mockRowTwo = mock(Row.class);

    View mockView = mock(View.class);

    doCallRealMethod().when(mockView).indexOf(any(Row.class));
    doReturn(Arrays.asList(mockRowOne, mockRowTwo)).when(mockView).rows();

    assertThat(mockView.indexOf(mockRowOne)).isEqualTo(0);
    assertThat(mockView.indexOf(mockRowTwo)).isEqualTo(1);
    assertThat(mockView.indexOf(mockRowOne)).isEqualTo(0);

    verify(mockView, times(2)).indexOf(eq(mockRowOne));
    verify(mockView, times(1)).indexOf(eq(mockRowTwo));
    verify(mockView, times(3)).rows();
    verifyNoInteractions(mockRowOne, mockRowTwo);
    verifyNoMoreInteractions(mockView);
  }

  @Test
  public void indexOfRowWithNoRowsIsSafeReturnsMinusOne() {

    Row mockRow = mock(Row.class);

    View mockView = mock(View.class);

    doCallRealMethod().when(mockView).indexOf(any(Row.class));
    doReturn(Collections.emptyList()).when(mockView).rows();

    assertThat(mockView.indexOf(mockRow)).isEqualTo(-1);

    verify(mockView, times(1)).indexOf(eq(mockRow));
    verify(mockView, times(1)).rows();
    verifyNoMoreInteractions(mockView);
    verifyNoInteractions(mockRow);
  }

  @Test
  @SuppressWarnings("all")
  public void indexOfNonExitingRowReturnsMinusOne() {

    Row mockRowOne = mock(Row.class);
    Row mockRowTwo = mock(Row.class);

    View mockView = mock(View.class);

    doReturn(Collections.singletonList(mockRowOne)).when(mockView).rows();
    doCallRealMethod().when(mockView).indexOf(any(Row.class));

    assertThat(mockView.indexOf(mockRowTwo)).isEqualTo(-1);

    verify(mockView, times(1)).indexOf(eq(mockRowTwo));
    verify(mockView, times(1)).rows();
    verifyNoInteractions(mockRowOne, mockRowTwo);
    verifyNoMoreInteractions(mockView);
  }

  @Test
  public void indexOfNullRowIsNullSafeReturnsMinusOne() {

    View mockView = mock(View.class);

    doCallRealMethod().when(mockView).indexOf(ArgumentMatchers.<Row>any());

    assertThat(mockView.indexOf((Row) null)).isEqualTo(-1);

    verify(mockView, times(1)).indexOf(ArgumentMatchers.<Row>isNull());
    verify(mockView, times(1)).rows();
    verifyNoMoreInteractions(mockView);
  }

  @Test
  public void isEmptyReturnsTrue() {

    View mockView = mock(View.class);

    doReturn(0).when(mockView).size();
    doCallRealMethod().when(mockView).isEmpty();

    assertThat(mockView.isEmpty()).isTrue();

    verify(mockView, times(1)).isEmpty();
    verify(mockView, times(1)).size();
    verifyNoMoreInteractions(mockView);
  }

  @Test
  public void isEmptyReturnsFalse() {

    View mockView = mock(View.class);

    doReturn(1).when(mockView).size();
    doCallRealMethod().when(mockView).isEmpty();

    assertThat(mockView.isEmpty()).isFalse();

    verify(mockView, times(1)).isEmpty();
    verify(mockView, times(1)).size();
    verifyNoMoreInteractions(mockView);
  }

  @Test
  public void queryReturnsResults() {

    Query mockQuery = mock(Query.class);

    View mockViewOne = mock(View.class);
    View mockViewTwo = mock(View.class);

    doReturn(mockQuery).when(mockQuery).from(any(View.class));
    doReturn(mockViewTwo).when(mockQuery).execute();
    doCallRealMethod().when(mockViewOne).query(any(Query.class));

    assertThat(mockViewOne.query(mockQuery)).isEqualTo(mockViewTwo);

    verify(mockViewOne, times(1)).query(eq(mockQuery));
    verify(mockQuery, times(1)).from(eq(mockViewOne));
    verify(mockQuery, times(1)).execute();
    verifyNoMoreInteractions(mockViewOne, mockQuery);
    verifyNoMoreInteractions(mockViewTwo);
  }

  @Test
  public void queryWithNullQueryThrowsException() {

    View mockView = mock(View.class);

    doCallRealMethod().when(mockView).query(any());

    assertThatIllegalArgumentException()
      .isThrownBy(() -> mockView.query(null))
      .withMessage("Query is required")
      .withNoCause();

    verify(mockView, times(1)).query(isNull());
    verifyNoMoreInteractions(mockView);
  }

  @Test
  public void rowsReturnsThis() {

    View mockView = mock(View.class);

    doCallRealMethod().when(mockView).rows();

    assertThat(mockView.rows()).isSameAs(mockView);

    verify(mockView, times(1)).rows();
    verifyNoMoreInteractions(mockView);
  }

  @Test
  public void sizeReturnsCount() {

    View mockView = mock(View.class);

    doReturn(100).when(mockView).count(any());
    doCallRealMethod().when(mockView).size();

    assertThat(mockView.size()).isEqualTo(100);

    verify(mockView, times(1)).size();
    verify(mockView, times(1)).count(isNotNull());
    verifyNoMoreInteractions(mockView);
  }

  @Test
  public void sizeReturnsRowCount() {

    Row mockRow = mock(Row.class);

    View mockView = mock(View.class);

    doReturn(Collections.singletonList(mockRow)).when(mockView).rows();
    doCallRealMethod().when(mockView).count(any());
    doCallRealMethod().when(mockView).size();

    assertThat(mockView.size()).isOne();

    verify(mockView, times(1)).size();
    verify(mockView, times(1)).count(isNotNull());
    verify(mockView, times(1)).rows();
    verifyNoMoreInteractions(mockView);
    verifyNoInteractions(mockRow);
  }
}
