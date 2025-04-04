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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.withSettings;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Predicate;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.Integers;
import org.mockito.ArgumentMatchers;
import org.mockito.quality.Strictness;

/**
 * Unit Tests for {@link Table}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.struct.tabular.Table
 * @since 1.0.0
 */
class TableUnitTests {

  @SuppressWarnings("unchecked")
  private static <T> List<T> listOf(T... elements) {
    return new ArrayList<>(Arrays.asList(elements));
  }

  private Column<?> mockColumn(String name) {

    Column<?> mockColumn = mock(Column.class, withSettings().strictness(Strictness.LENIENT).name(name));

    doReturn(name).when(mockColumn).getName();

    return mockColumn;
  }

  @Test
  void removeColumnAtIndexReturnsTrue() {

    Column<?> A = mockColumn("A");
    Column<?> B = mockColumn("B");
    Column<?> C = mockColumn("C");

    List<Column<?>> columns = new ArrayList<>(Arrays.asList(A, B, C));

    Table mockTable = mock(Table.class);

    doCallRealMethod().when(mockTable).removeColumn(anyInt());
    doReturn(columns).when(mockTable).columns();

    assertThat(columns).containsExactly(A, B, C);
    assertThat(mockTable.removeColumn(1)).isTrue();
    assertThat(columns).containsExactly(A, C);

    verify(mockTable, times(1)).removeColumn(eq(1));
    verify(mockTable, times(1)).columns();
    verifyNoMoreInteractions(mockTable);
  }

  @Test
  void removeColumnAtOverflowIndexReturnsFalse() {

    Column<?> A = mockColumn("A");
    Column<?> B = mockColumn("B");
    Column<?> C = mockColumn("C");

    List<Column<?>> columns = new ArrayList<>(Arrays.asList(A, B, C));

    Table mockTable = mock(Table.class);

    doCallRealMethod().when(mockTable).removeColumn(anyInt());
    doReturn(columns).when(mockTable).columns();

    assertThat(columns).containsExactly(A, B, C);
    assertThat(mockTable.removeColumn(3)).isFalse();
    assertThat(columns).containsExactly(A, B, C);

    verify(mockTable, times(1)).removeColumn(eq(3));
    verify(mockTable, times(1)).columns();
    verifyNoMoreInteractions(mockTable);
  }

  @Test
  void removeColumnAtUnderflowIndexReturnsFalse() {

    Column<?> A = mockColumn("A");
    Column<?> B = mockColumn("B");
    Column<?> C = mockColumn("C");

    List<Column<?>> columns = new ArrayList<>(Arrays.asList(A, B, C));

    Table mockTable = mock(Table.class);

    doCallRealMethod().when(mockTable).removeColumn(anyInt());
    doReturn(columns).when(mockTable).columns();

    assertThat(columns).containsExactly(A, B, C);
    assertThat(mockTable.removeColumn(-1)).isFalse();
    assertThat(columns).containsExactly(A, B, C);

    verify(mockTable, times(1)).removeColumn(eq(-1));
    verify(mockTable, times(1)).columns();
    verifyNoMoreInteractions(mockTable);
  }

  @Test
  void removeColumnByNameRemovesColumnAtIndexReturnsTrue() {

    Table mockTable = mock(Table.class);

    doCallRealMethod().when(mockTable).removeColumn(anyString());
    doReturn(2).when(mockTable).indexOf(anyString());
    doReturn(true).when(mockTable).removeColumn(anyInt());

    assertThat(mockTable.removeColumn("MockColumn")).isTrue();

    verify(mockTable, times(1)).removeColumn(eq("MockColumn"));
    verify(mockTable, times(1)).indexOf(eq("MockColumn"));
    verify(mockTable, times(1)).removeColumn(eq(2));
    verifyNoMoreInteractions(mockTable);
  }

  @Test
  void removeColumnByInvalidNameReturnsFalse() {

    Table mockTable = mock(Table.class);

    Arrays.asList("  ", "", null).forEach(columnName -> {

      doCallRealMethod().when(mockTable).removeColumn(eq(columnName));
      doReturn(Integers.MINUS_ONE).when(mockTable).indexOf(eq(columnName));

      assertThat(mockTable.removeColumn(columnName)).isFalse();

      verify(mockTable, times(1)).removeColumn(eq(columnName));
      verify(mockTable, times(1)).indexOf(eq(columnName));
      verify(mockTable, never()).removeColumn(anyInt());
      reset(mockTable);
    });

    verifyNoMoreInteractions(mockTable);
  }

  @Test
  void removeColumnByNonExistingNameReturnsFalse() {

    Table mockTable = mock(Table.class);

    doCallRealMethod().when(mockTable).removeColumn(anyString());
    doReturn(-1).when(mockTable).indexOf(anyString());

    assertThat(mockTable.removeColumn("NonExistingColumn")).isFalse();

    verify(mockTable, times(1)).removeColumn(eq("NonExistingColumn"));
    verify(mockTable, times(1)).indexOf(eq("NonExistingColumn"));
    verify(mockTable, never()).removeColumn(anyInt());
    verifyNoMoreInteractions(mockTable);
  }

  @Test
  void removeColumnRemovesColumnAtIndexReturnsTrue() {

    Column<?> mockColumn = mockColumn("MockColumn");

    Table mockTable = mock(Table.class);

    doCallRealMethod().when(mockTable).remove(any(Column.class));
    doReturn(2).when(mockTable).indexOf(any(Column.class));
    doReturn(true).when(mockTable).removeColumn(anyInt());

    assertThat(mockTable.remove(mockColumn)).isTrue();

    verify(mockTable, times(1)).remove(eq(mockColumn));
    verify(mockTable, times(1)).indexOf(eq(mockColumn));
    verify(mockTable, times(1)).removeColumn(eq(2));
    verifyNoMoreInteractions(mockTable);
  }

  @Test
  void removeNonExistingColumnReturnsFalse() {

    Column<?> mockColumn = mockColumn("MockColumn");

    Table mockTable = mock(Table.class);

    doCallRealMethod().when(mockTable).remove(any(Column.class));
    doReturn(Integers.MINUS_ONE).when(mockTable).indexOf(any(Column.class));

    assertThat(mockTable.remove(mockColumn)).isFalse();

    verify(mockTable, times(1)).remove(eq(mockColumn));
    verify(mockTable, times(1)).indexOf(eq(mockColumn));
    verify(mockTable, never()).removeColumn(anyInt());
    verifyNoMoreInteractions(mockTable);
  }

  @Test
  void removeNullColumnIsNullSafeReturnsFalse() {

    Table mockTable = mock(Table.class);

    doCallRealMethod().when(mockTable).remove(ArgumentMatchers.<Column<?>>any());
    doReturn(Integers.MINUS_ONE).when(mockTable).indexOf(ArgumentMatchers.<Column<?>>any());

    assertThat(mockTable.remove((Column<?>) null)).isFalse();

    verify(mockTable, times(1)).remove(isNull(Column.class));
    verify(mockTable, times(1)).indexOf(isNull(Column.class));
    verify(mockTable, never()).removeColumn(anyInt());
    verifyNoMoreInteractions(mockTable);
  }

  @Test
  @SuppressWarnings("unchecked")
  void removeAllRowsReturnsTrue() {

    Predicate<Row> mockPredicate = mock(Predicate.class);

    Row mockRowOne = mock(Row.class, "Mock Row One");
    Row mockRowTwo = mock(Row.class, "Mock Row Two");

    List<Row> rows = listOf(mockRowOne, mockRowTwo);

    Table mockTable = mock(Table.class);

    doCallRealMethod().when(mockTable).removeRows(any(Predicate.class));
    doReturn(rows).when(mockTable).rows();
    doAnswer(invocation ->  rows.remove(invocation.<Row>getArgument(0))).when(mockTable).remove(any(Row.class));
    doReturn(true).when(mockPredicate).test(any(Row.class));

    assertThat(rows).hasSize(2);
    assertThat(rows).containsExactly(mockRowOne, mockRowTwo);
    assertThat(mockTable.removeRows(mockPredicate)).isTrue();
    assertThat(rows).describedAs(rows.toString()).isEmpty();

    verify(mockTable, times(1)).removeRows(isA(Predicate.class));
    verify(mockTable, times(1)).rows();
    verify(mockTable, never()).remove(ArgumentMatchers.<Row>any());
    verify(mockPredicate, times(1)).test(eq(mockRowOne));
    verify(mockPredicate, times(1)).test(eq(mockRowTwo));
    verifyNoMoreInteractions(mockTable, mockPredicate);
    verifyNoInteractions(mockRowOne, mockRowTwo);
  }

  @Test
  @SuppressWarnings("unchecked")
  void removeNoRowsReturnsFalse() {

    Predicate<Row> mockPredicate = mock(Predicate.class);

    Row mockRowOne = mock(Row.class, "Mock Row One");
    Row mockRowTwo = mock(Row.class, "Mock Row Two");

    List<Row> rows = listOf(mockRowOne, mockRowTwo);

    Table mockTable = mock(Table.class);

    doCallRealMethod().when(mockTable).removeRows(any(Predicate.class));
    doReturn(rows).when(mockTable).rows();
    doReturn(false).when(mockPredicate).test(any());

    assertThat(rows).hasSize(2);
    assertThat(rows).containsExactly(mockRowOne, mockRowTwo);
    assertThat(mockTable.removeRows(mockPredicate)).isFalse();
    assertThat(rows).hasSize(2);
    assertThat(rows).containsExactly(mockRowOne, mockRowTwo);

    verify(mockTable, times(1)).removeRows(eq(mockPredicate));
    verify(mockTable, times(1)).rows();
    verify(mockTable, never()).remove(any(Row.class));
    verify(mockPredicate, times(1)).test(eq(mockRowOne));
    verify(mockPredicate, times(1)).test(eq(mockRowTwo));
    verifyNoMoreInteractions(mockTable, mockPredicate);
    verifyNoInteractions(mockRowOne, mockRowTwo);
  }

  @Test
  @SuppressWarnings("unchecked")
  void removeSelectRowsReturnsTrue() {

    Predicate<Row> mockPredicate = mock(Predicate.class);

    Row mockRowOne = mock(Row.class, "Mock Row One");
    Row mockRowTwo = mock(Row.class, "Mock Row Two");

    List<Row> rows = listOf(mockRowOne, mockRowTwo);

    Table mockTable = mock(Table.class);

    doCallRealMethod().when(mockTable).removeRows(any(Predicate.class));
    doReturn(rows).when(mockTable).rows();
    doAnswer(invocation -> rows.remove(invocation.<Row>getArgument(0))).when(mockTable).remove(any(Row.class));
    doReturn(true).doReturn(false).when(mockPredicate).test(any(Row.class));

    assertThat(rows).hasSize(2);
    assertThat(rows).containsExactly(mockRowOne, mockRowTwo);
    assertThat(mockTable.removeRows(mockPredicate)).isTrue();
    assertThat(rows).describedAs(rows.toString()).hasSize(1);
    assertThat(rows).containsExactly(mockRowTwo);

    verify(mockTable, times(1)).removeRows(eq(mockPredicate));
    verify(mockTable, times(1)).rows();
    verify(mockTable, never()).remove(ArgumentMatchers.<Row>any());
    verify(mockPredicate, times(1)).test(eq(mockRowOne));
    verify(mockPredicate, times(1)).test(eq(mockRowTwo));
    verifyNoMoreInteractions(mockTable, mockPredicate);
    verifyNoInteractions(mockRowOne, mockRowTwo);
  }

  @Test
  void removeRowsWithNullPredicateThrowsIllegalArgumentException() {

    Table mockTable = mock(Table.class);

    doCallRealMethod().when(mockTable).removeRows(any());

    assertThatIllegalArgumentException()
      .isThrownBy(() -> mockTable.removeRows(null))
      .withMessage("Predicate is required")
      .withNoCause();

    verify(mockTable, times(1)).removeRows(isNull());
    verify(mockTable, never()).rows();
    verify(mockTable, never()).remove(any(Row.class));
    verifyNoMoreInteractions(mockTable);
  }

  @Test
  void removeRowRemovesRowAtIndexReturnsTrue() {

    Row mockRow = mock(Row.class);

    Table mockTable = mock(Table.class);

    doCallRealMethod().when(mockTable).remove(any(Row.class));
    doReturn(2).when(mockTable).indexOf(any(Row.class));
    doReturn(true).when(mockTable).removeRow(anyInt());

    assertThat(mockTable.remove(mockRow)).isTrue();

    verify(mockTable, times(1)).remove(eq(mockRow));
    verify(mockTable, times(1)).indexOf(eq(mockRow));
    verify(mockTable, times(1)).removeRow(eq(2));
    verifyNoMoreInteractions(mockTable);
    verifyNoInteractions(mockRow);
  }

  @Test
  void removeNonExistingRowReturnsFalse() {

    Row mockRow = mock(Row.class);

    Table mockTable = mock(Table.class);

    doCallRealMethod().when(mockTable).remove(any(Row.class));
    doReturn(Integers.MINUS_ONE).when(mockTable).indexOf(any(Row.class));

    assertThat(mockTable.remove(mockRow)).isFalse();

    verify(mockTable, times(1)).remove(eq(mockRow));
    verify(mockTable, times(1)).indexOf(eq(mockRow));
    verify(mockTable, never()).removeRow(anyInt());
    verifyNoMoreInteractions(mockTable);
    verifyNoInteractions(mockRow);
  }

  @Test
  void removeNullRowIsNullSafeReturnsFalse() {

    Table mockTable = mock(Table.class);

    doCallRealMethod().when(mockTable).remove(ArgumentMatchers.<Row>any());
    doReturn(Integers.MINUS_ONE).when(mockTable).indexOf(ArgumentMatchers.<Row>any());

    assertThat(mockTable.remove((Row) null)).isFalse();

    verify(mockTable, times(1)).remove(isNull(Row.class));
    verify(mockTable, times(1)).indexOf(isNull(Row.class));
    verify(mockTable, never()).removeRow(anyInt());
    verifyNoMoreInteractions(mockTable);
  }

  @Test
  void removeRowAtIndexReturnsTrue() {

    Row mockRowOne = mock(Row.class, "Mock Row One");
    Row mockRowTwo = mock(Row.class, "Mock Row Two");

    List<Row> rows = listOf(mockRowOne, mockRowTwo);

    Table mockTable = mock(Table.class);

    doCallRealMethod().when(mockTable).removeRow(anyInt());
    doReturn(rows.iterator()).when(mockTable).iterator();

    assertThat(rows).hasSize(2);
    assertThat(rows).containsExactly(mockRowOne, mockRowTwo);
    assertThat(mockTable.removeRow(1)).isTrue();
    assertThat(rows).hasSize(1);
    assertThat(rows).containsExactly(mockRowOne);

    verify(mockTable, times(1)).removeRow(eq(1));
    verify(mockTable, times(1)).iterator();
    verifyNoInteractions(mockRowOne, mockRowTwo);
    verifyNoMoreInteractions(mockTable);
  }

  @Test
  void removeRowAtOverflowIndexReturnsFalse() {

    Row mockRowOne = mock(Row.class, "Mock Row One");
    Row mockRowTwo = mock(Row.class, "Mock Row Two");

    List<Row> rows = listOf(mockRowOne, mockRowTwo);

    Table mockTable = mock(Table.class);

    doCallRealMethod().when(mockTable).removeRow(anyInt());
    doReturn(rows.iterator()).when(mockTable).iterator();

    assertThat(rows).hasSize(2);
    assertThat(rows).containsExactly(mockRowOne, mockRowTwo);
    assertThat(mockTable.removeRow(2)).isFalse();
    assertThat(rows).hasSize(2);
    assertThat(rows).containsExactly(mockRowOne, mockRowTwo);

    verify(mockTable, times(1)).removeRow(eq(2));
    verify(mockTable, times(1)).iterator();
    verifyNoInteractions(mockRowOne, mockRowTwo);
    verifyNoMoreInteractions(mockTable);
  }

  @Test
  void removeRowAtUnderflowIndexReturnsFalse() {

    Row mockRowOne = mock(Row.class, "Mock Row One");
    Row mockRowTwo = mock(Row.class, "Mock Row Two");

    List<Row> rows = listOf(mockRowOne, mockRowTwo);

    Table mockTable = mock(Table.class);

    doCallRealMethod().when(mockTable).removeRow(anyInt());
    doReturn(rows.iterator()).when(mockTable).iterator();

    assertThat(rows).hasSize(2);
    assertThat(rows).containsExactly(mockRowOne, mockRowTwo);
    assertThat(mockTable.removeRow(-1)).isFalse();
    assertThat(rows).hasSize(2);
    assertThat(rows).containsExactly(mockRowOne, mockRowTwo);

    verify(mockTable, times(1)).removeRow(eq(-1));
    verify(mockTable, times(1)).iterator();
    verifyNoInteractions(mockRowOne, mockRowTwo);
    verifyNoMoreInteractions(mockTable);
  }

  @Test
  void setValueWithRowIndexAndColumnIndexReturnsCurrentValue() {

    Row mockRow = mock(Row.class);

    Table mockTable = mock(Table.class);

    doCallRealMethod().when(mockTable).setValue(anyInt(), anyInt(), any());
    doReturn(mockRow).when(mockTable).getRow(anyInt());
    doReturn("mock").when(mockRow).setValue(anyInt(), any());

    assertThat(mockTable.setValue(1, 2, "test")).isEqualTo("mock");

    verify(mockTable, times(1)).setValue(eq(1), eq(2), eq("test"));
    verify(mockTable, times(1)).getRow(eq(1));
    verify(mockRow, times(1)).setValue(eq(2), eq("test"));
    verifyNoMoreInteractions(mockTable, mockRow);
  }

  @Test
  void setValueWithRowIndexAndColumnNameReturnsCurrentValue() {

    Table mockTable = mock(Table.class);

    doCallRealMethod().when(mockTable).setValue(anyInt(), anyString(), any());
    doReturn(4).when(mockTable).indexOf(anyString());
    doReturn("mock").when(mockTable).setValue(anyInt(), eq(4), any());

    assertThat(mockTable.setValue(1, "TestColumn", "test")).isEqualTo("mock");

    verify(mockTable, times(1)).setValue(eq(1), eq("TestColumn"), eq("test"));
    verify(mockTable, times(1)).indexOf(eq("TestColumn"));
    verify(mockTable, times(1)).setValue(eq(1), eq(4), eq("test"));
    verifyNoMoreInteractions(mockTable);
  }

  @Test
  void setValueWithRowIndexAndInvalidColumnNameThrowsIllegalArgumentException() {

    Table mockTable = mock(Table.class);

    doCallRealMethod().when(mockTable).setValue(anyInt(), ArgumentMatchers.<String>any(), any());

    Arrays.asList("  ", "", null).forEach(columnName -> {

      assertThatIllegalArgumentException()
        .isThrownBy(() -> mockTable.setValue(1, columnName, "test"))
        .withMessage("Column [%s] is not valid", columnName)
        .withNoCause();

      verify(mockTable, times(1)).setValue(eq(1), eq(columnName), eq("test"));
    });

    verifyNoMoreInteractions(mockTable);
  }

  @Test
  void setValueWithRowIndexAndNonExistingColumnNameThrowsIllegalArgumentException() {

    Table mockTable = mock(Table.class);

    doCallRealMethod().when(mockTable).setValue(anyInt(), anyString(), any());
    doReturn(Integers.MINUS_ONE).when(mockTable).indexOf(anyString());

    assertThatIllegalArgumentException()
      .isThrownBy(() -> mockTable.setValue(1, "NonExistingColumn", "test"))
      .withMessage("Column [NonExistingColumn] is not valid")
      .withNoCause();

    verify(mockTable, times(1)).setValue(anyInt(), eq("NonExistingColumn"), eq("test"));
    verify(mockTable, times(1)).indexOf(eq("NonExistingColumn"));
    verifyNoMoreInteractions(mockTable);
  }

  @Test
  void setValueWithRowIndexAndColumnReturnsCurrentValue() {

    Column<?> mockColumn = mock(Column.class);

    Table mockTable = mock(Table.class);

    doCallRealMethod().when(mockTable).setValue(anyInt(), any(Column.class), any());
    doReturn(16).when(mockTable).indexOf(any(Column.class));
    doReturn("mock").when(mockTable).setValue(anyInt(), anyInt(), any());

    assertThat(mockTable.setValue(1, mockColumn, "test")).isEqualTo("mock");

    verify(mockTable, times(1)).setValue(eq(1), eq(mockColumn), eq("test"));
    verify(mockTable, times(1)).indexOf(eq(mockColumn));
    verify(mockTable, times(1)).setValue(eq(1), eq(16), eq("test"));
    verifyNoMoreInteractions(mockTable);
    verifyNoInteractions(mockColumn);
  }

  @Test
  void setValueWithRowIndexAndNonExistingColumnThrowsIllegalArgumentException() {

    Column<?> mockColumn = mock(Column.class);

    Table mockTable = mock(Table.class);

    doCallRealMethod().when(mockTable).setValue(anyInt(), any(Column.class), any());
    doReturn(Integers.MINUS_ONE).when(mockTable).indexOf(any(Column.class));

    assertThatIllegalArgumentException()
      .isThrownBy(() -> mockTable.setValue(1, mockColumn, "test"))
      .withMessage("Column [%s] is not valid", mockColumn)
      .withNoCause();

    verify(mockTable, times(1)).setValue(eq(1), eq(mockColumn), eq("test"));
    verify(mockTable, times(1)).indexOf(eq(mockColumn));
    verify(mockTable, never()).setValue(anyInt(), anyInt(), any());
    verifyNoMoreInteractions(mockTable);
    verifyNoInteractions(mockColumn);
  }

  @Test
  void setValueWithRowIndexAndNullColumnThrowsIllegalArgumentException() {

    Table mockTable = mock(Table.class);

    doCallRealMethod().when(mockTable).setValue(anyInt(), ArgumentMatchers.<Column<?>>any(), any());

    assertThatIllegalArgumentException()
      .isThrownBy(() -> mockTable.setValue(1, (Column<?>) null, "test"))
      .withMessage("Column [null] is not valid")
      .withNoCause();

    verify(mockTable, times(1)).setValue(eq(1), isNull(Column.class), eq("test"));
    verify(mockTable, never()).indexOf(any(Column.class));
    verify(mockTable, never()).setValue(anyInt(), anyInt(), any());
    verifyNoMoreInteractions(mockTable);
  }
}
