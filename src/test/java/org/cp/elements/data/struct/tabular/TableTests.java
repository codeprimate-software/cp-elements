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

package org.cp.elements.data.struct.tabular;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Predicate;

import org.cp.elements.lang.StringUtils;
import org.junit.Test;
import org.mockito.ArgumentMatchers;

/**
 * Unit tests for {@link Table}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.struct.tabular.Table
 * @since 1.0.0
 */
public class TableTests {

  @Test
  public void removeColumnByNameRemovesColumnAtIndexReturnsTrue() {

    Table mockTable = mock(Table.class);

    when(mockTable.indexOf(anyString())).thenReturn(2);
    when(mockTable.removeColumn(anyInt())).thenReturn(true);
    when(mockTable.removeColumn(anyString())).thenCallRealMethod();

    assertThat(mockTable.removeColumn("TestColumn")).isTrue();

    verify(mockTable, times(1)).indexOf(eq("TestColumn"));
    verify(mockTable, times(1)).removeColumn(eq(2));
  }

  @Test
  public void removeColumnByBlankNameReturnsFalse() {
    testRemoveColumnByInvalidNameReturnsFalse("  ");
  }

  @Test
  public void removeColumnByEmptyNameReturnsFalse() {
    testRemoveColumnByInvalidNameReturnsFalse("");
  }

  @Test
  public void removeColumnByNonExistingNameReturnsFalse() {
    testRemoveColumnByInvalidNameReturnsFalse("NonExistingColumn");
  }

  @Test
  public void removeColumnByNullNameReturnsFalse() {

    Table mockTable = mock(Table.class);

    when(mockTable.indexOf(ArgumentMatchers.<String>any())).thenReturn(-1);
    when(mockTable.removeColumn(anyInt())).thenThrow(new IndexOutOfBoundsException("test"));
    when(mockTable.removeColumn(any())).thenCallRealMethod();

    assertThat(mockTable.removeColumn(null)).isFalse();

    verify(mockTable, times(1)).indexOf(ArgumentMatchers.<String>isNull());
    verify(mockTable, never()).removeColumn(anyInt());
  }

  private void testRemoveColumnByInvalidNameReturnsFalse(String columnName) {

    Table mockTable = mock(Table.class);

    when(mockTable.indexOf(anyString())).thenReturn(-1);
    when(mockTable.removeColumn(anyInt())).thenThrow(new IndexOutOfBoundsException("test"));
    when(mockTable.removeColumn(anyString())).thenCallRealMethod();

    assertThat(mockTable.removeColumn(columnName)).isFalse();

    verify(mockTable, times(1)).indexOf(eq(columnName));
    verify(mockTable, never()).removeColumn(anyInt());
  }

  @Test
  public void removeColumnRemovesColumnAtIndexReturnsTrue() {

    Column mockColumn = mock(Column.class);

    Table mockTable = mock(Table.class);

    when(mockTable.indexOf(any(Column.class))).thenReturn(2);
    when(mockTable.removeColumn(anyInt())).thenReturn(true);
    when(mockTable.remove(any(Column.class))).thenCallRealMethod();

    assertThat(mockTable.remove(mockColumn)).isTrue();

    verify(mockTable, times(1)).indexOf(eq(mockColumn));
    verify(mockTable, times(1)).removeColumn(eq(2));
  }

  @Test
  public void removeNonExistingColumnReturnsFalse() {

    Column mockColumn = mock(Column.class);

    Table mockTable = mock(Table.class);

    when(mockTable.indexOf(any(Column.class))).thenReturn(-1);
    when(mockTable.removeColumn(anyInt())).thenThrow(new IndexOutOfBoundsException("test"));
    when(mockTable.remove(any(Column.class))).thenCallRealMethod();

    assertThat(mockTable.remove(mockColumn)).isFalse();

    verify(mockTable, times(1)).indexOf(eq(mockColumn));
    verify(mockTable, never()).removeColumn(eq(-1));
  }

  @Test
  public void removeNullColumnReturnsFalse() {

    Table mockTable = mock(Table.class);

    when(mockTable.indexOf(ArgumentMatchers.<Column>any())).thenReturn(-1);
    when(mockTable.removeColumn(anyInt())).thenThrow(new IndexOutOfBoundsException("test"));
    when(mockTable.remove(ArgumentMatchers.<Column>any())).thenCallRealMethod();

    assertThat(mockTable.remove((Column) null)).isFalse();

    verify(mockTable, times(1)).indexOf(ArgumentMatchers.<Column>isNull());
    verify(mockTable, never()).removeColumn(anyInt());
  }

  @Test
  @SuppressWarnings("unchecked")
  public void removeAllRowsReturnsTrue() {

    Row mockRowOne = mock(Row.class, "Mock Row One");
    Row mockRowTwo = mock(Row.class, "Mock Row Two");

    List<Row> rows = new ArrayList<>(Arrays.asList(mockRowOne, mockRowTwo));

    Table mockTable = mock(Table.class);

    when(mockTable.rows()).thenReturn(rows);

    when(mockTable.remove(any(Row.class)))
      .thenAnswer(invocation -> rows.remove(invocation.<Row>getArgument(0)));

    when(mockTable.removeRows(any(Predicate.class))).thenCallRealMethod();

    assertThat(rows).hasSize(2);
    assertThat(mockTable.removeRows(row -> true)).isTrue();
    assertThat(rows).describedAs(rows.toString()).isEmpty();

    verify(mockTable, times(1)).rows();
    verify(mockTable, never()).remove(ArgumentMatchers.<Row>any());
    verifyZeroInteractions(mockRowOne);
    verifyZeroInteractions(mockRowTwo);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void removeSelectRowsReturnsTrue() {

    Predicate<Row> mockPredicate = mock(Predicate.class);

    Row mockRowOne = mock(Row.class, "Mock Row One");
    Row mockRowTwo = mock(Row.class, "Mock Row Two");

    List<Row> rows = new ArrayList<>(Arrays.asList(mockRowOne, mockRowTwo));

    Table mockTable = mock(Table.class);

    when(mockTable.rows()).thenReturn(rows);

    when(mockTable.remove(any(Row.class)))
      .thenAnswer(invocation -> rows.remove(invocation.<Row>getArgument(0)));

    when(mockTable.removeRows(any(Predicate.class))).thenCallRealMethod();

    when(mockPredicate.test(any(Row.class))).thenReturn(true).thenReturn(false);

    assertThat(rows).hasSize(2);
    assertThat(mockTable.removeRows(mockPredicate)).isTrue();
    assertThat(rows).describedAs(rows.toString()).hasSize(1);
    assertThat(rows).containsExactly(mockRowTwo);

    verify(mockPredicate, times(1)).test(eq(mockRowOne));
    verify(mockPredicate, times(1)).test(eq(mockRowTwo));
    verify(mockTable, times(1)).rows();
    verify(mockTable, never()).remove(ArgumentMatchers.<Row>any());
    verifyZeroInteractions(mockRowOne);
    verifyZeroInteractions(mockRowTwo);
  }

  @Test(expected = IllegalArgumentException.class)
  public void removeRowsWithNullPredicateThrowsIllegalArgumentException() {

    Table mockTable = mock(Table.class);

    when(mockTable.removeRows(any())).thenCallRealMethod();

    try {
      mockTable.removeRows(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Predicate is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockTable, never()).rows();
      verify(mockTable, never()).remove(any(Row.class));
    }
  }

  @Test
  public void removeRowRemovesRowAtIndexReturnsTrue() {

    Row mockRow = mock(Row.class);

    Table mockTable = mock(Table.class);

    when(mockTable.indexOf(any(Row.class))).thenReturn(1);
    when(mockTable.removeRow(anyInt())).thenReturn(true);
    when(mockTable.remove(any(Row.class))).thenCallRealMethod();

    assertThat(mockTable.remove(mockRow)).isTrue();

    verify(mockTable, times(1)).indexOf(eq(mockRow));
    verify(mockTable, times(1)).removeRow(eq(1));
  }

  @Test
  public void removeNonExistingRowReturnsFalse() {

    Row mockRow = mock(Row.class);

    Table mockTable = mock(Table.class);

    when(mockTable.indexOf(any(Row.class))).thenReturn(-1);
    when(mockTable.removeRow(anyInt())).thenThrow(new IndexOutOfBoundsException("test"));
    when(mockTable.remove(any(Row.class))).thenCallRealMethod();

    assertThat(mockTable.remove(mockRow)).isFalse();

    verify(mockTable, times(1)).indexOf(eq(mockRow));
    verify(mockTable, never()).removeRow(anyInt());
  }

  @Test
  public void removeNullRowReturnsFalse() {

    Table mockTable = mock(Table.class);

    when(mockTable.remove(ArgumentMatchers.<Row>any())).thenCallRealMethod();

    assertThat(mockTable.remove((Row) null)).isFalse();

    verify(mockTable, never()).indexOf(ArgumentMatchers.<Row>any());
    verify(mockTable, never()).removeRow(anyInt());
  }

  @Test
  public void setValueWithRowAndColumnIndexReturnsCurrentValue() {

    Row mockRow = mock(Row.class);

    Table mockTable = mock(Table.class);

    when(mockRow.setValue(anyInt(), any())).thenReturn("mock");
    when(mockTable.getRow(anyInt())).thenReturn(mockRow);
    when(mockTable.setValue(anyInt(), anyInt(), any())).thenCallRealMethod();

    assertThat(mockTable.setValue(1, 1, "test")).isEqualTo("mock");

    verify(mockRow, times(1)).setValue(eq(1), eq("test"));
    verify(mockTable, times(1)).getRow(eq(1));
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void setValueWithRowAndColumnIndexUsingInvalidColumnIndexThrowsIndexOutOfBoundsException() {

    Row mockRow = mock(Row.class);

    Table mockTable = mock(Table.class);

    when(mockRow.setValue(anyInt(), any())).thenThrow(new IndexOutOfBoundsException("test"));
    when(mockTable.getRow(anyInt())).thenReturn(mockRow);
    when(mockTable.setValue(anyInt(), anyInt(), any())).thenCallRealMethod();

    try {
      mockTable.setValue(0, -1, "test");
    }
    catch (IndexOutOfBoundsException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockTable, times(1)).getRow(eq(0));
      verify(mockRow, times(1)).setValue(eq(-1), eq("test"));
    }
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void setValueWithRowAndColumnIndexUsingInvalidRowIndexThrowsIndexOutOfBoundsException() {

    Table mockTable = mock(Table.class);

    when(mockTable.getRow(anyInt())).thenThrow(new IndexOutOfBoundsException("test"));
    when(mockTable.setValue(anyInt(), anyInt(), any())).thenCallRealMethod();

    try {
      mockTable.setValue(-1, 0, "test");
    }
    catch (IndexOutOfBoundsException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockTable, times(1)).getRow(eq(-1));
    }
  }

  @Test
  public void setValueWithRowIndexAndColumnNameReturnsCurrentValue() {

    Table mockTable = mock(Table.class);

    when(mockTable.indexOf(anyString())).thenReturn(2);
    when(mockTable.setValue(anyInt(), anyInt(), any())).thenReturn("mock");
    when(mockTable.setValue(anyInt(), anyString(), any())).thenCallRealMethod();

    assertThat(mockTable.setValue(1, "TestColumn", "test")).isEqualTo("mock");

    verify(mockTable, times(1)).indexOf(eq("TestColumn"));
    verify(mockTable, times(1)).setValue(eq(1), eq(2), eq("test"));
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void setValueWithInvalidRowIndexAndColumnNameThrowsIndexOutOfBoundsException() {

    Table mockTable = mock(Table.class);

    when(mockTable.indexOf(anyString())).thenReturn(1);
    when(mockTable.setValue(anyInt(), anyInt(), any())).thenThrow(new IndexOutOfBoundsException("test"));
    when(mockTable.setValue(anyInt(), anyString(), any())).thenCallRealMethod();

    try {
      mockTable.setValue(-1, "TestColumn", "test");
    }
    catch (IndexOutOfBoundsException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockTable, times(1)).indexOf(eq("TestColumn"));
      verify(mockTable, times(1)).setValue(eq(-1), eq(1), eq("test"));
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void setValueWithRowIndexAndBlankColumnNameThrowsIllegalArgumentException() {
    testSetValueWithRowIndexAndInvalidColumnNameThrowsIllegalArgumentException("  ");
  }

  @Test(expected = IllegalArgumentException.class)
  public void setValueWithRowIndexAndEmptyColumnNameThrowsIllegalArgumentException() {
    testSetValueWithRowIndexAndInvalidColumnNameThrowsIllegalArgumentException("");
  }

  @Test(expected = IllegalArgumentException.class)
  public void setValueWithRowIndexAndNonExistingColumnNameThrowsIllegalArgumentException() {
    testSetValueWithRowIndexAndInvalidColumnNameThrowsIllegalArgumentException("NonExistingColumn");
  }

  @Test(expected = IllegalArgumentException.class)
  public void setValueWithRowIndexAndNullColumnNameThrowsIllegalArgumentException() {
    testSetValueWithRowIndexAndInvalidColumnNameThrowsIllegalArgumentException(null);
  }

  private void testSetValueWithRowIndexAndInvalidColumnNameThrowsIllegalArgumentException(String columnName) {

    Table mockTable = mock(Table.class);

    when(mockTable.indexOf(anyString())).thenReturn(-1);
    when(mockTable.setValue(anyInt(), ArgumentMatchers.<String>any(), any())).thenCallRealMethod();

    try {
      mockTable.setValue(1, columnName, "test");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Column [%s] is not valid", columnName);
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {

      if (StringUtils.hasText(columnName)) {
        verify(mockTable, times(1)).indexOf(eq(columnName));
      }
      else {
        verify(mockTable, never()).indexOf(anyString());
      }

      verify(mockTable, never()).setValue(anyInt(), anyInt(), any());
    }
  }

  @Test
  public void setValueWithRowIndexAndColumnReturnsCurrentValue() {

    Column mockColumn = mock(Column.class);

    Table mockTable = mock(Table.class);

    when(mockTable.indexOf(any(Column.class))).thenReturn(2);
    when(mockTable.setValue(anyInt(), anyInt(), any())).thenReturn("mock");
    when(mockTable.setValue(anyInt(), any(Column.class), any())).thenCallRealMethod();

    assertThat(mockTable.setValue(1, mockColumn, "test")).isEqualTo("mock");

    verify(mockTable, times(1)).indexOf(eq(mockColumn));
    verify(mockTable, times(1)).setValue(eq(1), eq(2), eq("test"));
    verifyZeroInteractions(mockColumn);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void setValueWithInvalidRowIndexAndColumnThrowsIndexOutOfBoundsException() {

    Column mockColumn = mock(Column.class);

    Table mockTable = mock(Table.class);

    when(mockTable.indexOf(any(Column.class))).thenReturn(1);
    when(mockTable.setValue(anyInt(), anyInt(), any())).thenThrow(new IndexOutOfBoundsException("test"));
    when(mockTable.setValue(anyInt(), any(Column.class), any())).thenCallRealMethod();

    try {
      mockTable.setValue(-1, mockColumn, "test");
    }
    catch (IndexOutOfBoundsException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockTable, times(1)).indexOf(eq(mockColumn));
      verify(mockTable, times(1)).setValue(eq(-1), eq(1), eq("test"));
      verifyZeroInteractions(mockColumn);
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void setValueWithRowIndexAndInvalidColumnThrowsIllegalArgumentException() {

    Column mockColumn = mock(Column.class);

    Table mockTable = mock(Table.class);

    when(mockColumn.toString()).thenReturn("TestColumn");
    when(mockTable.indexOf(any(Column.class))).thenReturn(-1);
    when(mockTable.setValue(anyInt(), any(Column.class), any())).thenCallRealMethod();

    try {
      mockTable.setValue(1, mockColumn, "test");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Column [TestColumn] is not valid");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockTable, times(1)).indexOf(eq(mockColumn));
      verify(mockTable, never()).setValue(anyInt(), anyInt(), any());
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void setValueWithRowIndexAndNullColumnThrowsIllegalArgumentException() {

    Table mockTable = mock(Table.class);

    when(mockTable.setValue(anyInt(), ArgumentMatchers.<Column>any(), any())).thenCallRealMethod();

    try {
      mockTable.setValue(0, (Column) null, "test");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Column [null] is not valid");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockTable, never()).indexOf(ArgumentMatchers.<Column>any());
      verify(mockTable, never()).setValue(anyInt(), anyInt(), any());
    }
  }
}
