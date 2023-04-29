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
package org.cp.elements.data.struct.tabular.provider;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import java.util.Arrays;
import java.util.Collections;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import org.junit.Test;

import org.cp.elements.data.struct.tabular.Column;
import org.cp.elements.data.struct.tabular.Row;
import org.cp.elements.util.stream.StreamUtils;

import org.mockito.quality.Strictness;

/**
 * Unit Tests for {@link InMemoryTable}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.struct.tabular.Column
 * @see org.cp.elements.data.struct.tabular.Row
 * @see org.cp.elements.data.struct.tabular.provider.InMemoryTable
 * @see org.cp.elements.data.struct.tabular.provider.InMemoryTable.InMemoryColumn
 * @see org.cp.elements.data.struct.tabular.provider.InMemoryTable.InMemoryRow
 * @since 1.0.0
 */
public class InMemoryTableUnitTests {

  @SuppressWarnings({ "unchecked" })
  private <T> Column<T> mockColumn(String name) {

    Column<T> mockColumn = mock(Column.class, withSettings().strictness(Strictness.LENIENT).name(name));

    doReturn(name).when(mockColumn).getName();
    doReturn(Object.class).when(mockColumn).getType();

    return mockColumn;
  }

  @Test
  public void constructNewInMemoryTable() {

    Column<?> mockColumnOne = mockColumn("ONE");
    Column<?> mockColumnTwo = mockColumn("TWO");

    InMemoryTable table = new InMemoryTable(mockColumnOne, mockColumnTwo);

    assertThat(table).isNotNull();
    assertThat(table.rows()).isEmpty();

    AtomicInteger index = new AtomicInteger(0);

    StreamUtils.stream(table.columns()).forEach(column -> {

      assertThat(column.getName())
        .isEqualTo(Arrays.asList(mockColumnOne, mockColumnTwo).get(index.getAndIncrement()).getName());

      assertThat(column.getView().orElse(null)).isEqualTo(table);

    });

    assertThat(index.get()).isEqualTo(2);
  }

  @Test
  public void constructNewInMemoryTableWithEmptyColumnsThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(InMemoryTable::new)
      .withMessage("Columns are required")
      .withNoCause();
  }

  @Test
  public void constructNewInMemoryTableWithNullColumnsThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new InMemoryTable((Column<?>[]) null))
      .withMessage("Columns are required")
      .withNoCause();
  }

  @Test
  public void ofColumnArray() {

    Column<?> mockColumnOne = mockColumn("ONE");
    Column<?> mockColumnTwo = mockColumn("TWO");

    InMemoryTable table = InMemoryTable.of(mockColumnOne, mockColumnTwo);

    assertThat(table).isNotNull();
    assertThat(table).isEmpty();
    assertThat(table.getColumns().stream().map(Column::getName).collect(Collectors.toList()))
      .containsExactly("ONE", "TWO");
  }

  @Test
  public void ofEmptyColumnArrayThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(InMemoryTable::of)
      .withMessage("Columns are required")
      .withNoCause();
  }

  @Test
  public void ofNullColumnArrayThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> InMemoryTable.of((Column<?>[]) null))
      .withMessage("Columns are required")
      .withNoCause();
  }

  @Test
  public void ofIterableColumns() {

    Column<?> mockColumnOne = mockColumn("ONE");
    Column<?> mockColumnTwo = mockColumn("TWO");

    InMemoryTable table = InMemoryTable.of(Arrays.asList(mockColumnOne, mockColumnTwo));

    assertThat(table).isNotNull();
    assertThat(table).isEmpty();
    assertThat(table.getColumns().stream()
      .map(Column::getName)
      .collect(Collectors.toList()))
      .containsExactly("ONE", "TWO");
  }

  @Test
  public void ofEmptyIterableColumnsThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> InMemoryTable.of(Collections.emptyList()))
      .withMessage("Columns are required")
      .withNoCause();
  }

  @Test
  public void ofNullIterableColumnsThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> InMemoryTable.of((Iterable<Column<?>>) null))
      .withMessage("Columns are required")
      .withNoCause();
  }

  @Test
  @SuppressWarnings("all")
  public void addColumnIsSuccessful() {

    Column mockColumnOne = mockColumn("ONE");
    Column mockColumnTwo = mockColumn("TWO");

    InMemoryTable.InMemoryRow mockRowOne = mock(InMemoryTable.InMemoryRow.class);
    InMemoryTable.InMemoryRow mockRowTwo = mock(InMemoryTable.InMemoryRow.class);

    doNothing().when(mockRowOne).addColumn();
    doNothing().when(mockRowTwo).addColumn();

    InMemoryTable table = spy(InMemoryTable.of(mockColumnOne));

    assertThat(table).isNotNull();

    doReturn(Arrays.asList(mockRowOne, mockRowTwo)).when(table).getRows();

    assertThat(table).hasSize(2);
    assertThat(table.getColumns().stream().map(Column::getName).collect(Collectors.toList())).containsExactly("ONE");
    assertThat(table.add(mockColumnTwo)).isTrue();
    assertThat(table.getColumns().stream()
      .map(Column::getName)
      .collect(Collectors.toList()))
      .containsExactly("ONE", "TWO");

    verify(mockRowOne, times(1)).addColumn();
    verify(mockRowTwo, times(1)).addColumn();
  }

  @SuppressWarnings("all")
  @Test(expected = IllegalArgumentException.class)
  public void addInvalidColumnThrowsIllegalArgumentException() {

    Column mockColumnOne = mockColumn("ONE");

    InMemoryTable.InMemoryRow mockRowOne = mock(InMemoryTable.InMemoryRow.class);
    InMemoryTable.InMemoryRow mockRowTwo = mock(InMemoryTable.InMemoryRow.class);

    doNothing().when(mockRowOne).addColumn();
    doNothing().when(mockRowTwo).addColumn();

    InMemoryTable table = spy(InMemoryTable.of(mockColumnOne));

    assertThat(table).isNotNull();

    doReturn(Arrays.asList(mockRowOne, mockRowTwo)).when(table).getRows();

    assertThat(table).hasSize(2);
    assertThat(table.getColumns().stream()
      .map(Column::getName)
      .collect(Collectors.toList()))
      .containsExactly("ONE");

    try {
      table.add((Column) null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Column is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {

      assertThat(table.getColumns().stream()
        .map(Column::getName)
        .collect(Collectors.toList()))
        .containsExactly("ONE");

      verify(mockRowOne, never()).addColumn();
      verify(mockRowTwo, never()).addColumn();
    }
  }

  @Test
  public void addRowIsSuccessful() {

    Column<?> mockColumn = mockColumn("MockColumn");

    InMemoryTable table = InMemoryTable.of(mockColumn);

    assertThat(table).isNotNull();
    assertThat(table).hasSize(0);
    assertThat(table.getColumns().stream().map(Column::getName).collect(Collectors.toSet()))
      .containsExactly("MockColumn");

    Row mockRow = mock(Row.class);

    when(mockRow.getValue(eq(0))).thenReturn("test");

    assertThat(table.add(mockRow)).isTrue();
    assertThat(table).hasSize(1);
    assertThat(table.getRow(0).<String>getValue(mockColumn)).isEqualTo("test");

    verify(mockRow, times(1)).getValue(eq(0));
  }

  @Test(expected = IllegalArgumentException.class)
  public void addInvalidRowThrowsIllegalArgumentException() {

    Column<?> mockColumn = mockColumn("MockColumn");

    InMemoryTable table = InMemoryTable.of(mockColumn);

    assertThat(table).isNotNull();
    assertThat(table).hasSize(0);
    assertThat(table.getColumns().stream().map(Column::getName).collect(Collectors.toSet()))
      .containsExactly("MockColumn");

    try {
      table.add((Row) null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Row is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      assertThat(table).hasSize(0);
    }
  }

  @Test
  public void columnsReturnsTableColumns() {

    Column<?> mockColumnOne = mockColumn("One");
    Column<?> mockColumnTwo = mockColumn("Two");

    InMemoryTable table = InMemoryTable.of(mockColumnOne, mockColumnTwo);

    assertThat(table).isNotNull();

    assertThat(StreamSupport.stream(table.columns().spliterator(), false)
      .map(Column::getName)
      .collect(Collectors.toSet()))
      .containsExactly("One", "Two");
  }

  @Test
  public void iteratorReturnsNoRowsWhenTableIsEmpty() {

    Column<?> mockColumn = mockColumn("MockColumn");

    InMemoryTable table = InMemoryTable.of(mockColumn);

    assertThat(table).isNotNull();
    assertThat(table).isEmpty();
    assertThat(table).isEmpty();
  }

  @Test
  @SuppressWarnings("all")
  public void iteratorReturnsTableRows() {

    Column mockColumn = mockColumn("MockColumn");

    Row mockRowOne = mock(Row.class);
    Row mockRowTwo = mock(Row.class);

    InMemoryTable table = spy(InMemoryTable.of(mockColumn));

    assertThat(table).isNotNull();

    doReturn(Arrays.asList(mockRowOne, mockRowTwo)).when(table).getRows();

    assertThat(table).containsExactly(mockRowOne, mockRowTwo);

    verify(table, times(1)).getRows();
  }

  @Test
  @SuppressWarnings("all")
  public void removeColumnIsSuccessful() {

    Column mockColumnOne = mockColumn("One");
    Column mockColumnTwo = mockColumn("Two");

    InMemoryTable.InMemoryRow mockRowOne = mock(InMemoryTable.InMemoryRow.class);
    InMemoryTable.InMemoryRow mockRowTwo = mock(InMemoryTable.InMemoryRow.class);

    doNothing().when(mockRowOne).removeColumn(anyInt());
    doNothing().when(mockRowTwo).removeColumn(anyInt());

    InMemoryTable table = spy(InMemoryTable.of(mockColumnOne, mockColumnTwo));

    assertThat(table).isNotNull();

    doReturn(Arrays.asList(mockRowOne, mockRowTwo)).when(table).getRows();

    assertThat(table).hasSize(2);
    assertThat(table.getColumns().stream().map(Column::getName).collect(Collectors.toSet()))
      .containsExactly("One", "Two");
    assertThat(table.removeColumn(0)).isTrue();
    assertThat(table).hasSize(2);
    assertThat(table.getColumns().stream().map(Column::getName).collect(Collectors.toSet()))
      .containsExactly("Two");

    verify(mockRowOne, times(1)).removeColumn(eq(0));
    verify(mockRowTwo, times(1)).removeColumn(eq(0));
  }

  @SuppressWarnings("all")
  @Test(expected = IndexOutOfBoundsException.class)
  public void removeInvalidColumnThrowsIndexOutOfBoundsException() {

    Column mockColumn = mockColumn("MockColumn");

    InMemoryTable.InMemoryRow mockRowOne = mock(InMemoryTable.InMemoryRow.class);
    InMemoryTable.InMemoryRow mockRowTwo = mock(InMemoryTable.InMemoryRow.class);

    doNothing().when(mockRowOne).removeColumn(anyInt());
    doNothing().when(mockRowTwo).removeColumn(anyInt());

    InMemoryTable table = spy(InMemoryTable.of(mockColumn));

    assertThat(table).isNotNull();

    doReturn(Arrays.asList(mockRowOne, mockRowTwo)).when(table).getRows();

    assertThat(table).hasSize(2);
    assertThat(table.getColumns().stream().map(Column::getName).collect(Collectors.toSet()))
      .containsExactly("MockColumn");

    try {
      table.removeColumn(1);
    }
    finally {

      assertThat(table).hasSize(2);
      assertThat(table.getColumns().stream().map(Column::getName).collect(Collectors.toSet()))
        .containsExactly("MockColumn");

      verify(mockRowOne, never()).removeColumn(anyInt());
      verify(mockRowTwo, never()).removeColumn(anyInt());
    }
  }

  @Test
  public void removeRowIsSuccessful() {

    Column<?> mockColumn = mockColumn("MockColumn");

    Row mockRow = mock(Row.class);

    InMemoryTable table = InMemoryTable.of(mockColumn);

    assertThat(table).isNotNull();
    assertThat(table).isEmpty();
    assertThat(table.getRows().add(mockRow)).isTrue();
    assertThat(table).hasSize(1);
    assertThat(table.removeRow(0)).isTrue();
    assertThat(table).isEmpty();
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void removeInvalidRowThrowsIndexOutOfBoundsException() {

    Column<?> mockColumn = mockColumn("MockColumn");

    InMemoryTable table = InMemoryTable.of(mockColumn);

    assertThat(table).isNotNull();
    assertThat(table).isEmpty();

    table.removeRow(0);
  }

  @Test
  @SuppressWarnings("all")
  public void rowsReturnsTableRows() {

    Column mockColumn = mockColumn("MockColumn");

    Row mockRowOne = mock(Row.class);
    Row mockRowTwo = mock(Row.class);

    InMemoryTable table = spy(InMemoryTable.of(mockColumn));

    assertThat(table).isNotNull();

    doReturn(Arrays.asList(mockRowOne, mockRowTwo)).when(table).getRows();

    assertThat(table.rows()).containsExactly(mockRowOne, mockRowTwo);

    verify(table, times(1)).getRows();
  }

  @Test
  public void rowsReturnsEmptyIterableWhenTableIsEmpty() {

    Column<?> mockColumn = mockColumn("MockColumn");

    InMemoryTable table = spy(InMemoryTable.of(mockColumn));

    assertThat(table).isNotNull();
    assertThat(table).isEmpty();
    assertThat(table.rows()).isEmpty();
  }
}
