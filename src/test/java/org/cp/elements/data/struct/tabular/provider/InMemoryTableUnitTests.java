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
import static org.assertj.core.api.Assertions.assertThatIndexOutOfBoundsException;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.withSettings;

import java.util.Arrays;
import java.util.Collections;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import org.junit.Test;

import org.cp.elements.data.struct.tabular.Column;
import org.cp.elements.data.struct.tabular.Row;
import org.cp.elements.util.stream.StreamUtils;

import org.mockito.ArgumentMatchers;
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
    doReturn(Optional.of(name)).when(mockColumn).getAlias();
    doReturn(Optional.empty()).when(mockColumn).getDefaultValue();
    doReturn(Optional.of(String.format("Mock Column [%s]", name))).when(mockColumn).getDescription();
    doReturn(Optional.empty()).when(mockColumn).getView();

    return mockColumn;
  }

  private void verifyMockColumnInteractions(Column<?>... mockColumns) {

    Arrays.stream(mockColumns).forEach(mockColumn -> {
      verify(mockColumn, atLeastOnce()).getName();
      verify(mockColumn, times(1)).getType();
      verify(mockColumn, times(1)).getAlias();
      verify(mockColumn, times(1)).getDefaultValue();
      verify(mockColumn, times(1)).getDescription();
      //verify(mockColumn).getView();
      verifyNoMoreInteractions(mockColumn);
    });
  }

  @Test
  public void constructNewInMemoryTable() {

    Column<?> mockColumnOne = mockColumn("ONE");
    Column<?> mockColumnTwo = mockColumn("TWO");

    InMemoryTable table = new InMemoryTable(mockColumnOne, mockColumnTwo);

    assertThat(table).isNotNull();
    assertThat(table).isEmpty();
    assertThat(table.getColumns()).containsExactly(mockColumnOne, mockColumnTwo);
    assertThat(table.getRows()).isEmpty();

    AtomicInteger index = new AtomicInteger(0);

    StreamUtils.stream(table.columns()).forEach(column -> {

      assertThat(column.getName())
        .isEqualTo(Arrays.asList(mockColumnOne, mockColumnTwo).get(index.getAndIncrement()).getName());

      assertThat(column.getView()).isPresent();
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
    assertThat(table.getColumns()).containsExactly(mockColumnOne, mockColumnTwo);
    assertThat(table.getRows()).isEmpty();

    assertThat(table.getColumns().stream()
      .filter(column -> column.getView().isPresent()
        && column.getView().orElse(null).equals(table))
      .map(Column::getName)
      .collect(Collectors.toList()))
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
    assertThat(table.getColumns()).containsExactly(mockColumnOne, mockColumnTwo);
    assertThat(table.getRows()).isEmpty();

    assertThat(table.getColumns().stream()
      .filter(column -> column.getView().isPresent()
        && column.getView().orElse(null).equals(table))
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

    doReturn(Arrays.asList(mockRowOne, mockRowTwo)).when(table).getRows();

    assertThat(table).isNotNull();
    assertThat(table.getColumns()).containsExactly(mockColumnOne);
    assertThat(table.getRows()).containsExactly(mockRowOne, mockRowTwo);

    assertThat(table.getColumns().stream()
      //.filter(column -> column.getView().isPresent() && column.getView().orElse(null).equals(table))
      .map(Column::getName)
      .collect(Collectors.toList()))
      .containsExactly("ONE");

    assertThat(table.add(mockColumnTwo)).isTrue();

    assertThat(table.getColumns().stream()
      //.filter(column -> column.getView().isPresent() && column.getView().orElse(null).equals(table))
      .map(Column::getName)
      .collect(Collectors.toList()))
      .containsExactly("ONE", "TWO");

    Arrays.asList(mockRowOne, mockRowTwo).forEach(mockRow -> {
      verify(mockRow, times(1)).addColumn();
    });

    verify(table, times(1)).add(eq(mockColumnTwo));
    verify(table, times(1)).validateColumn(eq(mockColumnTwo));
    verify(table, times(1)).newColumn(eq(mockColumnTwo));
    verify(table, times(4)).getColumns();
    verify(table, times(1)).iterator();
    verify(table, times(1)).rows();
    verify(table, times(2)).getRows();
    verify(mockColumnOne, atLeastOnce()).getView();
    verifyMockColumnInteractions(mockColumnOne, mockColumnTwo);
    verifyNoMoreInteractions(table, mockRowOne, mockRowTwo);
  }

  @Test
  public void addNullColumnThrowsIllegalArgumentException() {

    Column<?> mockColumnOne = mockColumn("ONE");

    InMemoryTable.InMemoryRow mockRowOne = mock(InMemoryTable.InMemoryRow.class);
    InMemoryTable.InMemoryRow mockRowTwo = mock(InMemoryTable.InMemoryRow.class);

    doNothing().when(mockRowOne).addColumn();
    doNothing().when(mockRowTwo).addColumn();

    InMemoryTable table = spy(InMemoryTable.of(mockColumnOne));

    doReturn(Arrays.asList(mockRowOne, mockRowTwo)).when(table).getRows();

    assertThat(table).isNotNull();
    assertThat(table.getColumns()).containsExactly(mockColumnOne);
    assertThat(table.getRows()).containsExactly(mockRowOne, mockRowTwo);

    assertThat(table.getColumns().stream()
      .map(Column::getName)
      .collect(Collectors.toList()))
      .containsExactly("ONE");

    assertThatIllegalArgumentException()
      .isThrownBy(() -> table.add((Column<?>) null))
      .withMessage("Column is required")
        .withNoCause();

    assertThat(table.getColumns().stream()
      .map(Column::getName)
      .collect(Collectors.toList()))
      .containsExactly("ONE");

    verify(table, times(1)).add(ArgumentMatchers.<Column<?>>isNull());
    verify(table, times(4)).getColumns();
    verify(table, times(1)).getRows();
    verify(table, times(1)).validateColumn(isNull());
    verify(mockColumnOne, times(4)).getView();
    verifyMockColumnInteractions(mockColumnOne);
    verifyNoInteractions(mockRowOne, mockRowTwo);
    verifyNoMoreInteractions(table);
  }

  @Test
  public void addRowIsSuccessful() {

    Column<?> mockColumn = mockColumn("MockColumn");

    InMemoryTable table = InMemoryTable.of(mockColumn);

    assertThat(table).isNotNull();
    assertThat(table.getColumns()).containsExactly(mockColumn);
    assertThat(table.getRows()).isEmpty();

    assertThat(table.getColumns().stream()
      .filter(column -> column.getView().isPresent() && column.getView().orElse(null).equals(table))
      .map(Column::getName)
      .collect(Collectors.toSet()))
      .containsExactly("MockColumn");

    Row mockRow = mock(Row.class);

    doReturn("test").when(mockRow).getValue(eq(0));

    assertThat(table.add(mockRow)).isTrue();

    assertThat(table.getColumns()).containsExactly(mockColumn);
    assertThat(table.getRows()).hasSize(1);
    assertThat(table.getRow(0).<String>getValue(mockColumn)).isEqualTo("test");

    verify(mockColumn, atLeastOnce()).getView();
    verify(mockRow, times(1)).getValue(eq(0));
    verifyMockColumnInteractions(mockColumn);
    verifyNoMoreInteractions(mockRow);
  }

  @Test
  public void addNullRowThrowsIllegalArgumentException() {

    Column<?> mockColumn = mockColumn("MockColumn");

    InMemoryTable table = InMemoryTable.of(mockColumn);

    assertThat(table).isNotNull();
    assertThat(table.getColumns()).containsExactly(mockColumn);
    assertThat(table.getRows()).isEmpty();

    assertThat(table.getColumns().stream()
      .filter(column -> column.getView().isPresent() && column.getView().orElse(null).equals(table))
      .map(Column::getName)
      .collect(Collectors.toSet()))
      .containsExactly("MockColumn");

    assertThatIllegalArgumentException()
      .isThrownBy(() -> table.add((Row) null))
      .withMessage("Row is required")
      .withNoCause();

    assertThat(table.getColumns()).containsExactly(mockColumn);
    assertThat(table).hasSize(0);

    verify(mockColumn, atLeastOnce()).getView();
    verifyMockColumnInteractions(mockColumn);
  }

  @Test
  public void columnsReturnsTableColumns() {

    Column<?> mockColumnOne = mockColumn("ONE");
    Column<?> mockColumnTwo = mockColumn("TWO");

    InMemoryTable table = InMemoryTable.of(mockColumnOne, mockColumnTwo);

    assertThat(table).isNotNull();
    assertThat(table.getColumns()).containsExactly(mockColumnOne, mockColumnTwo);
    assertThat(table).isEmpty();

    InMemoryTable tableSpy = spy(table);

    assertThat(StreamUtils.stream(tableSpy.columns())
      .filter(column -> column.getView().isPresent() && column.getView().orElse(null).equals(table))
      .map(Column::getName)
      .collect(Collectors.toSet()))
      .containsExactly("ONE", "TWO");

    verify(tableSpy, times(1)).columns();
    verify(tableSpy, times(1)).getColumns();
    verifyNoMoreInteractions(tableSpy);
  }

  @Test
  public void iteratorReturnsNoRowsWhenTableIsEmpty() {

    Column<?> mockColumn = mockColumn("MockColumn");

    InMemoryTable table = InMemoryTable.of(mockColumn);

    assertThat(table).isEmpty();
    assertThat(table).isNotNull();
  }

  @Test
  @SuppressWarnings("all")
  public void iteratorReturnsTableRows() {

    Column mockColumn = mockColumn("MockColumn");

    Row mockRowOne = mock(Row.class);
    Row mockRowTwo = mock(Row.class);

    InMemoryTable table = spy(InMemoryTable.of(mockColumn));

    doReturn(Arrays.asList(mockRowOne, mockRowTwo)).when(table).getRows();

    assertThat(table).isNotNull();
    assertThat(table).containsExactly(mockRowOne, mockRowTwo);

    verify(table, times(1)).iterator();
    verify(table, times(1)).rows();
    verify(table, times(1)).getRows();
    verify(table, times(1)).spliterator();
    verifyNoMoreInteractions(table);
  }

  @Test
  public void removeColumnIsSuccessful() {

    Column<?> mockColumnOne = mockColumn("ONE");
    Column<?> mockColumnTwo = mockColumn("TWO");

    InMemoryTable.InMemoryRow mockRowOne = mock(InMemoryTable.InMemoryRow.class);
    InMemoryTable.InMemoryRow mockRowTwo = mock(InMemoryTable.InMemoryRow.class);

    doNothing().when(mockRowOne).removeColumn(anyInt());
    doNothing().when(mockRowTwo).removeColumn(anyInt());

    InMemoryTable table = spy(InMemoryTable.of(mockColumnOne, mockColumnTwo));

    doReturn(Arrays.asList(mockRowOne, mockRowTwo)).when(table).getRows();

    assertThat(table).isNotNull();
    assertThat(table.getColumns().stream()
      .map(Column::getName)
      .collect(Collectors.toSet()))
      .containsExactly("ONE", "TWO");
    assertThat(table).hasSize(2);

    assertThat(table.removeColumn(0)).isTrue();

    assertThat(table.getColumns().stream()
      .map(Column::getName)
      .collect(Collectors.toSet()))
      .containsExactly("TWO");
    assertThat(table).hasSize(2);

    verify(table, times(1)).removeColumn(eq(0));
    verify(table, times(3)).getColumns();
    verify(table, times(3)).iterator();
    verify(table, times(3)).rows();
    verify(table, times(3)).getRows();
    verify(table, times(2)).spliterator();
    verify(mockRowOne, times(1)).removeColumn(eq(0));
    verify(mockRowTwo, times(1)).removeColumn(eq(0));
    verifyMockColumnInteractions(mockColumnOne, mockColumnTwo);
    verifyNoMoreInteractions(table, mockRowOne, mockRowTwo);
  }

  @Test
  public void removeInvalidColumnThrowsIndexOutOfBoundsException() {

    Column<?> mockColumn = mockColumn("MockColumn");

    InMemoryTable.InMemoryRow mockRowOne = mock(InMemoryTable.InMemoryRow.class);
    InMemoryTable.InMemoryRow mockRowTwo = mock(InMemoryTable.InMemoryRow.class);

    doNothing().when(mockRowOne).removeColumn(anyInt());
    doNothing().when(mockRowTwo).removeColumn(anyInt());

    InMemoryTable table = spy(InMemoryTable.of(mockColumn));

    doReturn(Arrays.asList(mockRowOne, mockRowTwo)).when(table).getRows();

    assertThat(table).isNotNull();
    assertThat(table).hasSize(2);

    assertThat(table.getColumns().stream()
      .map(Column::getName)
      .collect(Collectors.toSet()))
      .containsExactly("MockColumn");

    assertThatIndexOutOfBoundsException()
      .isThrownBy(() -> table.removeColumn(1))
      .withNoCause();

    assertThat(table).hasSize(2);

    assertThat(table.getColumns().stream()
      .map(Column::getName)
      .collect(Collectors.toSet()))
      .containsExactly("MockColumn");

    verify(table, times(1)).removeColumn(eq(1));
    verify(table, times(3)).getColumns();
    verify(table, times(2)).iterator();
    verify(table, times(2)).rows();
    verify(table, times(2)).getRows();
    verify(table, times(2)).spliterator();
    verify(mockRowOne, never()).removeColumn(anyInt());
    verify(mockRowTwo, never()).removeColumn(anyInt());
    verifyNoMoreInteractions(table, mockRowOne, mockRowTwo);
  }

  @Test
  public void removeRowIsSuccessful() {

    Column<?> mockColumn = mockColumn("MockColumn");

    Row mockRow = mock(Row.class);

    InMemoryTable table = InMemoryTable.of(mockColumn);

    assertThat(table).isNotNull();
    assertThat(table.getColumns()).containsExactly(mockColumn);
    assertThat(table).isEmpty();
    assertThat(table.getRows().add(mockRow)).isTrue();
    assertThat(table).hasSize(1);
    assertThat(table.removeRow(0)).isTrue();
    assertThat(table.getColumns()).containsExactly(mockColumn);
    assertThat(table).isEmpty();

    verify(mockColumn, atLeastOnce()).getView();
    verifyMockColumnInteractions(mockColumn);
    verifyNoInteractions(mockRow);
  }

  @Test
  public void removeInvalidRowThrowsIndexOutOfBoundsException() {

    Column<?> mockColumn = mockColumn("MockColumn");

    InMemoryTable table = InMemoryTable.of(mockColumn);

    assertThat(table).isNotNull();
    assertThat(table).isEmpty();

    assertThatIndexOutOfBoundsException()
      .isThrownBy(() -> table.removeRow(0))
      .withNoCause();
  }

  @Test
  public void rowsReturnsTableRows() {

    Column<?> mockColumn = mockColumn("MockColumn");

    Row mockRowOne = mock(Row.class);
    Row mockRowTwo = mock(Row.class);

    InMemoryTable table = spy(InMemoryTable.of(mockColumn));

    doReturn(Arrays.asList(mockRowOne, mockRowTwo)).when(table).getRows();

    assertThat(table).isNotNull();
    assertThat(table.rows()).containsExactly(mockRowOne, mockRowTwo);

    verify(table, times(1)).rows();
    verify(table, times(1)).getRows();
    verifyNoMoreInteractions(table);
  }

  @Test
  public void rowsReturnsEmptyIterableWhenTableIsEmpty() {

    Column<?> mockColumn = mockColumn("MockColumn");

    InMemoryTable table = spy(InMemoryTable.of(mockColumn));

    assertThat(table).isNotNull();
    assertThat(table).isEmpty();
    assertThat(table.rows()).isEmpty();

    verify(table, times(2)).rows();
    verify(table, times(2)).getRows();
    verify(table, times(1)).iterator();
    verifyNoMoreInteractions(table);
  }

  @Test
  public void validateNonNullColumn() {

    InMemoryTable table = mock(InMemoryTable.class);

    doCallRealMethod().when(table).validateColumn(any());

    Column<?> mockColumn = mockColumn("MockColun");

    assertThat(table.validateColumn(mockColumn)).isSameAs(mockColumn);

    verify(table, times(1)).validateColumn(eq(mockColumn));
    verifyNoMoreInteractions(table);
    verifyNoInteractions(mockColumn);
  }

  @Test
  public void validateNullColumn() {

    InMemoryTable table = mock(InMemoryTable.class);

    doCallRealMethod().when(table).validateColumn(any());

    assertThatIllegalArgumentException()
      .isThrownBy(() -> table.validateColumn(null))
      .withMessage("Column is required")
      .withNoCause();

    verify(table, times(1)).validateColumn(isNull());
    verifyNoMoreInteractions(table);
  }

  @Test
  public void validateNonNullRow() {

    InMemoryTable table = mock(InMemoryTable.class);

    doCallRealMethod().when(table).validateRow(any());

    Row mockRow= mock(Row.class);

    assertThat(table.validateRow(mockRow)).isSameAs(mockRow);

    verify(table, times(1)).validateRow(eq(mockRow));
    verifyNoMoreInteractions(table);
    verifyNoInteractions(mockRow);
  }

  @Test
  public void validateNullRow() {

    InMemoryTable table = mock(InMemoryTable.class);

    doCallRealMethod().when(table).validateRow(any());

    assertThatIllegalArgumentException()
      .isThrownBy(() -> table.validateRow(null))
      .withMessage("Row is required")
      .withNoCause();

    verify(table, times(1)).validateRow(isNull());
    verifyNoMoreInteractions(table);
  }

  @Test
  public void validateNonNullValue() {

    InMemoryTable table = mock(InMemoryTable.class);

    doCallRealMethod().when(table).validateValue(any());

    assertThat(table.validateValue("test")).isEqualTo("test");

    verify(table, times(1)).validateValue(eq("test"));
    verifyNoMoreInteractions(table);
  }

  @Test
  public void validateNullValue() {

    InMemoryTable table = mock(InMemoryTable.class);

    doCallRealMethod().when(table).validateValue(any());

    assertThat(table.validateValue(null)).isNull();

    verify(table, times(1)).validateValue(isNull());
    verifyNoMoreInteractions(table);
  }
}
