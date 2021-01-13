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
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.stream.Collectors;

import org.cp.elements.data.struct.tabular.Column;
import org.cp.elements.data.struct.tabular.Row;
import org.cp.elements.data.struct.tabular.View;
import org.junit.Test;

/**
 * Unit tests for {@link InMemoryTable.InMemoryRow}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.struct.tabular.Column
 * @see org.cp.elements.data.struct.tabular.Row
 * @see org.cp.elements.data.struct.tabular.View
 * @see org.cp.elements.data.struct.tabular.provider.InMemoryTable
 * @see org.cp.elements.data.struct.tabular.provider.InMemoryTable.InMemoryRow
 * @since 1.0.0
 */
public class InMemoryRowTests {

  @SuppressWarnings("unchecked")
  private <T> Column<T> mockColumn(String name) {

    Column<T> mockColumn = mock(Column.class, name);

    when(mockColumn.getName()).thenReturn(name);
    when(mockColumn.getType()).thenReturn((Class) Object.class);

    return mockColumn;
  }

  @Test
  public void constructNewInMemoryRowWithMockRowIsSuccessful() {

    Column mockColumnOne = mockColumn("One");
    Column mockColumnTwo = mockColumn("Two");

    InMemoryTable table = InMemoryTable.of(mockColumnOne, mockColumnTwo);

    assertThat(table).isNotNull();

    Row mockRow = mock(Row.class);

    when(mockRow.getValue(eq(0))).thenReturn("one");
    when(mockRow.getValue(eq(1))).thenReturn("two");

    InMemoryTable.InMemoryRow row = table.newRow(mockRow);

    assertThat(row).isNotNull();
    assertThat(row.getView().orElse(null)).isSameAs(table);
    assertThat(row.<Object>getValue(0)).isEqualTo("one");
    assertThat(row.<Object>getValue(1)).isEqualTo("two");
  }

  @Test
  public void setValueAtIndexIsSuccessful() {

    Column mockColumnOne = mockColumn("One");
    Column mockColumnTwo = mockColumn("Two");

    InMemoryTable table = InMemoryTable.of(mockColumnOne, mockColumnTwo);

    assertThat(table).isNotNull();
    assertThat(table).isEmpty();

    Row mockRow = mock(Row.class);

    when(mockRow.getValue(eq(0))).thenReturn("one");
    when(mockRow.getValue(eq(1))).thenReturn("two");

    assertThat(table.add(mockRow)).isTrue();
    assertThat(table).hasSize(1);

    Row row = table.getRows().get(0);

    assertThat(row).isInstanceOf(InMemoryTable.InMemoryRow.class);
    assertThat(row.getView().orElse(null)).isSameAs(table);
    assertThat(row.setValue(0, "two")).isEqualTo("one");
    assertThat(row.setValue(1, "four")).isEqualTo("two");
    assertThat(row.<Object>getValue(0)).isEqualTo("two");
    assertThat(row.<Object>getValue(1)).isEqualTo("four");
  }

  @Test(expected = UnsupportedOperationException.class)
  public void setViewThrowsUnsupportedOperationException() {

    Column mockColumn = mockColumn("MockColumn");

    InMemoryTable table = InMemoryTable.of(mockColumn);

    assertThat(table).isNotNull();
    assertThat(table).isEmpty();

    Row mockRow = mock(Row.class);

    when(mockRow.getValue(eq(0))).thenReturn("test");

    assertThat(table.add(mockRow)).isTrue();
    assertThat(table).hasSize(1);

    Row row = table.getRows().get(0);

    assertThat(row).isInstanceOf(InMemoryTable.InMemoryRow.class);

    InMemoryTable.InMemoryRow inMemoryRow = (InMemoryTable.InMemoryRow) row;

    try {
      inMemoryRow.setView(mock(View.class));
    }
    catch (UnsupportedOperationException expected) {

      assertThat(expected).hasMessage("The View for this Row [0] cannot be changed");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      assertThat(inMemoryRow.getView().orElse(null)).isSameAs(table);
    }
  }

  @Test
  public void addColumnIsSuccessful() {

    Column mockColumnOne = mockColumn("One");
    Column mockColumnTwo = mockColumn("Two");

    InMemoryTable table = InMemoryTable.of(mockColumnOne);

    assertThat(table).isNotNull();
    assertThat(table).isEmpty();
    assertThat(table.getColumns().stream().map(Column::getName).collect(Collectors.toSet())).containsExactly("One");

    Row mockRowOne = mock(Row.class, "MockRowOne");
    Row mockRowTwo = mock(Row.class, "MockRowTwo");

    when(mockRowOne.getValue(eq(0))).thenReturn("test");
    when(mockRowTwo.getValue(eq(0))).thenReturn("mock");

    assertThat(table.add(mockRowOne)).isTrue();
    assertThat(table.add(mockRowTwo)).isTrue();
    assertThat(table).hasSize(2);

    Row rowOne = table.getRows().get(0);
    Row rowTwo = table.getRows().get(1);

    assertThat(rowOne).isInstanceOf(InMemoryTable.InMemoryRow.class);
    assertThat(rowTwo).isInstanceOf(InMemoryTable.InMemoryRow.class);
    assertThat(rowOne.values()).containsExactly("test");
    assertThat(rowTwo.values()).containsExactly("mock");
    assertThat(table.add(mockColumnTwo)).isTrue();
    assertThat(table.getColumns().stream().map(Column::getName)
      .collect(Collectors.toSet())).containsExactly("One", "Two");
    assertThat(rowOne.values()).containsExactly("test", null);
    assertThat(rowTwo.values()).containsExactly("mock", null);
  }

  @Test
  public void removeFirstColumn() {

    Column mockColumnOne = mockColumn("One");
    Column mockColumnTwo = mockColumn("Two");
    Column mockColumnThree = mockColumn("Three");

    InMemoryTable table = InMemoryTable.of(mockColumnOne, mockColumnTwo, mockColumnThree);

    assertThat(table).isNotNull();
    assertThat(table).isEmpty();
    assertThat(table.getColumns().stream().map(Column::getName).collect(Collectors.toSet()))
      .containsExactly("One", "Two", "Three");

    Row mockRowOne = mock(Row.class, "MockRowOne");
    Row mockRowTwo = mock(Row.class, "MockRowTwo");

    when(mockRowOne.getValue(eq(0))).thenReturn("1A");
    when(mockRowOne.getValue(eq(1))).thenReturn("1B");
    when(mockRowOne.getValue(eq(2))).thenReturn("1C");
    when(mockRowTwo.getValue(eq(0))).thenReturn("2A");
    when(mockRowTwo.getValue(eq(1))).thenReturn("2B");
    when(mockRowTwo.getValue(eq(2))).thenReturn("2C");

    assertThat(table.add(mockRowOne)).isTrue();
    assertThat(table.add(mockRowTwo)).isTrue();
    assertThat(table).hasSize(2);

    Row rowOne = table.getRows().get(0);
    Row rowTwo = table.getRows().get(1);

    assertThat(rowOne).isInstanceOf(InMemoryTable.InMemoryRow.class);
    assertThat(rowTwo).isInstanceOf(InMemoryTable.InMemoryRow.class);
    assertThat(rowOne.values()).containsExactly("1A", "1B", "1C");
    assertThat(rowTwo.values()).containsExactly("2A", "2B", "2C");
    assertThat(table.remove(mockColumnOne)).isTrue();
    assertThat(table.getColumns().stream().map(Column::getName).collect(Collectors.toSet()))
      .containsExactly("Two", "Three");
    assertThat(rowOne.values()).containsExactly("1B", "1C");
    assertThat(rowTwo.values()).containsExactly("2B", "2C");

  }

  @Test
  public void removeLastColumn() {

    Column mockColumnOne = mockColumn("One");
    Column mockColumnTwo = mockColumn("Two");
    Column mockColumnThree = mockColumn("Three");

    InMemoryTable table = InMemoryTable.of(mockColumnOne, mockColumnTwo, mockColumnThree);

    assertThat(table).isNotNull();
    assertThat(table).isEmpty();
    assertThat(table.getColumns().stream().map(Column::getName).collect(Collectors.toSet()))
      .containsExactly("One", "Two", "Three");

    Row mockRowOne = mock(Row.class, "MockRowOne");
    Row mockRowTwo = mock(Row.class, "MockRowTwo");

    when(mockRowOne.getValue(eq(0))).thenReturn("1A");
    when(mockRowOne.getValue(eq(1))).thenReturn("1B");
    when(mockRowOne.getValue(eq(2))).thenReturn("1C");
    when(mockRowTwo.getValue(eq(0))).thenReturn("2A");
    when(mockRowTwo.getValue(eq(1))).thenReturn("2B");
    when(mockRowTwo.getValue(eq(2))).thenReturn("2C");

    assertThat(table.add(mockRowOne)).isTrue();
    assertThat(table.add(mockRowTwo)).isTrue();
    assertThat(table).hasSize(2);

    Row rowOne = table.getRows().get(0);
    Row rowTwo = table.getRows().get(1);

    assertThat(rowOne).isInstanceOf(InMemoryTable.InMemoryRow.class);
    assertThat(rowTwo).isInstanceOf(InMemoryTable.InMemoryRow.class);
    assertThat(rowOne.values()).containsExactly("1A", "1B", "1C");
    assertThat(rowTwo.values()).containsExactly("2A", "2B", "2C");
    assertThat(table.remove(mockColumnThree)).isTrue();
    assertThat(table.getColumns().stream().map(Column::getName).collect(Collectors.toSet()))
      .containsExactly("One", "Two");
    assertThat(rowOne.values()).containsExactly("1A", "1B");
    assertThat(rowTwo.values()).containsExactly("2A", "2B");
  }

  @Test
  public void removeSecondColumn() {

    Column mockColumnOne = mockColumn("One");
    Column mockColumnTwo = mockColumn("Two");
    Column mockColumnThree = mockColumn("Three");

    InMemoryTable table = InMemoryTable.of(mockColumnOne, mockColumnTwo, mockColumnThree);

    assertThat(table).isNotNull();
    assertThat(table).isEmpty();
    assertThat(table.getColumns().stream().map(Column::getName).collect(Collectors.toSet()))
      .containsExactly("One", "Two", "Three");

    Row mockRowOne = mock(Row.class, "MockRowOne");
    Row mockRowTwo = mock(Row.class, "MockRowTwo");

    when(mockRowOne.getValue(eq(0))).thenReturn("1A");
    when(mockRowOne.getValue(eq(1))).thenReturn("1B");
    when(mockRowOne.getValue(eq(2))).thenReturn("1C");
    when(mockRowTwo.getValue(eq(0))).thenReturn("2A");
    when(mockRowTwo.getValue(eq(1))).thenReturn("2B");
    when(mockRowTwo.getValue(eq(2))).thenReturn("2C");

    assertThat(table.add(mockRowOne)).isTrue();
    assertThat(table.add(mockRowTwo)).isTrue();
    assertThat(table).hasSize(2);

    Row rowOne = table.getRows().get(0);
    Row rowTwo = table.getRows().get(1);

    assertThat(rowOne).isInstanceOf(InMemoryTable.InMemoryRow.class);
    assertThat(rowTwo).isInstanceOf(InMemoryTable.InMemoryRow.class);
    assertThat(rowOne.values()).containsExactly("1A", "1B", "1C");
    assertThat(rowTwo.values()).containsExactly("2A", "2B", "2C");
    assertThat(table.remove(mockColumnTwo)).isTrue();
    assertThat(table.getColumns().stream().map(Column::getName).collect(Collectors.toSet()))
      .containsExactly("One", "Three");
    assertThat(rowOne.values()).containsExactly("1A", "1C");
    assertThat(rowTwo.values()).containsExactly("2A", "2C");
  }

  @Test
  public void inMemoryRowValuesAreCorrect() {

    Column mockColumnOne = mockColumn("One");
    Column mockColumnTwo = mockColumn("Two");

    InMemoryTable table = InMemoryTable.of(mockColumnOne, mockColumnTwo);

    assertThat(table).isNotNull();
    assertThat(table).isEmpty();
    assertThat(table.getColumns().stream().map(Column::getName).collect(Collectors.toSet()))
      .containsExactly("One", "Two");

    Row mockRow = mock(Row.class, "MockRow");

    when(mockRow.getValue(eq(0))).thenReturn("one");
    when(mockRow.getValue(eq(1))).thenReturn("two");

    assertThat(table.add(mockRow)).isTrue();
    assertThat(table).hasSize(1);

    Row row = table.getRows().get(0);

    assertThat(row).isNotNull();
    assertThat(row).isNotSameAs(mockRow);
    assertThat(row.getView().orElse(null)).isSameAs(table);
    assertThat(row.index()).isEqualTo(0);

    Object[] rowValues = row.values();

    assertThat(rowValues).isNotNull();
    assertThat(rowValues).hasSize(2);
    assertThat(rowValues).containsExactly("one", "two");
    assertThat(rowValues[0]).isSameAs("one");
    assertThat(rowValues[1]).isSameAs("two");
  }
}
