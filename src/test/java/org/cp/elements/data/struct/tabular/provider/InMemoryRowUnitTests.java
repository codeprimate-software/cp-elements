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
import static org.cp.elements.lang.ThrowableAssertions.assertThatUnsupportedOperationException;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;

import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Test;

import org.cp.elements.data.struct.tabular.Row;
import org.cp.elements.data.struct.tabular.View;
import org.cp.elements.data.struct.tabular.provider.InMemoryTable.InMemoryRow;
import org.cp.elements.lang.ThrowableOperation;

/**
 * Unit Tests for {@link InMemoryTable.InMemoryRow}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.struct.tabular.Row
 * @see org.cp.elements.data.struct.tabular.Table
 * @see org.cp.elements.data.struct.tabular.View
 * @see org.cp.elements.data.struct.tabular.provider.InMemoryTable
 * @see org.cp.elements.data.struct.tabular.provider.InMemoryTable.InMemoryRow
 * @since 1.0.0
 */
public class InMemoryRowUnitTests {

  @Test
  public void constructNewInMemoryRowWithMockRowIsSuccessful() {

    InMemoryTable table = mock(InMemoryTable.class);

    doAnswer(invocation -> invocation.getArgument(0)).when(table).validateValue(any());

    Row mockRow = mock(Row.class);

    doReturn("ONE").when(mockRow).getValue(eq(0));
    doReturn("TWO").when(mockRow).getValue(eq(1));

    InMemoryTable.InMemoryRow row = table.new InMemoryRow(mockRow) {

      @Override
      protected int getColumnSize(InMemoryTable table) {
        return 2;
      }
    };

    assertThat(row).isNotNull();
    assertThat(row).isNotSameAs(mockRow);
    assertThat(row.getView()).isPresent();
    assertThat(row.getView().orElse(null)).isSameAs(table);
    assertThat(row.<Object>getValue(0)).isEqualTo("ONE");
    assertThat(row.<Object>getValue(1)).isEqualTo("TWO");
  }

  @Test
  public void setAndGetValueAtIndexIsSuccessful() {

    InMemoryTable table = mock(InMemoryTable.class);

    doAnswer(invocation -> invocation.getArgument(0)).when(table).validateValue(any());

    Row mockRow = mock(Row.class);

    doReturn("ONE").when(mockRow).getValue(eq(0));
    doReturn("TWO").when(mockRow).getValue(eq(1));

    InMemoryRow row = table.new InMemoryRow(mockRow) {

      @Override
      protected int getColumnSize(InMemoryTable table) {
        return 2;
      }
    };

    assertThat(row).isNotNull();
    assertThat(row).isNotEqualTo(mockRow);
    assertThat(row.getView()).isPresent();
    assertThat(row.getView().orElse(null)).isSameAs(table);
    assertThat(row.setValue(0, "TWO")).isEqualTo("ONE");
    assertThat(row.setValue(1, "FOUR")).isEqualTo("TWO");
    assertThat(row.<Object>getValue(0)).isEqualTo("TWO");
    assertThat(row.<Object>getValue(1)).isEqualTo("FOUR");
  }

  @Test
  public void setViewThrowsUnsupportedOperationException() {

    View mockView = mock(View.class);

    Row mockRow = mock(Row.class);

    InMemoryTable table = mock(InMemoryTable.class);

    InMemoryRow row = table.new InMemoryRow(mockRow);

    assertThat(row).isNotNull();
    assertThat(row.getView()).isPresent();
    assertThat(row.getView().orElse(null)).isEqualTo(table);

    assertThatUnsupportedOperationException()
      .isThrownBy(ThrowableOperation.fromRunnable(() -> row.setView(mockView)))
      .havingMessage("The View for this Row [0] cannot be changed")
      .withNoCause();

    assertThat(row.getView()).isPresent();
    assertThat(row.getView().orElse(null)).isEqualTo(table);
  }

  @Test
  public void addColumnIsSuccessful() {

    InMemoryTable table = mock(InMemoryTable.class);

    AtomicInteger columnSize = new AtomicInteger(2);

    Row mockRow = mock(Row.class);

    InMemoryRow row = table.new InMemoryRow(mockRow) {

      @Override
      int getColumnSize(InMemoryTable table) {
        return columnSize.get();
      }
    };

    assertThat(row).isNotNull();
    assertThat(row.getView()).isPresent();
    assertThat(row.getView().orElse(null)).isEqualTo(table);
    assertThat(row.values()).hasSize(2);

    columnSize.incrementAndGet();
    row.addColumn();

    assertThat(row.values()).hasSize(3);
  }

  @Test
  public void removeFirstColumn() {

    InMemoryTable table = mock(InMemoryTable.class);

    doAnswer(invocation -> invocation.getArgument(0)).when(table).validateValue(any());

    Row mockRow = mock(Row.class);

    doReturn("A").when(mockRow).getValue(eq(0));
    doReturn("B").when(mockRow).getValue(eq(1));
    doReturn("C").when(mockRow).getValue(eq(2));

    InMemoryRow row = table.new InMemoryRow(mockRow) {

      @Override
      int getColumnSize(InMemoryTable table) {
        return 3;
      }
    };

    assertThat(row).isNotNull();
    assertThat(row.getView()).isPresent();
    assertThat(row.getView().orElse(null)).isEqualTo(table);
    assertThat(row.values()).hasSize(3);
    assertThat(row.values()).containsExactly("A", "B", "C");

    row.removeColumn(0);

    assertThat(row.values()).hasSize(2);
    assertThat(row.values()).containsExactly("B", "C");
  }

  @Test
  public void removeLastColumn() {

    InMemoryTable table = mock(InMemoryTable.class);

    doAnswer(invocation -> invocation.getArgument(0)).when(table).validateValue(any());

    Row mockRow = mock(Row.class);

    doReturn("A").when(mockRow).getValue(eq(0));
    doReturn("B").when(mockRow).getValue(eq(1));
    doReturn("C").when(mockRow).getValue(eq(2));

    InMemoryRow row = table.new InMemoryRow(mockRow) {

      @Override
      int getColumnSize(InMemoryTable table) {
        return 3;
      }
    };

    assertThat(row).isNotNull();
    assertThat(row.getView()).isPresent();
    assertThat(row.getView().orElse(null)).isEqualTo(table);
    assertThat(row.values()).hasSize(3);
    assertThat(row.values()).containsExactly("A", "B", "C");

    row.removeColumn(2);

    assertThat(row.values()).hasSize(2);
    assertThat(row.values()).containsExactly("A", "B");
  }

  @Test
  public void removeSecondColumn() {

    InMemoryTable table = mock(InMemoryTable.class);

    doAnswer(invocation -> invocation.getArgument(0)).when(table).validateValue(any());

    Row mockRow = mock(Row.class);

    doReturn("A").when(mockRow).getValue(eq(0));
    doReturn("B").when(mockRow).getValue(eq(1));
    doReturn("C").when(mockRow).getValue(eq(2));

    InMemoryRow row = table.new InMemoryRow(mockRow) {

      @Override
      int getColumnSize(InMemoryTable table) {
        return 3;
      }
    };

    assertThat(row).isNotNull();
    assertThat(row.getView()).isPresent();
    assertThat(row.getView().orElse(null)).isEqualTo(table);
    assertThat(row.values()).hasSize(3);
    assertThat(row.values()).containsExactly("A", "B", "C");

    row.removeColumn(1);

    assertThat(row.values()).hasSize(2);
    assertThat(row.values()).containsExactly("A", "C");
  }

  @Test
  public void inMemoryRowValuesAreCorrect() {

    InMemoryTable table = mock(InMemoryTable.class);

    doAnswer(invocation -> invocation.getArgument(0)).when(table).validateValue(any());

    Row mockRow = mock(Row.class);

    doReturn("ONE").when(mockRow).getValue(eq(0));
    doReturn("TWO").when(mockRow).getValue(eq(1));

    InMemoryTable.InMemoryRow row = table.new InMemoryRow(mockRow) {

      @Override
      protected int getColumnSize(InMemoryTable table) {
        return 2;
      }
    };

    assertThat(row).isNotNull();
    assertThat(row).isNotSameAs(mockRow);
    assertThat(row.getView()).isPresent();
    assertThat(row.getView().orElse(null)).isSameAs(table);
    assertThat(row.<Object>getValue(0)).isEqualTo("ONE");
    assertThat(row.<Object>getValue(1)).isEqualTo("TWO");
    assertThat(row.values()).containsExactly("ONE", "TWO");
  }
}
