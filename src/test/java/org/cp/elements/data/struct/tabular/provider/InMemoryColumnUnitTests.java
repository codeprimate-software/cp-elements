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
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Optional;

import org.junit.jupiter.api.Test;

import org.cp.elements.data.struct.tabular.Column;
import org.cp.elements.data.struct.tabular.View;
import org.cp.elements.data.struct.tabular.provider.InMemoryTable.InMemoryColumn;
import org.cp.elements.lang.ThrowableAssertions;
import org.cp.elements.lang.ThrowableOperation;

/**
 * Unit Tests for {@link InMemoryTable.InMemoryColumn}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.struct.tabular.Column
 * @see org.cp.elements.data.struct.tabular.Table
 * @see org.cp.elements.data.struct.tabular.View
 * @see org.cp.elements.data.struct.tabular.provider.InMemoryTable
 * @see org.cp.elements.data.struct.tabular.provider.InMemoryTable.InMemoryColumn
 * @since 1.0.0
 */
public class InMemoryColumnUnitTests {

  @SuppressWarnings("unchecked")
  private <T> Column<T> mockColumn(String name) {

    Column<T> mockColumn = mock(Column.class, name);

    doReturn(name).when(mockColumn).getName();
    doReturn(Object.class).when(mockColumn).getType();

    return mockColumn;
  }

  @Test
  public void constructInMemoryColumn() {

    Column<?> mockColumn = mockColumn("MockColumn");

    InMemoryTable table = mock(InMemoryTable.class);

    InMemoryColumn<?> column = table.new InMemoryColumn<>(mockColumn);

    assertThat(column).isNotNull();
    assertThat(column.getName()).isEqualTo("MockColumn");
    assertThat(column.getType()).isEqualTo(Object.class);
    assertThat(column.getView()).isPresent();
    assertThat(column.getView().orElse(null)).isEqualTo(table);

    verify(mockColumn, times(1)).getName();
    verify(mockColumn, times(1)).getType();
    verify(mockColumn, times(1)).getAlias();
    verify(mockColumn, times(1)).getDefaultValue();
    verify(mockColumn, times(1)).getDescription();
    verifyNoMoreInteractions(mockColumn);
    verifyNoMoreInteractions(table);
  }

  @Test
  public void constructInMemoryColumnWithNullColumnThrowsIllegalArgumentException() {

    InMemoryTable table = mock(InMemoryTable.class);

    assertThatIllegalArgumentException()
      .isThrownBy(() -> table.new InMemoryColumn<>(null))
      .withMessage("The Column to copy is required")
      .withNoCause();

    verifyNoInteractions(table);
  }

  @Test
  public void viewIsSameAsContainingInMemoryTable() {

    InMemoryTable table = mock(InMemoryTable.class);

    Column<?> mockColumn = mockColumn("MockColumn");
    Column<?> column = table.new InMemoryColumn<>(mockColumn);

    assertThat(column).isNotNull();
    assertThat(column.getName()).isEqualTo("MockColumn");
    assertThat(column.getType()).isEqualTo(Object.class);
    assertThat(column.getView()).isPresent();
    assertThat(column.getView().orElse(null)).isSameAs(table);

    verifyNoInteractions(table);
  }

  @Test
  public void setViewThrowsUnsupportedOperationException() {

    Column<?> mockColumn = mockColumn("MockColumn");

    InMemoryTable table = mock(InMemoryTable.class);

    View mockView = mock(View.class);

    assertThat(table).isNotNull();

    InMemoryColumn<?> column = table.new InMemoryColumn<>(mockColumn);

    assertThat(column).isNotNull();
    assertThat(column.getName()).isEqualTo("MockColumn");
    assertThat(column.getView()).isPresent();
    assertThat(column.getView().orElse(null)).isEqualTo(table);

    ThrowableAssertions.assertThatUnsupportedOperationException()
      .isThrownBy(ThrowableOperation.fromRunnable(() -> column.setView(mockView)))
      .havingMessage("The View for this Column [MockColumn] cannot be changed")
      .withNoCause();

    assertThat(column.getView()).isPresent();
    assertThat(column.getView().orElse(null)).isEqualTo(table);

    verifyNoInteractions(table, mockView);
  }

  @Test
  public void equalsForEqualNamedColumns() {

    InMemoryTable table = mock(InMemoryTable.class);

    Column<?> mockColumn = mockColumn("MockColumn");
    Column<?> column = table.new InMemoryColumn<>(mockColumn);

    assertThat(column).isNotNull();
    assertThat(column.getView()).isPresent();
    assertThat(column).isEqualTo(mockColumn);

    verify(mockColumn, atLeastOnce()).getName();
  }

  @Test
  public void equalsForEqualViewedColumns() {

    InMemoryTable table = mock(InMemoryTable.class);

    Column<?> mockColumn = mockColumn("MockColumn");

    doReturn(Optional.of(table)).when(mockColumn).getView();

    Column<?> column = table.new InMemoryColumn<>(mockColumn);

    assertThat(column).isNotNull();
    assertThat(column.getView()).isPresent();
    assertThat(column).isEqualTo(mockColumn);

    verify(mockColumn, atLeastOnce()).getName();
  }

  @Test
  public void equalsForUnequalNamedColumns() {

    InMemoryTable table = mock(InMemoryTable.class);

    Column<?> mockColumnOne = mockColumn("MockColumn");
    Column<?> mockColumnTwo = mockColumn("TestColumn");
    Column<?> column = table.new InMemoryColumn<>(mockColumnOne);

    assertThat(column).isNotNull();
    assertThat(column.getView()).isPresent();
    assertThat(column).isNotEqualTo(mockColumnTwo);

    verify(mockColumnOne, atLeastOnce()).getName();
    verify(mockColumnOne, never()).getView();
    verify(mockColumnTwo, atLeastOnce()).getName();
    verify(mockColumnTwo, never()).getView();
  }

  @Test
  public void equalsForUnequalViewedColumns() {

    View mockView = mock(View.class);
    InMemoryTable table = mock(InMemoryTable.class);

    Column<?> mockColumn = mockColumn("MockColumn");

    doReturn(Optional.of(mockView)).when(mockColumn).getView();

    Column<?> column = table.new InMemoryColumn<>(mockColumn);

    assertThat(column).isNotNull();
    assertThat(column.getView()).isPresent();
    assertThat(column).isNotEqualTo(mockColumn);

    verify(mockColumn, atLeastOnce()).getName();
    verify(mockColumn, times(1)).getView();
  }

  @Test
  public void hashCodeIsNotZero() {

    InMemoryTable table = mock(InMemoryTable.class);

    Column<?> mockColumn = mockColumn("MockColumn");
    Column<?> column = table.new InMemoryColumn<>(mockColumn);

    assertThat(column).isNotNull();
    assertThat(column.getName()).isEqualTo("MockColumn");
    assertThat(column.getView()).isPresent();
    assertThat(column.getView().orElse(null)).isEqualTo(table);

    int hashCode = column.hashCode();

    assertThat(hashCode).isNotZero();
    assertThat(column.hashCode()).isEqualTo(hashCode);
  }

  @Test
  public void toStringEqualsName() {

    InMemoryTable table = mock(InMemoryTable.class);

    Column<?> mockColumn = mockColumn("MockColumn");
    Column<?> column = table.new InMemoryColumn<>(mockColumn);

    assertThat(column).isNotNull();
    assertThat(column.getName()).isEqualTo("MockColumn");
    assertThat(column.toString()).isEqualTo("MockColumn");
  }
}
