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
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.cp.elements.data.struct.tabular.Column;
import org.cp.elements.data.struct.tabular.View;
import org.junit.Test;

/**
 * Unit tests for {@link InMemoryTable.InMemoryColumn}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.struct.tabular.Column
 * @see org.cp.elements.data.struct.tabular.View
 * @see org.cp.elements.data.struct.tabular.provider.InMemoryTable
 * @see org.cp.elements.data.struct.tabular.provider.InMemoryTable.InMemoryColumn
 * @since 1.0.0
 */
public class InMemoryColumnTests {

  @SuppressWarnings("unchecked")
  private <T> Column<T> mockColumn(String name) {

    Column<T> mockColumn = mock(Column.class, name);

    when(mockColumn.getName()).thenReturn(name);
    when(mockColumn.getType()).thenReturn((Class) Object.class);

    return mockColumn;
  }

  @Test
  public void viewIsSameAsContainingInMemoryTable() {

    InMemoryTable table = InMemoryTable.of(mockColumn("MockColumn"));

    assertThat(table).isNotNull();
    assertThat(table.getColumns()).hasSize(1);

    Column<?> tableColumn = table.getColumns().get(0);

    assertThat(tableColumn).isNotNull();
    assertThat(tableColumn.getName()).isEqualTo("MockColumn");
    assertThat(tableColumn.getView().orElse(null)).isSameAs(table);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void setViewThrowsUnSupportedOperationException() {

    InMemoryTable table = InMemoryTable.of(mockColumn("MockColumn"));

    assertThat(table).isNotNull();
    assertThat(table.getColumns()).hasSize(1);

    Column<?> tableColumn = table.getColumns().get(0);

    assertThat(tableColumn).isInstanceOf(InMemoryTable.InMemoryColumn.class);
    assertThat(tableColumn.getName()).isEqualTo("MockColumn");

    try {
      ((InMemoryTable.InMemoryColumn) tableColumn).setView(mock(View.class));
    }
    catch (UnsupportedOperationException expected) {

      assertThat(expected).hasMessage("The View for this Column [MockColumn] cannot be changed");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      assertThat(tableColumn.getView().orElse(null)).isSameAs(table);
    }
  }
}
