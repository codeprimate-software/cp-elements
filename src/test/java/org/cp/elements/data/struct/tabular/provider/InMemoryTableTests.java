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

package org.cp.elements.data.struct.tabular.provider;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.StreamSupport;

import org.cp.elements.data.struct.tabular.Column;
import org.junit.Test;

/**
 * Unit tests for {@link InMemoryTable}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.struct.tabular.provider.InMemoryTable
 * @since 1.0.0
 */
public class InMemoryTableTests {

  @SuppressWarnings("unchecked")
  private <T> Column<T> mockColumn(String name) {

    Column<T> mockColumn = mock(Column.class, name);

    when(mockColumn.getName()).thenReturn(name);
    when(mockColumn.getType()).thenReturn((Class) Object.class);

    return mockColumn;
  }

  @Test
  @SuppressWarnings("unchecked")
  public void constructNewInMemoryTableIsSuccessful() {

    Column<?> mockColumnOne = mockColumn("One");
    Column<?> mockColumnTwo = mockColumn("Two");

    InMemoryTable table = new InMemoryTable(mockColumnOne, mockColumnTwo);

    assertThat(table).isNotNull();
    assertThat(table.rows()).isEmpty();

    AtomicInteger index = new AtomicInteger(0);

    StreamSupport.stream(table.columns().spliterator(), false).forEach(column -> {

      assertThat(column.getName())
        .isEqualTo(Arrays.asList(mockColumnOne, mockColumnTwo).get(index.getAndIncrement()).getName());

      assertThat(column.getView().orElse(null)).isEqualTo(table);
    });

    assertThat(index.get()).isEqualTo(2);
  }

  @Test(expected = IllegalArgumentException.class)
  public void constructNewInMemoryTableWithEmptyCoumnsThrowsIllegalArgumentException() {

    try {
      new InMemoryTable();
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Columns are required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void constructNewInMemoryTableWithNullCoumnsThrowsIllegalArgumentException() {

    try {
      new InMemoryTable((Column[]) null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Columns are required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }
}
