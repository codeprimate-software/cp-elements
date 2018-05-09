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
import static org.mockito.Mockito.when;

import org.junit.Test;

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
  public void removeColumnByNonExistingNameReturnsFalse() {

    Table mockTable = mock(Table.class);

    when(mockTable.indexOf(anyString())).thenReturn(-1);
    when(mockTable.removeColumn(anyInt())).thenThrow(new IndexOutOfBoundsException("test"));
    when(mockTable.removeColumn(anyString())).thenCallRealMethod();

    assertThat(mockTable.removeColumn("TestColumn")).isFalse();

    verify(mockTable, times(1)).indexOf(eq("TestColumn"));
    verify(mockTable, never()).removeColumn(eq(-1));
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
}
