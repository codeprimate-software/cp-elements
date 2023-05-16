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
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Optional;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link Column}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.struct.tabular.Column
 * @since 1.0.0
 */
public class ColumnUnitTests {

  @Test
  public void columnAliasIsEmptyByDefault() {

    Column<?> column = mock(Column.class);

    doCallRealMethod().when(column).getAlias();

    Optional<String> alias = column.getAlias();

    assertThat(alias).isNotNull();
    assertThat(alias).isNotPresent();

    verify(column, times(1)).getAlias();
    verifyNoMoreInteractions(column);
  }

  @Test
  public void columnDefaultValueIsEmptyByDefault() {

    Column<?> column = mock(Column.class);

    doCallRealMethod().when(column).getDefaultValue();

    Optional<?> defaultValue = column.getDefaultValue();

    assertThat(defaultValue).isNotNull();
    assertThat(defaultValue).isNotPresent();

    verify(column, times(1)).getDefaultValue();
    verifyNoMoreInteractions(column);
  }

  @Test
  public void columnDescriptionIsEmptyByDefault() {

    Column<?> column = mock(Column.class);

    doCallRealMethod().when(column).getDescription();

    Optional<String> description = column.getDescription();

    assertThat(description).isNotNull();
    assertThat(description).isNotPresent();

    verify(column, times(1)).getDescription();
    verifyNoMoreInteractions(column);
  }

  @Test
  public void indexOfColumnInView() {

    View mockView = mock(View.class);

    Column<?> mockColumn = mock(Column.class);

    doCallRealMethod().when(mockColumn).index();
    doReturn(Optional.ofNullable(mockView)).when(mockColumn).getView();
    doReturn(8).when(mockView).indexOf(eq(mockColumn));

    assertThat(mockColumn.index()).isEqualTo(8);

    verify(mockColumn, times(1)).index();
    verify(mockColumn, times(1)).getView();
    verify(mockView, times(1)).indexOf(eq(mockColumn));
    verifyNoMoreInteractions(mockColumn, mockView);
  }

  @Test
  public void indexOfColumnNotInView() {

    Column<?> mockColumn = mock(Column.class);

    doCallRealMethod().when(mockColumn).index();
    doReturn(Optional.empty()).when(mockColumn).getView();

    assertThat(mockColumn.index()).isEqualTo(-1);

    verify(mockColumn, times(1)).index();
    verify(mockColumn, times(1)).getView();
    verifyNoMoreInteractions(mockColumn);
  }
}
