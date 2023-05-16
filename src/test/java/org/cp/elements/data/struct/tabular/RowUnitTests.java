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
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.assertj.core.api.Assertions.assertThatIllegalStateException;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newIndexOutOfBoundsException;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Arrays;
import java.util.Optional;

import org.junit.jupiter.api.Test;

import org.mockito.ArgumentMatchers;

/**
 * Unit Tests for {@link Row}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.struct.tabular.Row
 * @since 1.0.0
 */
public class RowUnitTests {

  @Test
  public void getValueAtColumnIndex() {

    Row mockRow = mock(Row.class);

    Object[] values = { true, 'x', 1, Math.PI, "test" };

    doReturn(values).when(mockRow).values();
    doCallRealMethod().when(mockRow).getValue(anyInt());

    assertThat(mockRow.<Boolean>getValue(0)).isTrue();
    assertThat(mockRow.<Character>getValue(1)).isEqualTo('x');
    assertThat(mockRow.<Integer>getValue(2)).isEqualTo(1);
    assertThat(mockRow.<Double>getValue(3)).isEqualTo(Math.PI);
    assertThat(mockRow.<String>getValue(4)).isEqualTo("test");

    verify(mockRow, times(5)).getValue(anyInt());
    verify(mockRow, times(5)).index();
    verify(mockRow, times(5)).values();
    verifyNoMoreInteractions(mockRow);
  }

  @Test
  public void getValueAtInvalidColumnIndex() {

    Row mockRow = mock(Row.class);

    doReturn(2).when(mockRow).index();
    doCallRealMethod().when(mockRow).getValue(anyInt());

    assertThatExceptionOfType(IndexOutOfBoundsException.class)
      .isThrownBy(() -> mockRow.getValue(-1))
      .withMessage("Column index [-1] for Row [2] must be greater than equal to 0")
      .withNoCause();

    verify(mockRow, times(1)).getValue(eq(-1));
    verify(mockRow, times(1)).index();
    verifyNoMoreInteractions(mockRow);
  }

  @Test
  public void getValueWithColumnName() {

    Row mockRow = mock(Row.class);

    View mockView = mock(View.class);

    doReturn(Optional.of(mockView)).when(mockRow).getView();
    doCallRealMethod().when(mockRow).getValue(anyString());
    doReturn(1).when(mockView).indexOf(anyString());
    doReturn("test").when(mockRow).getValue(anyInt());

    assertThat(mockRow.<String>getValue("TestColumn")).isEqualTo("test");

    verify(mockRow, times(1)).getValue(eq("TestColumn"));
    verify(mockRow, times(1)).getView();
    verify(mockRow, times(1)).getValue(eq(1));
    verify(mockView, times(1)).indexOf(eq("TestColumn"));
    verifyNoMoreInteractions(mockRow, mockView);
  }

  @Test
  public void getValueWithColumnNameHavingNoView() {

    Row mockRow = mock(Row.class);

    doReturn(Optional.empty()).when(mockRow).getView();
    doCallRealMethod().when(mockRow).getValue(anyString());
    doReturn(-1).when(mockRow).index();

    assertThatIllegalStateException()
      .isThrownBy(() -> mockRow.getValue("MockColumn"))
      .withMessage("This Row [-1] is not associated with a View")
      .withNoCause();

    verify(mockRow, times(1)).getValue(eq("MockColumn"));
    verify(mockRow, times(1)).getView();
    verify(mockRow, times(1)).index();
    verifyNoMoreInteractions(mockRow);
  }

  @Test
  public void getValueWithInvalidNonExistingColumnName() {

    Row mockRow = mock(Row.class);

    View mockView = mock(View.class);

    doCallRealMethod().when(mockRow).getValue(anyString());
    doReturn(Optional.of(mockView)).when(mockRow).getView();
    doReturn(-1).when(mockView).indexOf(anyString());
    doThrow(newIndexOutOfBoundsException("test")).when(mockRow).getValue(anyInt());

    assertThatExceptionOfType(IndexOutOfBoundsException.class)
      .isThrownBy(() -> mockRow.getValue("InvalidNonExistingColumn"))
      .withMessage("test")
      .withNoCause();

    verify(mockRow, times(1)).getValue(eq("InvalidNonExistingColumn"));
    verify(mockRow, times(1)).getView();
    verify(mockRow, times(1)).getValue(eq(-1));
    verify(mockView, times(1)).indexOf(eq("InvalidNonExistingColumn"));
    verifyNoMoreInteractions(mockRow, mockView);
  }

  @Test
  public void getValueWithColumn() {

    Column<?> mockColumn = mock(Column.class);

    Row mockRow = mock(Row.class);

    doCallRealMethod().when(mockRow).getValue(any(Column.class));
    doReturn("TestColumn").when(mockColumn).getName();
    doReturn("test").when(mockRow).getValue(anyString());

    assertThat(mockRow.<String>getValue(mockColumn)).isEqualTo("test");

    verify(mockRow, times(1)).getValue(eq(mockColumn));
    verify(mockColumn, times(1)).getName();
    verify(mockRow, times(1)).getValue(eq("TestColumn"));
    verifyNoMoreInteractions(mockColumn, mockRow);
  }

  @Test
  public void getValueWithColumnHavingNoView() {

    Column<?> mockColumn = mock(Column.class);

    Row mockRow = mock(Row.class);

    doCallRealMethod().when(mockRow).getValue(eq(mockColumn));
    doReturn(2).when(mockRow).index();
    doReturn(Optional.empty()).when(mockRow).getView();

    assertThatIllegalArgumentException()
      .isThrownBy(() -> mockRow.getValue(mockColumn))
      .withMessage("[%s] is not a Column in this Row [2]", mockColumn)
      .withNoCause();

    verify(mockRow, times(1)).getValue(eq(mockColumn));
    verify(mockColumn, times(1)).getName();
    verify(mockRow, times(1)).index();
    verifyNoMoreInteractions(mockRow, mockColumn);
  }

  @Test
  public void getValueWithNullColumn() {

    Row mockRow = mock(Row.class);

    doReturn(2).when(mockRow).index();
    doCallRealMethod().when(mockRow).getValue(ArgumentMatchers.<Column<?>>any());

    assertThatIllegalArgumentException()
      .isThrownBy(() -> mockRow.getValue((Column<?>) null))
      .withMessage("[null] is not a Column in this Row [2]")
      .withNoCause();

    verify(mockRow, times(1)).getValue(isNull(Column.class));
    verify(mockRow, times(1)).index();
    verifyNoMoreInteractions(mockRow);
  }

  @Test
  public void getViewIsEmpty() {

    Row mockRow = mock(Row.class);

    doCallRealMethod().when(mockRow).getView();

    Optional<View> view = mockRow.getView();

    assertThat(view).isNotNull();
    assertThat(view).isNotPresent();

    verify(mockRow).getView();
    verifyNoMoreInteractions(mockRow);
  }

  @Test
  public void setValueWithColumnName() {

    Row mockRow = mock(Row.class);

    View mockView = mock(View.class);

    doCallRealMethod().when(mockRow).setValue(anyString(), any());
    doReturn(Optional.of(mockView)).when(mockRow).getView();
    doReturn(4).when(mockView).indexOf(anyString());
    doReturn("CurrentValue").when(mockRow).setValue(eq(4), any());

    assertThat(mockRow.setValue("TestColumn", "NewValue")).isEqualTo("CurrentValue");

    verify(mockRow, times(1)).setValue(eq("TestColumn"), eq("NewValue"));
    verify(mockRow, times(1)).getView();
    verify(mockView, times(1)).indexOf(eq("TestColumn"));
    verify(mockRow, times(1)).setValue(eq(4), eq("NewValue"));
    verifyNoMoreInteractions(mockRow, mockView);
  }

  @Test
  public void setValueWithColumnNameWhenViewIsNull() {

    Row mockRow = mock(Row.class);

    doCallRealMethod().when(mockRow).setValue(anyString(), any());
    doReturn(Optional.empty()).when(mockRow).getView();
    doReturn(5).when(mockRow).index();

    assertThatIllegalStateException()
      .isThrownBy(() -> mockRow.setValue("MockColumn", "test"))
      .withMessage("Row [5] is not associated with a View")
      .withNoCause();

    verify(mockRow, times(1)).setValue(eq("MockColumn"), eq("test"));
    verify(mockRow, times(1)).getView();
    verify(mockRow, times(1)).index();
    verifyNoMoreInteractions(mockRow);
  }

  @Test
  public void setValueWithColumnNameForInvalidNonExistingColumn() {

    Row mockRow = mock(Row.class);

    View mockView = mock(View.class);

    doCallRealMethod().when(mockRow).setValue(anyString(), any());
    doReturn(Optional.of(mockView)).when(mockRow).getView();
    doReturn(-1).when(mockView).indexOf(anyString());
    doReturn(8).when(mockRow).index();

    assertThatIllegalStateException()
      .isThrownBy(() -> mockRow.setValue("InvalidNonExistingColumn", "test"))
      .withMessage("Row [8] is not associated with a View")
      .withNoCause();

    verify(mockRow, times(1)).setValue(eq("InvalidNonExistingColumn"), eq("test"));
    verify(mockRow, times(1)).getView();
    verify(mockView, times(1)).indexOf(eq("InvalidNonExistingColumn"));
    verify(mockRow, times(1)).index();
    verifyNoMoreInteractions(mockRow, mockView);
  }

  @Test
  public void setValueWithColumn() {

    Column<?> mockColumn = mock(Column.class);

    Row mockRow = mock(Row.class);

    doCallRealMethod().when(mockRow).setValue(any(Column.class), any());
    doReturn("TestColumn").when(mockColumn).getName();
    doReturn("OldValue").when(mockRow).setValue(eq("TestColumn"), any());

    assertThat(mockRow.setValue(mockColumn, "NewValue")).isEqualTo("OldValue");

    verify(mockRow, times(1)).setValue(eq(mockColumn), eq("NewValue"));
    verify(mockColumn, times(1)).getName();
    verify(mockRow, times(1)).setValue(eq("TestColumn"), eq("NewValue"));
    verifyNoMoreInteractions(mockColumn, mockRow);
  }

  @Test
  public void setValueWithNullColumn() {

    Row mockRow = mock(Row.class);

    doCallRealMethod().when(mockRow).setValue(ArgumentMatchers.<Column<?>>any(), any());

    assertThatIllegalArgumentException()
      .isThrownBy(() -> mockRow.setValue((Column<?>) null, "test"))
      .withMessage("[null] is not a Column in this Row")
      .withNoCause();

    verify(mockRow, times(1)).setValue(isNull(Column.class), eq("test"));
    verifyNoMoreInteractions(mockRow);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void setValueWithInvalidNoNamedColumn() {

    Column<?> mockColumn = mock(Column.class);

    Row mockRow = mock(Row.class);

    doCallRealMethod().when(mockRow).setValue(ArgumentMatchers.<Column<?>>any(), any());

    Arrays.asList("  ", "", null).forEach(columnName -> {

      doReturn(columnName).when(mockColumn).getName();

      assertThatIllegalArgumentException()
        .isThrownBy(() -> mockRow.setValue(mockColumn, "test"))
        .withMessage("[%s] is not a Column in this Row", mockColumn)
        .withNoCause();

      verify(mockColumn, times(1)).getName();
      reset(mockColumn);
    });

    verify(mockRow, times(3)).setValue(eq(mockColumn), eq("test"));
    verifyNoMoreInteractions(mockColumn, mockRow);
  }

  @Test
  public void indexReturnsIndex() {

    Row mockRow = mock(Row.class);

    View mockView = mock(View.class);

    doCallRealMethod().when(mockRow).index();
    doReturn(Optional.of(mockView)).when(mockRow).getView();
    doReturn(2).when(mockView).indexOf(any(Row.class));

    assertThat(mockRow.index()).isEqualTo(2);

    verify(mockRow, times(1)).index();
    verify(mockRow, times(1)).getView();
    verify(mockView, times(1)).indexOf(eq(mockRow));
    verifyNoMoreInteractions(mockRow, mockView);
  }

  @Test
  public void indexWithNullViewReturnsMinusOne() {

    Row mockRow = mock(Row.class);

    doCallRealMethod().when(mockRow).index();
    doReturn(Optional.empty()).when(mockRow).getView();

    assertThat(mockRow.index()).isEqualTo(-1);

    verify(mockRow, times(1)).index();
    verify(mockRow, times(1)).getView();
    verifyNoMoreInteractions(mockRow);
  }
}
