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

import java.util.Optional;

import org.junit.Test;
import org.mockito.ArgumentMatchers;

/**
 * Unit tests for {@link Row}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.struct.tabular.Row
 * @since 1.0.0
 */
public class RowTests {

  @Test
  public void getValueWithColumnName() {

    Row mockRow = mock(Row.class);

    View mockView = mock(View.class);

    when(mockRow.getValue(anyInt())).thenReturn("test");
    when(mockRow.getValue(anyString())).thenCallRealMethod();
    when(mockRow.getView()).thenReturn(Optional.of(mockView));
    when(mockView.indexOf(anyString())).thenReturn(1);

    assertThat(mockRow.<String>getValue("TestColumn")).isEqualTo("test");

    verify(mockView, times(1)).indexOf(eq("TestColumn"));
    verify(mockRow, times(1)).getValue(eq(1));
  }

  @Test(expected = IllegalStateException.class)
  public void getValueWithColumnNameWhenViewIsNullThrowsIllegalStateException() {

    Row mockRow = mock(Row.class);

    when(mockRow.getView()).thenReturn(Optional.empty());
    when(mockRow.getValue(anyString())).thenCallRealMethod();

    try {
      mockRow.getValue("TestColumn");
    }
    catch (IllegalStateException expected) {

      assertThat(expected).hasMessage("This Row is not associated with a View");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockRow, times(1)).getView();
      verify(mockRow, never()).getValue(anyInt());
    }
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void getValueWithInvalidColumnNameThrowsIndexOutOfBoundsException() {

    Row mockRow = mock(Row.class);

    View mockView = mock(View.class);

    when(mockRow.getValue(anyString())).thenCallRealMethod();
    when(mockRow.getValue(anyInt())).thenThrow(new IndexOutOfBoundsException("test"));
    when(mockRow.getView()).thenReturn(Optional.of(mockView));
    when(mockView.indexOf(anyString())).thenReturn(-1);

    try {
      mockRow.getValue("TestColumn");
    }
    catch (IndexOutOfBoundsException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockRow, times(1)).getView();
      verify(mockRow, times(1)).getValue(eq(-1));
      verify(mockView, times(1)).indexOf(eq("TestColumn"));
    }
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void getValueWithNullColumnNameThrowsIndexOutOfBoundsException() {

    Row mockRow = mock(Row.class);

    View mockView = mock(View.class);

    when(mockRow.getValue(anyInt())).thenThrow(new IndexOutOfBoundsException("test"));
    when(mockRow.getValue(ArgumentMatchers.<String>any())).thenCallRealMethod();
    when(mockRow.getView()).thenReturn(Optional.of(mockView));
    when(mockView.indexOf(ArgumentMatchers.<String>any())).thenReturn(-1);

    try {
      mockRow.getValue((String) null);
    }
    catch (IndexOutOfBoundsException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockRow, times(1)).getView();
      verify(mockRow, times(1)).getValue(eq(-1));
      verify(mockView, times(1)).indexOf(ArgumentMatchers.<String>isNull());
    }
  }

  @Test
  public void getValueWithColumn() {

    Column mockColumn = mock(Column.class);

    Row mockRow = mock(Row.class);

    when(mockColumn.getName()).thenReturn("TestColumn");
    when(mockRow.getValue(anyString())).thenReturn("test");
    when(mockRow.getValue(any(Column.class))).thenCallRealMethod();

    assertThat(mockRow.<String>getValue(mockColumn)).isEqualTo("test");

    verify(mockColumn, times(1)).getName();
    verify(mockRow, times(1)).getValue(eq("TestColumn"));
  }

  @Test(expected = IllegalStateException.class)
  public void getValueWithColumnWhenViewIsNullThrowsIllegalArgumentException() {

    Column mockColumn = mock(Column.class);

    Row mockRow = mock(Row.class);

    when(mockColumn.getName()).thenReturn("TestColumn");
    when(mockRow.getValue(anyString())).thenCallRealMethod();
    when(mockRow.getValue(ArgumentMatchers.<Column>any())).thenCallRealMethod();
    when(mockRow.getView()).thenReturn(Optional.empty());

    try {
      mockRow.getValue(mockColumn);
    }
    catch (IllegalStateException expected) {

      assertThat(expected).hasMessage("This Row is not associated with a View");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockColumn, times(1)).getName();
      verify(mockRow, times(1)).getView();
      verify(mockRow, times(1)).getValue(eq("TestColumn"));
      verify(mockRow, never()).getValue(anyInt());
    }
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void getValueWithInvalidColumnThrowsIndexOutOfBoundsException() {

    Column mockColumn = mock(Column.class);

    Row mockRow = mock(Row.class);

    View mockView = mock(View.class);

    when(mockColumn.getName()).thenReturn("TestColumn");
    when(mockRow.getValue(anyInt())).thenThrow(new IndexOutOfBoundsException("test"));
    when(mockRow.getValue(anyString())).thenCallRealMethod();
    when(mockRow.getValue(ArgumentMatchers.<Column>any())).thenCallRealMethod();
    when(mockRow.getView()).thenReturn(Optional.of(mockView));
    when(mockView.indexOf(anyString())).thenReturn(-1);

    try {
      mockRow.getValue(mockColumn);
    }
    catch (IndexOutOfBoundsException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockColumn, times(1)).getName();
      verify(mockRow, times(1)).getValue(eq(-1));
      verify(mockRow, times(1)).getValue(eq("TestColumn"));
      verify(mockRow, times(1)).getView();
      verify(mockView, times(1)).indexOf(eq("TestColumn"));
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void getValueWithNullColumnThrowsIllegalArgumentException() {

    Row mockRow = mock(Row.class);

    when(mockRow.getValue(ArgumentMatchers.<Column>any())).thenCallRealMethod();

    try {
      mockRow.getValue((Column) null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("[null] is not a valid Column in this Row");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockRow, never()).getValue(anyString());
    }
  }

  @Test
  public void setValueWithColumnName() {

    Row mockRow = mock(Row.class);

    View mockView = mock(View.class);

    when(mockRow.setValue(anyInt(), any())).thenReturn("currentValue");
    when(mockRow.setValue(anyString(), any())).thenCallRealMethod();
    when(mockRow.getView()).thenReturn(Optional.of(mockView));
    when(mockView.indexOf(anyString())).thenReturn(1);

    assertThat(mockRow.setValue("TestColumn", "newValue")).isEqualTo("currentValue");

    verify(mockRow, times(1)).getView();
    verify(mockRow, times(1)).setValue(eq(1), eq("newValue"));
    verify(mockView, times(1)).indexOf(eq("TestColumn"));
  }

  @Test(expected = IllegalStateException.class)
  public void setValueWithColumnNameWhenViewIsNullThrowsIllegalStateException() {

    Row mockRow = mock(Row.class);

    when(mockRow.getView()).thenReturn(Optional.empty());
    when(mockRow.setValue(anyString(), any())).thenCallRealMethod();

    try {
      mockRow.setValue("TestColumn", "test");
    }
    catch (IllegalStateException expected) {

      assertThat(expected).hasMessage("This Row is not associated with a View");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockRow, times(1)).getView();
      verify(mockRow, never()).setValue(anyInt(), any());
    }
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void setValueWithInvalidColumnNameThrowsIndexOutOfBoundsException() {

    Row mockRow = mock(Row.class);

    View mockView = mock(View.class);

    when(mockRow.setValue(anyString(), any())).thenCallRealMethod();
    when(mockRow.setValue(anyInt(), any())).thenThrow(new IndexOutOfBoundsException("test"));
    when(mockRow.getView()).thenReturn(Optional.of(mockView));
    when(mockView.indexOf(anyString())).thenReturn(-1);

    try {
      mockRow.setValue("TestColumn", "test");
    }
    catch (IllegalStateException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockRow, times(1)).getView();
      verify(mockRow, times(1)).setValue(eq(-1), eq("test"));
      verify(mockView, times(1)).indexOf(eq("TestColumn"));
    }
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void setValueWithNullColumnNameThrowsIndexOutOfBoundsException() {

    Row mockRow = mock(Row.class);

    View mockView = mock(View.class);

    when(mockRow.setValue(ArgumentMatchers.<String>any(), any())).thenCallRealMethod();
    when(mockRow.setValue(anyInt(), any())).thenThrow(new IndexOutOfBoundsException("test"));
    when(mockRow.getView()).thenReturn(Optional.of(mockView));
    when(mockView.indexOf(ArgumentMatchers.<String>any())).thenReturn(-1);

    try {
      mockRow.setValue((String) null, "test");
    }
    catch (IllegalStateException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockRow, times(1)).getView();
      verify(mockRow, times(1)).setValue(eq(-1), eq("test"));
      verify(mockView, times(1)).indexOf(ArgumentMatchers.<String>isNull());
    }
  }

  @Test
  public void setValueWithColumn() {

    Column mockColumn = mock(Column.class);

    Row mockRow = mock(Row.class);

    View mockView = mock(View.class);

    when(mockColumn.getName()).thenReturn("TestColumn");
    when(mockRow.setValue(anyInt(), any())).thenReturn("currentValue");
    when(mockRow.setValue(anyString(), any())).thenCallRealMethod();
    when(mockRow.setValue(any(Column.class), any())).thenCallRealMethod();
    when(mockRow.getView()).thenReturn(Optional.of(mockView));
    when(mockView.indexOf(anyString())).thenReturn(1);

    assertThat(mockRow.setValue(mockColumn, "newValue")).isEqualTo("currentValue");

    verify(mockColumn, times(1)).getName();
    verify(mockRow, times(1)).getView();
    verify(mockRow, times(1)).setValue(eq(1), eq("newValue"));
    verify(mockRow, times(1)).setValue(eq("TestColumn"), eq("newValue"));
    verify(mockView, times(1)).indexOf(eq("TestColumn"));
  }

  @Test(expected = IllegalStateException.class)
  public void setValueWithColumnWhenViewIsNullThrowsIllegalStateException() {

    Column mockColumn = mock(Column.class);

    Row mockRow = mock(Row.class);

    when(mockColumn.getName()).thenReturn("TestColumn");
    when(mockRow.getView()).thenReturn(Optional.empty());
    when(mockRow.setValue(anyString(), any())).thenCallRealMethod();
    when(mockRow.setValue(any(Column.class), any())).thenCallRealMethod();

    try {
      mockRow.setValue(mockColumn, "test");
    }
    catch (IllegalStateException expected) {

      assertThat(expected).hasMessage("This Row is not associated with a View");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockColumn, times(1)).getName();
      verify(mockRow, times(1)).getView();
      verify(mockRow, times(1)).setValue(eq("TestColumn"), eq("test"));
    }
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void setValueWithInvalidColumnThrowsIndexOutOfBoundsException() {

    Column mockColumn = mock(Column.class);

    Row mockRow = mock(Row.class);

    View mockView = mock(View.class);

    when(mockColumn.getName()).thenReturn("TestColumn");
    when(mockRow.getView()).thenReturn(Optional.of(mockView));
    when(mockRow.setValue(anyInt(), any())).thenThrow(new IndexOutOfBoundsException("test"));
    when(mockRow.setValue(anyString(), any())).thenCallRealMethod();
    when(mockRow.setValue(any(Column.class), any())).thenCallRealMethod();
    when(mockView.indexOf(anyString())).thenReturn(-1);

    try {
      mockRow.setValue(mockColumn, "test");
    }
    catch (IndexOutOfBoundsException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockColumn, times(1)).getName();
      verify(mockRow, times(1)).getView();
      verify(mockRow, times(1)).setValue(eq("TestColumn"), eq("test"));
      verify(mockRow, times(1)).setValue(eq(-1), eq("test"));
      verify(mockView, times(1)).indexOf(eq("TestColumn"));
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void setValueWithNullColumnThrowsIllegalArgumentException() {

    Row mockRow = mock(Row.class);

    when(mockRow.setValue(ArgumentMatchers.<Column>any(), any())).thenCallRealMethod();

    try {
      mockRow.setValue((Column) null, "test");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("[null] is not a valid Column in this Row");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    verify(mockRow, never()).setValue(anyString(), any());
  }

  @Test
  public void indexReturnsIndex() {

    Row mockRow = mock(Row.class);

    View mockView = mock(View.class);

    when(mockRow.index()).thenCallRealMethod();
    when(mockRow.getView()).thenReturn(Optional.of(mockView));
    when(mockView.indexOf(any(Row.class))).thenReturn(2);

    assertThat(mockRow.index()).isEqualTo(2);

    verify(mockRow, times(1)).getView();
    verify(mockView, times(1)).indexOf(eq(mockRow));
  }

  @Test
  public void indexWithNullViewReturnsMinusOne() {

    Row mockRow = mock(Row.class);

    when(mockRow.index()).thenCallRealMethod();
    when(mockRow.getView()).thenReturn(Optional.empty());

    assertThat(mockRow.index()).isEqualTo(-1);

    verify(mockRow, times(1)).getView();
  }
}
