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
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Optional;

import org.junit.Test;

/**
 * Unit tests for {@link AbstractColumn}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.struct.tabular.AbstractColumn
 * @since 1.0.0
 */
public class AbstractColumnTests {

  @Test
  public void constructsNewColumnWithNameAndType() {

    AbstractColumn<Object> column = new TestColumn<>("TestName", Object.class);

    assertThat(column).isNotNull();
    assertThat(column.getName()).isEqualTo("TestName");
    assertThat(column.getType()).isEqualTo(Object.class);
    assertThat(column.getAlias().isPresent()).isFalse();
    assertThat(column.getDefaultValue().isPresent()).isFalse();
    assertThat(column.getDescription().isPresent()).isFalse();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void constructNewColumnCopiedFromExistingColumn() {

    Column<Object> mockColumn = mock(Column.class);

    when(mockColumn.getName()).thenReturn("MockName");
    when(mockColumn.getType()).thenReturn(Object.class);
    when(mockColumn.getAlias()).thenReturn(Optional.empty());
    when(mockColumn.getDefaultValue()).thenReturn(Optional.empty());
    when(mockColumn.getDescription()).thenReturn(Optional.empty());

    AbstractColumn<Object> column = new TestColumn<>(mockColumn);

    assertThat(column).isNotNull();
    assertThat(column.getName()).isEqualTo("MockName");
    assertThat(column.getType()).isEqualTo(Object.class);
    assertThat(column.getAlias().isPresent()).isFalse();
    assertThat(column.getDefaultValue().isPresent()).isFalse();
    assertThat(column.getDescription().isPresent()).isFalse();

    verify(mockColumn, times(1)).getName();
    verify(mockColumn, times(1)).getType();
    verify(mockColumn, times(1)).getAlias();
    verify(mockColumn, times(1)).getDefaultValue();
    verify(mockColumn, times(1)).getDescription();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void constructNewColumnCopiedFromExistingFullyInitializedColumn() {

    Column<Object> mockColumn = mock(Column.class);

    when(mockColumn.getName()).thenReturn("MockName");
    when(mockColumn.getType()).thenReturn(Object.class);
    when(mockColumn.getAlias()).thenReturn(Optional.of("MockAlias"));
    when(mockColumn.getDefaultValue()).thenReturn(Optional.of("test"));
    when(mockColumn.getDescription()).thenReturn(Optional.of("A Mock Column storing Objects"));

    AbstractColumn<Object> column = new TestColumn<>(mockColumn);

    assertThat(column).isNotNull();
    assertThat(column.getName()).isEqualTo("MockName");
    assertThat(column.getType()).isEqualTo(Object.class);
    assertThat(column.getAlias().orElse(null)).isEqualTo("MockAlias");
    assertThat(column.getDefaultValue().orElse(null)).isEqualTo("test");
    assertThat(column.getDescription().orElse(null)).isEqualTo("A Mock Column storing Objects");

    verify(mockColumn, times(1)).getName();
    verify(mockColumn, times(1)).getType();
    verify(mockColumn, times(1)).getAlias();
    verify(mockColumn, times(1)).getDefaultValue();
    verify(mockColumn, times(1)).getDescription();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void setAndGetAlias() {

    AbstractColumn<Object> column = new TestColumn<>("TestColumn", Object.class);

    assertThat(column.getAlias().isPresent()).isFalse();

    column.setAlias("TestAlias");

    assertThat(column.getAlias().orElse(null)).isEqualTo("TestAlias");

    column.setAlias(null);

    assertThat(column.getAlias().isPresent()).isFalse();
    assertThat(column.<AbstractColumn>withAlias("X")).isEqualTo(column);
    assertThat(column.getAlias().orElse(null)).isEqualTo("X");
  }

  @Test
  @SuppressWarnings("unchecked")
  public void setAndGetDefaultValue() {

    AbstractColumn<Object> column = new TestColumn<>("TestColumn", Object.class);

    assertThat(column.getDefaultValue().isPresent()).isFalse();

    column.setDefaultValue("test");

    assertThat(column.getDefaultValue().orElse(null)).isEqualTo("test");

    column.setDefaultValue(null);

    assertThat(column.getDefaultValue().isPresent()).isFalse();
    assertThat(column.<AbstractColumn>usingDefaultValue(2)).isEqualTo(column);
    assertThat(column.getDefaultValue().orElse(null)).isEqualTo(2);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void setAndGetDescription() {

    AbstractColumn<Object> column = new TestColumn<>("TestColumn", Object.class);

    assertThat(column.getDescription().isPresent()).isFalse();

    column.setDescription("A test description");

    assertThat(column.getDescription().orElse(null)).isEqualTo("A test description");

    column.setDescription(null);

    assertThat(column.getDescription().isPresent()).isFalse();
    assertThat(column.<AbstractColumn>describedAs("Must not be null")).isEqualTo(column);
    assertThat(column.getDescription().orElse(null)).isEqualTo("Must not be null");
  }

  @Test
  public void equalsDifferentColumnReturnsFalse() {

    AbstractColumn<?> columnOne = new TestColumn<>("TestColumn", Object.class);
    AbstractColumn<?> columnTwo = new TestColumn<>("MockColumn", Object.class);

    assertThat(columnOne.equals(columnTwo)).isFalse();
  }

  @Test
  public void equalsEqualColumnReturnsTrue() {

    AbstractColumn<?> columnOne = new TestColumn<>("TestColumn", Object.class);
    AbstractColumn<?> columnTwo = new TestColumn<>("TestColumn", Object.class);

    assertThat(columnOne.equals(columnTwo)).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void equalsIdenticalColumnReturnsTrue() {

    AbstractColumn<?> column = new TestColumn<Object>("TestColumn", Object.class);

    assertThat(column.equals(column)).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void equalsNullReturnsFalse() {

    AbstractColumn<?> column = new TestColumn<Object>("TestColumn", Object.class);

    assertThat(column.equals(null)).isFalse();
  }

  @Test
  public void computesHashCode() {

    AbstractColumn<?> testColumn = new TestColumn<>("TestColumn", Object.class);

    int hashValue = testColumn.hashCode();

    assertThat(hashValue).isNotZero();
    assertThat(hashValue).isEqualTo(testColumn.hashCode());

    AbstractColumn<?> mockColumn = new TestColumn<>("MockColumn", Object.class);

    assertThat(hashValue).isNotEqualTo(mockColumn.hashCode());
  }

  @Test
  public void toStringIsCorrect() {

    AbstractColumn<String> column = new TestColumn<String>("TestColumn", String.class)
      .describedAs("A test column")
      .usingDefaultValue("TEST")
      .withAlias("X");

    assertThat(column.toString())
      .isEqualTo(String.format(AbstractColumn.COLUMN_TO_STRING,
        column.getClass().getName(), column.getName(), column.getAlias().orElse(null),
          column.getDescription().orElse(null), column.getType(), column.getDefaultValue().orElse(null)));

  }

  static final class TestColumn<T> extends AbstractColumn<T> {

    public TestColumn(String name, Class<T> type) {
      super(name, type);
    }

    public TestColumn(Column<T> column) {
      super(column);
    }
  }
}
