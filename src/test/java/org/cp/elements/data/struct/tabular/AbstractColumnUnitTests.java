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
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.time.Instant;
import java.time.Month;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.Arrays;
import java.util.Optional;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link AbstractColumn}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.struct.tabular.AbstractColumn
 * @since 1.0.0
 */
class AbstractColumnUnitTests {

  private TestColumn<Object> newColumn(String name) {
    return new TestColumn<>(name, Object.class);
  }

  private <T> TestColumn<T> newColumn(String name, Class<T> type) {
    return new TestColumn<>(name, type);
  }

  @Test
  void constructsNewColumnWithNameAndType() {

    AbstractColumn<Object> column = new TestColumn<>("TestColumn", Object.class);

    assertThat(column).isNotNull();
    assertThat(column.getName()).isEqualTo("TestColumn");
    assertThat(column.getType()).isEqualTo(Object.class);
    assertThat(column.getAlias()).isNotPresent();
    assertThat(column.getDefaultValue()).isNotPresent();
    assertThat(column.getDescription()).isNotPresent();
    assertThat(column.getView()).isNotPresent();
    assertThat(column.getResolvedView()).isNull();
  }

  @Test
  void constructsNewColumnWithIllegalNameThrowsException() {

    Arrays.asList("  ", "", null).forEach(invalidColumnName ->
      assertThatIllegalArgumentException()
        .isThrownBy(() -> new TestColumn<>(invalidColumnName, Object.class))
        .withMessage("Name [%s] is required", invalidColumnName)
        .withNoCause());
  }

  @Test
  void constructsNewColumnWithIllegalTypeThrowsException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new TestColumn<>("TestColumn", null))
      .withMessage("Type is required")
      .withNoCause();
  }

  @Test
  @SuppressWarnings("unchecked")
  void constructNewColumnCopiedFromExistingColumn() {

    Column<Object> mockColumn = mock(Column.class);

    doReturn("MockColumn").when(mockColumn).getName();
    doReturn(Instant.class).when(mockColumn).getType();
    doReturn(Optional.empty()).when(mockColumn).getAlias();
    doReturn(Optional.empty()).when(mockColumn).getDefaultValue();
    doReturn(Optional.empty()).when(mockColumn).getDescription();

    AbstractColumn<Object> column = new TestColumn<>(mockColumn);

    assertThat(column).isNotNull();
    assertThat(column.getName()).isEqualTo("MockColumn");
    assertThat(column.getType()).isEqualTo(Instant.class);
    assertThat(column.getAlias()).isNotPresent();
    assertThat(column.getDefaultValue()).isNotPresent();
    assertThat(column.getDescription()).isNotPresent();
    assertThat(column.getView()).isNotPresent();

    verify(mockColumn, times(1)).getName();
    verify(mockColumn, times(1)).getType();
    verify(mockColumn, times(1)).getAlias();
    verify(mockColumn, times(1)).getDefaultValue();
    verify(mockColumn, times(1)).getDescription();
    verifyNoMoreInteractions(mockColumn);
  }

  @Test
  @SuppressWarnings("unchecked")
  void constructNewColumnCopiedFromExistingFullyInitializedColumn() {

    ZonedDateTime dateTime = ZonedDateTime.of(2023, Month.MARCH.getValue(), 6,
      22, 40, 30, 0, ZoneOffset.systemDefault());

    View mockView = mock(View.class);

    Column<Object> mockColumn = mock(Column.class);

    doReturn("MockColumn").when(mockColumn).getName();
    doReturn(Instant.class).when(mockColumn).getType();
    doReturn(Optional.of("MockAlias")).when(mockColumn).getAlias();
    doReturn(Optional.of(dateTime.toInstant())).when(mockColumn).getDefaultValue();
    doReturn(Optional.of("Mock description.")).when(mockColumn).getDescription();
    doReturn(Optional.of(mockView)).when(mockColumn).getView();

    AbstractColumn<Object> column = new TestColumn<>(mockColumn);

    assertThat(column).isNotNull();
    assertThat(column.getName()).isEqualTo("MockColumn");
    assertThat(column.getType()).isEqualTo(Instant.class);
    assertThat(column.getAlias().orElse(null)).isEqualTo("MockAlias");
    assertThat(column.getDefaultValue().orElse(null)).isEqualTo(dateTime.toInstant());
    assertThat(column.getDescription().orElse(null)).isEqualTo("Mock description.");

    verify(mockColumn, times(1)).getName();
    verify(mockColumn, times(1)).getType();
    verify(mockColumn, times(1)).getAlias();
    verify(mockColumn, times(1)).getDefaultValue();
    verify(mockColumn, times(1)).getDescription();
    verify(mockColumn, never()).getView();
    verifyNoMoreInteractions(mockColumn);
  }

  @Test
  void constructNewColumnFromNullColumnThrowsException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new TestColumn<>((Column<?>) null))
      .withMessage("The Column to copy is required")
      .withNoCause();
  }

  @Test
  void constructAndBuildNewColumn() {

    Instant now = Instant.now();

    View mockView = mock(View.class);

    AbstractColumn<Instant> column = new TestColumn<>("TestColumn", Instant.class)
      .aliasedWith("TestAlias")
      .describedAs("Test description")
      .in(mockView)
      .valueDefaultingTo(now);

    assertThat(column).isNotNull();
    assertThat(column.getName()).isEqualTo("TestColumn");
    assertThat(column.getType()).isEqualTo(Instant.class);
    assertThat(column.getView().orElse(null)).isEqualTo(mockView);
    assertThat(column.getAlias().orElse(null)).isEqualTo("TestAlias");
    assertThat(column.getDescription().orElse(null)).isEqualTo("Test description");
    assertThat(column.getDefaultValue().orElse(null)).isEqualTo(now);
  }

  @Test
  void setAndGetAlias() {

    AbstractColumn<Object> column = newColumn("TestColumn");

    assertThat(column.getAlias()).isNotPresent();

    column.setAlias("TestAlias");

    assertThat(column.getAlias().orElse(null)).isEqualTo("TestAlias");

    column.setAlias("MockAlias");

    assertThat(column.getAlias().orElse(null)).isEqualTo("MockAlias");

    column.setAlias(null);

    assertThat(column.getAlias()).isNotPresent();
  }

  @Test
  void setAndGetDefaultValue() {

    AbstractColumn<String> column = newColumn("TestColumn", String.class);

    assertThat(column.getDefaultValue()).isNotPresent();

    column.setDefaultValue("test");

    assertThat(column.getDefaultValue().orElse(null)).isEqualTo("test");

    column.setDefaultValue("mock");

    assertThat(column.getDefaultValue().orElse(null)).isEqualTo("mock");

    column.setDefaultValue(null);

    assertThat(column.getDefaultValue()).isNotPresent();
  }

  @Test
  void setAndGetDescription() {

    AbstractColumn<Object> column = newColumn("TestColumn");

    assertThat(column.getDescription()).isNotPresent();

    column.setDescription("Test description.");

    assertThat(column.getDescription().orElse(null)).isEqualTo("Test description.");

    column.setDescription("Mock description.");

    assertThat(column.getDescription().orElse(null)).isEqualTo("Mock description.");

    column.setDescription(null);

    assertThat(column.getDescription()).isNotPresent();
  }

  @Test
  void setAndGetView() {

    View mockViewOne = mock(View.class);
    View mockViewTwo = mock(View.class);

    AbstractColumn<Object> column = newColumn("TestColumn");

    assertThat(column.getView()).isNotPresent();
    assertThat(column.getResolvedView()).isNull();

    column.setView(mockViewOne);

    assertThat(column.getView().orElse(null)).isEqualTo(mockViewOne);
    assertThat(column.getResolvedView()).isEqualTo(mockViewOne);

    column.setView(mockViewTwo);

    assertThat(column.getView().orElse(null)).isEqualTo(mockViewTwo);
    assertThat(column.getResolvedView()).isEqualTo(mockViewTwo);

    column.setView(null);

    assertThat(column.getView()).isNotPresent();
    assertThat(column.getResolvedView()).isNull();
  }

  @Test
  void aliasedWithIsCorrect() {

    AbstractColumn<String> column = newColumn("TestColumn", String.class);

    assertThat(column.getAlias()).isNotPresent();
    assertThat(column.<AbstractColumn<String>>aliasedWith("TestAlias")).isSameAs(column);
    assertThat(column.getAlias().orElse(null)).isEqualTo("TestAlias");
    assertThat(column.<AbstractColumn<String>>aliasedWith("MockAlias")).isSameAs(column);
    assertThat(column.getAlias().orElse(null)).isEqualTo("MockAlias");
    assertThat(column.<AbstractColumn<String>>aliasedWith(null)).isSameAs(column);
    assertThat(column.getAlias()).isNotPresent();
  }

  @Test
  void describedAsIsCorrect() {

    AbstractColumn<String> column = newColumn("TestColumn", String.class);

    assertThat(column.getDescription()).isNotPresent();
    assertThat(column.<AbstractColumn<String>>describedAs("Test description")).isSameAs(column);
    assertThat(column.getDescription().orElse(null)).isEqualTo("Test description");
    assertThat(column.<AbstractColumn<String>>describedAs("Mock description")).isSameAs(column);
    assertThat(column.getDescription().orElse(null)).isEqualTo("Mock description");
    assertThat(column.<AbstractColumn<String>>describedAs(null)).isSameAs(column);
    assertThat(column.getDescription()).isNotPresent();
  }

  @Test
  void inIsCorrect() {

    View mockViewOne = mock(View.class);
    View mockViewTwo = mock(View.class);

    AbstractColumn<Object> column = newColumn("TestColumn");

    assertThat(column.getView()).isNotPresent();
    assertThat(column.<AbstractColumn<Object>>in(mockViewOne)).isSameAs(column);
    assertThat(column.getView().orElse(null)).isEqualTo(mockViewOne);
    assertThat(column.<AbstractColumn<Object>>in(mockViewTwo)).isSameAs(column);
    assertThat(column.getView().orElse(null)).isEqualTo(mockViewTwo);
    assertThat(column.<AbstractColumn<Object>>in(null)).isSameAs(column);
    assertThat(column.getView()).isNotPresent();
  }

  @Test
  void valueDefaultingToIsCorrect() {

    AbstractColumn<String> column = newColumn("TestColumn", String.class);

    assertThat(column.getDefaultValue()).isNotPresent();
    assertThat(column.<AbstractColumn<String>>valueDefaultingTo("test")).isSameAs(column);
    assertThat(column.getDefaultValue().orElse(null)).isEqualTo("test");
    assertThat(column.<AbstractColumn<String>>valueDefaultingTo("mock")).isSameAs(column);
    assertThat(column.getDefaultValue().orElse(null)).isEqualTo("mock");
    assertThat(column.<AbstractColumn<String>>valueDefaultingTo(null)).isSameAs(column);
    assertThat(column.getDefaultValue()).isNotPresent();
  }

  @Test
  void compareToIsCorrect() {

    AbstractColumn<Object> columnOne = newColumn("MockColumn");
    AbstractColumn<Object> columnTwo = newColumn("TestColumn");
    AbstractColumn<Object> columnThree = newColumn("TestColumn");

    assertThat(columnOne).isLessThan(columnTwo);
    assertThat(columnTwo).isGreaterThan(columnOne);
    assertThat(columnOne).isEqualByComparingTo(columnOne);
    assertThat(columnTwo).isEqualByComparingTo(columnThree);
  }

  @Test
  void equalsColumnWithSameNameInDifferentViewReturnsFalse() {

    View mockViewOne = mock(View.class);
    View mockViewTwo = mock(View.class);

    AbstractColumn<Object> columnZero = newColumn("TestColumn");
    AbstractColumn<Object> columnOne = newColumn("TestColumn").in(mockViewOne);
    AbstractColumn<Object> columnTwo = newColumn("TestColumn").in(mockViewTwo);

    assertThat(columnZero.getView()).isNotPresent();
    assertThat(columnOne).isNotEqualTo(columnZero);
    assertThat(columnOne).isNotEqualTo(columnTwo);

    verifyNoInteractions(mockViewOne, mockViewTwo);
  }

  @Test
  void equalsColumnWithDifferentNameInSameViewReturnsFalse() {

    View mockView = mock(View.class);

    AbstractColumn<Object> columnOne = newColumn("TestColumn").in(mockView);
    AbstractColumn<Object> columnTwo = newColumn("MockColumn").in(mockView);

    assertThat(columnOne).isNotEqualTo(columnTwo);

    verifyNoInteractions(mockView);
  }

  @Test
  void equalsEqualColumnReturnsTrue() {

    AbstractColumn<?> columnOne = newColumn("TestColumn", Integer.class);
    AbstractColumn<?> columnTwo = newColumn("TestColumn", String.class);

    assertThat(columnOne.equals(columnTwo)).isTrue();
  }

  @Test
  void equalsSameColumnReturnsTrue() {

    AbstractColumn<?> column = newColumn("TestColumn");

    assertThat(column).isEqualTo(column);
  }

  @Test
  void equalsNullIsNullSafeReturnsFalse() {
    assertThat(newColumn("TestColumn")).isNotEqualTo(null);
  }

  @Test
  void equalsObjectReturnsFalse() {
    assertThat(newColumn("TestColumn")).isNotEqualTo("TestColumn");
  }

  @Test
  void computesHashCodeCorrectly() {

    View mockView = mock(View.class);

    AbstractColumn<Object> column = newColumn("TestColumn");

    int hashValue = column.hashCode();

    assertThat(hashValue).isNotZero();
    assertThat(hashValue).isEqualTo(column.hashCode());
    assertThat(column).hasSameHashCodeAs(newColumn("TestColumn"));
    assertThat(column).doesNotHaveSameHashCodeAs(newColumn("MockColumn"));
    assertThat(column).doesNotHaveSameHashCodeAs(newColumn("TestColumn").in(mockView));
  }

  @Test
  void toStringIsCorrect() {

    View mockView = mock(View.class);

    doReturn("MockView").when(mockView).getName();

    AbstractColumn<String> column = new TestColumn<>("TestColumn", String.class)
      .aliasedWith("TestAlias")
      .describedAs("Test description")
      .in(mockView)
      .valueDefaultingTo("test");

    assertThat(column.toString()).isEqualTo(String.format(AbstractColumn.COLUMN_TO_STRING, column.getClass().getName(),
      "TestColumn", "TestAlias", "Test description", String.class.getName(), "test", "MockView"));

    verify(mockView, times(1)).getName();
    verifyNoMoreInteractions(mockView);
  }

  @Test
  void resolveViewFromAbstractColumn() {

    View mockView = mock(View.class);
    TestColumn<Object> column = spy(new TestColumn<>("mock", Object.class));

    doReturn(mockView).when(column).getResolvedView();

    assertThat(column.resolveView(column)).isEqualTo(mockView);

    verify(column, times(1)).resolveView(eq(column));
    verify(column, times(1)).getResolvedView();
    verifyNoMoreInteractions(column);
    verifyNoInteractions(mockView);
  }

  @Test
  @SuppressWarnings("unchecked")
  void resolveViewFromColumn() {

    AbstractColumn<?> column = new TestColumn<>("test", Object.class);
    Column<Object> mockColumn = mock(Column.class);
    View mockView = mock(View.class);

    doReturn(Optional.of(mockView)).when(mockColumn).getView();

    assertThat(column.resolveView(mockColumn)).isEqualTo(mockView);

    verify(mockColumn, times(1)).getView();
    verifyNoMoreInteractions(mockColumn);
    verifyNoInteractions(mockView);
  }

  @Test
  void resolveViewFromColumnWithNoView() {

    AbstractColumn<?> column = new TestColumn<>("test", Object.class);
    Column<Object> mockColumn = mock(Column.class);
    View mockView = mock(View.class);

    doReturn(Optional.empty()).when(mockColumn).getView();

    assertThat(column.resolveView(mockColumn)).isNull();

    verify(mockColumn, times(1)).getView();
    verifyNoMoreInteractions(mockColumn);
    verifyNoInteractions(mockView);
  }

  @Test
  void resolveViewIsNullSafe() {
    assertThat(new TestColumn<>("test", Object.class).resolveView(null)).isNull();
  }

  static class TestColumn<T> extends AbstractColumn<T> {

    public TestColumn(String name, Class<T> type) {
      super(name, type);
    }

    public TestColumn(Column<T> column) {
      super(column);
    }
  }
}
