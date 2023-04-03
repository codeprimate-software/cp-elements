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
import static org.cp.elements.lang.RuntimeExceptionsFactory.newUnsupportedOperationException;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.lang.reflect.InvocationTargetException;
import java.time.LocalDateTime;
import java.util.Optional;

import org.junit.Test;

import org.cp.elements.data.mapping.MappingException;
import org.cp.elements.enums.Gender;
import org.cp.elements.lang.Constants;
import org.cp.elements.lang.reflect.MethodInvocationException;

import lombok.Data;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

/**
 * Unit Tests for {@link AbstractRow}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.struct.tabular.AbstractRow
 * @since 1.0.0
 */
public class AbstractRowUnitTests {

  @Test
  public void ofValuesReturnsRowOfValues() {

    Object[] values = { true, 'X', 1, Math.PI, "test" };

    AbstractRow row = AbstractRow.of(values);

    assertThat(row).isNotNull();
    assertThat(row.values()).isEqualTo(values);

    for (int index = 0; index < values.length; index++) {
      assertThat(row.<Object>getValue(index)).isEqualTo(values[index]);
    }

    assertThat(values[1]).isEqualTo('X');
    assertThat(row.<Object>getValue(1)).isEqualTo('X');
    assertThat(row.setValue(1, 'Y')).isEqualTo('X');
    assertThat(row.<Object>getValue(1)).isEqualTo('Y');
    assertThat(values[1]).isEqualTo('Y');
  }

  @Test
  public void ofEmptyValuesReturnsEmptyRow() {

    AbstractRow row = AbstractRow.of(null);

    assertThat(row).isNotNull();
    assertThat(row.values()).isNotNull();
    assertThat(row.values()).isEmpty();
  }

  @Test
  public void ofNullValuesReturnsEmptyRow() {

    AbstractRow row = AbstractRow.of(null);

    assertThat(row).isNotNull();
    assertThat(row.values()).isNotNull();
    assertThat(row.values()).isEmpty();
  }

  @Test
  public void setAndGetView() {

    AbstractRow row = new TestRow();

    View mockViewOne = mock(View.class);
    View mockViewTwo = mock(View.class);

    assertThat(row.getView()).isNotPresent();

    row.setView(mockViewOne);

    assertThat(row.getView()).isPresent();
    assertThat(row.getView().orElse(null)).isSameAs(mockViewOne);

    row.setView(mockViewTwo);

    assertThat(row.getView()).isPresent();
    assertThat(row.getView().orElse(null)).isSameAs(mockViewTwo);

    row.setView(null);

    assertThat(row.getView()).isNotPresent();

    verifyNoInteractions(mockViewOne, mockViewTwo);
  }

  @Test
  public void getViewReturnsEmptyByDefault() {

    AbstractRow row = new TestRow();

    Optional<View> view = row.getView();

    assertThat(view).isNotNull();
    assertThat(view).isNotPresent();
  }

  @Test(expected = MappingException.class)
  @SuppressWarnings("unchecked")
  public void mapToExplodingTypeThrowsMappingException() {

    AbstractRow row = spy(new TestRow());

    Column<Object> mockValueColumn = mock(Column.class);

    View mockView = mock(View.class);

    row.setView(mockView);

    when(mockView.getColumn(eq("value"))).thenReturn(Optional.of(mockValueColumn));
    doReturn("test").when(row).getValue(eq(mockValueColumn));

    try {
      row.map(ExplodingType.class);
    }
    catch(MappingException expected) {

      assertThat(expected).hasMessage("Failed to map object of type [%s] with values from this Row [%s]",
        ExplodingType.class.getName(), row);

      assertThat(expected).hasCauseInstanceOf(MethodInvocationException.class);
      assertThat(expected.getCause().getCause()).isInstanceOf(InvocationTargetException.class);
      assertThat(expected.getCause().getCause().getCause()).isInstanceOf(UnsupportedOperationException.class);
      assertThat(expected.getCause().getCause().getCause()).hasMessage("test");

      throw expected;
    }
    finally {
      verify(mockView, times(1)).getColumn(eq("value"));
      verify(row, times(1)).getValue(eq(mockValueColumn));
    }
  }

  @Test(expected = MappingException.class)
  public void mapToNonInstantiableTypeThrowsMappingException() {

    AbstractRow row = spy(new TestRow());

    View mockView = mock(View.class);

    row.setView(mockView);

    try {
      row.map(NonInstantiableType.class);
    }
    catch (MappingException expected) {

      assertThat(expected).hasMessage("Failed to map object of type [%s] with values from this Row [%s]",
        NonInstantiableType.class.getName(), row);

      assertThat(expected).hasCauseInstanceOf(IllegalAccessException.class);

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void mapWithNullTypeThrowsIllegalArgumentException() {

    AbstractRow row = new TestRow();

    try {
      row.map(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Class type of the object to map is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalStateException.class)
  public void mapWithNullViewThrowsIllegalStateException() {

    AbstractRow row = new TestRow();

    assertThat(row.getView().isPresent()).isFalse();

    try {
      row.map(Person.class);
    }
    catch (IllegalStateException expected) {

      assertThat(expected).hasMessage("This Row [%s] is not associated with a View", row);
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void rowMapsFemalePersonCorrectly() {

    AbstractRow row = spy(new TestRow());

    Column<Gender> mockGenderColumn = mock(Column.class);
    Column<String> mockNameColumn = mock(Column.class);

    View mockView = mock(View.class);

    when(mockView.<Gender>getColumn(eq("gender"))).thenReturn(Optional.of(mockGenderColumn));
    when(mockView.<String>getColumn(eq("name"))).thenReturn(Optional.of(mockNameColumn));

    row.setView(mockView);

    doReturn(Gender.MALE).when(row).getValue(eq(mockGenderColumn));
    doReturn("Jane Doe").when(row).getValue(eq(mockNameColumn));

    FemalePerson janeDoe = row.map(FemalePerson.class);

    assertThat(janeDoe).isNotNull();
    assertThat(janeDoe.getName()).isEqualTo("Jane Doe");
    assertThat(janeDoe.getGender()).isEqualTo(Gender.FEMALE);

    verify(mockView, times(1)).getColumn(eq("birthDate"));
    verify(mockView, never()).getColumn(eq("gender"));
    verify(mockView, times(1)).getColumn(eq("name"));
    verify(row, never()).getValue(eq("birthDate"));
    verify(row, never()).getValue(eq(mockGenderColumn));
    verify(row, times(1)).getValue(eq(mockNameColumn));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void rowMapsPersonCorrectly() {

    AbstractRow row = spy(new TestRow());

    Column<Gender> mockGenderColumn = mock(Column.class);
    Column<String> mockNameColumn = mock(Column.class);

    View mockView = mock(View.class);

    when(mockView.<Gender>getColumn(eq("gender"))).thenReturn(Optional.of(mockGenderColumn));
    when(mockView.<String>getColumn(eq("name"))).thenReturn(Optional.of(mockNameColumn));

    row.setView(mockView);

    doReturn(Gender.MALE).when(row).getValue(eq(mockGenderColumn));
    doReturn("Jon Doe").when(row).getValue(eq(mockNameColumn));

    Person jonDoe = row.map(Person.class);

    assertThat(jonDoe).isNotNull();
    assertThat(jonDoe.getName()).isEqualTo("Jon Doe");
    assertThat(jonDoe.getGender()).isEqualTo(Gender.MALE);

    verify(mockView, times(1)).getColumn(eq("gender"));
    verify(mockView, times(1)).getColumn(eq("name"));
    verify(row, times(1)).getValue(eq(mockGenderColumn));
    verify(row, times(1)).getValue(eq(mockNameColumn));
  }

  @Test
  public void toStringIsCorrect() {

    AbstractRow row = spy(new TestRow());

    doReturn(4).when(row).index();

    assertThat(row.toString()).isEqualTo("Row 4");

    verify(row, times(1)).index();
  }

  @Test
  public void valuesReturnsEmptyObjectArray() {

    AbstractRow row = new TestRow();

    Object[] values = row.values();

    assertThat(values).isNotNull();
    assertThat(values).isEmpty();
  }

  static class TestRow extends AbstractRow {

    @Override
    public <T> T setValue(int columnIndex, T value) {
      throw newUnsupportedOperationException(Constants.NOT_IMPLEMENTED);
    }
  }

  @NoArgsConstructor
  @SuppressWarnings("unused")
  private static final class ExplodingType {

    @Getter
    private Object value;

    public void setValue(Object value) {
      throw new UnsupportedOperationException("test");
    }
  }

  private static final class NonInstantiableType {

    private NonInstantiableType() { }

  }

  @NoArgsConstructor
  private static class FemalePerson {

    @Getter
    private final Gender gender = Gender.FEMALE;

    @Getter @Setter
    private LocalDateTime birthDate;

    @Getter @Setter
    private String name;

  }

  @Data
  @NoArgsConstructor
  @SuppressWarnings("unused")
  private static class Person {

    private Gender gender;
    private String name;

  }
}
