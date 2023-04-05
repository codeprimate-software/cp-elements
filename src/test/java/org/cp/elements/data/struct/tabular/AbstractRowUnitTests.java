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
import static org.assertj.core.api.Assertions.assertThatIllegalStateException;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newUnsupportedOperationException;
import static org.cp.elements.lang.ThrowableAssertions.assertThatThrowableOfType;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

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

  private static void assertPerson(Person person, String expectedName, Gender expectedGender) {

    assertThat(person).isNotNull();
    assertThat(person.getGender()).isEqualTo(expectedGender);
    assertThat(person.getName()).isEqualTo(expectedName);
  }

  private static void assertPerson(FemalePerson person, String expectedName, Gender expectedGender) {

    assertThat(person).isNotNull();
    assertThat(person.getGender()).isEqualTo(expectedGender);
    assertThat(person.getName()).isEqualTo(expectedName);
  }

  private static Person newPerson(String name, Gender gender) {

    Person person = new Person();

    person.setName(name);
    person.setGender(gender);

    return person;
  }

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

  @Test
  public void mapToExplodingTypeThrowsMappingException() {

    View mockView = mock(View.class);

    Column<?> mockValueColumn = mock(Column.class);

    AbstractRow row = spy(new TestRow());

    doReturn(Optional.of(mockView)).when(row).getView();
    doReturn(Optional.of(mockValueColumn)).when(mockView).getColumn(eq("value"));
    doReturn("test").when(row).getValue(eq(mockValueColumn));
    doReturn(2).when(row).index();

    assertThatThrowableOfType(MappingException.class)
      .isThrownBy(args -> row.map(ExplodingType.class))
      .havingMessage("Failed to map object of type [%s] with values from this Row [2]",
        ExplodingType.class.getName())
      .causedBy(MethodInvocationException.class)
      .causedBy(InvocationTargetException.class)
      .causedBy(UnsupportedOperationException.class)
      .havingMessage("test")
      .withNoCause();

    verify(row, times(1)).map(eq(ExplodingType.class));
    verify(row, times(1)).getView();
    verify(row, times(1)).getValue(eq(mockValueColumn));
    verify(row, times(2)).index();
    verify(mockView, times(1)).getColumn(eq("value"));
    verifyNoMoreInteractions(row, mockView);
    verifyNoInteractions(mockValueColumn);
  }

  @Test
  public void mapToNonInstantiableTypeThrowsMappingException() {

    View mockView = mock(View.class);

    AbstractRow row = spy(new TestRow());

    doReturn(Optional.of(mockView)).when(row).getView();
    doReturn(4).when(row).index();

    assertThatThrowableOfType(MappingException.class)
      .isThrownBy(args -> row.map(NonInstantiableType.class))
      .havingMessage("Failed to map object of type [%s] with values from this Row [4]",
        NonInstantiableType.class.getName())
      .causedBy(IllegalAccessException.class)
      .withNoCause();

    verify(row, times(1)).map(eq(NonInstantiableType.class));
    verify(row, times(1)).getView();
    verify(row, times(2)).index();
    verifyNoMoreInteractions(row);
    verifyNoInteractions(mockView);
  }

  @Test
  public void mapWithNoViewThrowsIllegalStateException() {

    AbstractRow row = spy(TestRow.class);

    doReturn(8).when(row).index();
    doReturn(Optional.empty()).when(row).getView();

    assertThatIllegalStateException()
      .isThrownBy(() -> row.map(Person.class))
      .withMessage("Row [8] is not associated with a View")
      .withNoCause();

    verify(row, times(1)).map(eq(Person.class));
    verify(row, times(2)).index();
    verify(row, times(1)).getView();
    verifyNoMoreInteractions(row);
  }

  @Test
  public void mapWithNullTypeThrowsIllegalArgumentException() {

    AbstractRow row = spy(new TestRow());

    doReturn(16).when(row).index();

    assertThatIllegalArgumentException()
      .isThrownBy(() -> row.map(null))
      .withMessage("Class type of the object to map from this Row [16] is required")
      .withNoCause();

    verify(row, times(1)).map(isNull());
    verify(row, times(1)).index();
    verifyNoMoreInteractions(row);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void rowMapsFemalePersonCorrectly() {

    View mockView = mock(View.class);

    Column<Gender> mockGenderColumn = mock(Column.class);
    Column<String> mockNameColumn = mock(Column.class);

    AbstractRow row = spy(new TestRow());

    doReturn(Optional.of(mockGenderColumn)).when(mockView).getColumn(eq("gender"));
    doReturn(Optional.of(mockNameColumn)).when(mockView).getColumn(eq("name"));
    doReturn(Optional.ofNullable(mockView)).when(row).getView();
    doReturn(Gender.MALE).when(row).getValue(eq(mockGenderColumn));
    doReturn("Jane Doe").when(row).getValue(eq(mockNameColumn));
    doReturn(16).when(row).index();

    FemalePerson janeDoe = row.map(FemalePerson.class);

    assertPerson(janeDoe, "Jane Doe", Gender.FEMALE);

    verify(row, times(1)).map(eq(FemalePerson.class));
    verify(row, times(1)).index();
    verify(row, times(1)).getView();
    verify(row, times(1)).getValue(eq(mockNameColumn));
    verify(row, never()).getValue(eq(mockGenderColumn));
    verify(row, never()).getValue(eq("birthDate"));
    verify(mockView, times(1)).getColumn(eq("name"));
    verify(mockView, never()).getColumn(eq("gender"));
    verify(mockView, times(1)).getColumn(eq("birthDate"));
    verifyNoMoreInteractions(row, mockView);
    verifyNoInteractions(mockGenderColumn, mockNameColumn);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void rowMapsPersonCorrectly() {

    View mockView = mock(View.class);

    Column<Gender> mockGenderColumn = mock(Column.class);
    Column<String> mockNameColumn = mock(Column.class);

    AbstractRow row = spy(new TestRow());

    doReturn(Optional.of(mockGenderColumn)).when(mockView).getColumn(eq("gender"));
    doReturn(Optional.of(mockNameColumn)).when(mockView).getColumn(eq("name"));
    doReturn(64).when(mockView).indexOf(eq(row));
    doReturn(Optional.of(mockView)).when(row).getView();
    doReturn(Gender.MALE).when(row).getValue(eq(mockGenderColumn));
    doReturn("Jon Doe").when(row).getValue(eq(mockNameColumn));

    Person jonDoe = row.map(Person.class);

    assertPerson(jonDoe, "Jon Doe", Gender.MALE);

    verify(mockView, times(1)).getColumn(eq("gender"));
    verify(mockView, times(1)).getColumn(eq("name"));
    verify(mockView, times(1)).indexOf(eq(row));
    verify(row, times(1)).map(eq(Person.class));
    verify(row, times(2)).getView();
    verify(row, times(1)).getValue(eq(mockGenderColumn));
    verify(row, times(1)).getValue(eq(mockNameColumn));
    verify(row, times(1)).index();
    verifyNoMoreInteractions(mockView, row);
    verifyNoInteractions(mockGenderColumn, mockNameColumn);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void storePersonToTable() {

    View mockView = mock(View.class);

    Column<Gender> mockGenderColumn = mock(Column.class);
    Column<Gender> mockNameColumn = mock(Column.class);

    AbstractRow row = spy(TestRow.class);

    doReturn(Optional.of(mockGenderColumn)).when(mockView).getColumn(eq("gender"));
    doReturn(Optional.of(mockNameColumn)).when(mockView).getColumn(eq("name"));
    doReturn(128).when(mockView).indexOf(eq(row));
    doReturn(Optional.of(mockView)).when(row).getView();
    doReturn(null).when(row).setValue(eq(mockNameColumn), any());
    doReturn(null).when(row).setValue(eq(mockGenderColumn), any());

    Person jonDoe = newPerson("Jon Doe", Gender.MALE);

    row.store(jonDoe);

    verify(mockView, times(1)).indexOf(eq(row));
    verify(mockView, times(1)).getColumn(eq("gender"));
    verify(mockView, times(1)).getColumn(eq("name"));
    verify(row, times(1)).store(eq(jonDoe));
    verify(row, times(1)).index();
    verify(row, times(2)).getView();
    verify(row, times(1)).setValue(eq(mockGenderColumn), eq(jonDoe.getGender()));
    verify(row, times(1)).setValue(eq(mockNameColumn), eq(jonDoe.getName()));
    verifyNoMoreInteractions(mockView, row);
    verifyNoInteractions(mockGenderColumn, mockNameColumn);
  }

  @Test
  public void storeNullObjectThrowsIllegalArgumentException() {

    AbstractRow row = spy(TestRow.class);

    View mockView = mock(View.class);

    doReturn(256).when(mockView).indexOf(eq(row));
    doReturn(Optional.of(mockView)).when(row).getView();

    assertThatIllegalArgumentException()
      .isThrownBy(() -> row.store(null))
      .withMessage("Object to map to this Row [256] is required")
      .withNoCause();

    verify(row, times(1)).store(isNull());
    verify(row, times(1)).index();
    verify(row, times(1)).getView();
    verify(mockView, times(1)).indexOf(eq(row));
    verifyNoMoreInteractions(mockView, row);
  }

  @Test
  public void storeWithNoViewThrowsIllegalStateException() {

    Object target = new Object();

    AbstractRow row = spy(TestRow.class);

    doReturn(Optional.empty()).when(row).getView();

    assertThatIllegalStateException()
      .isThrownBy(() -> row.store(target))
      .withMessage("Row [-1] is not associated with a View")
      .withNoCause();

    verify(row, times(1)).store(eq(target));
    verify(row, times(2)).index();
    verify(row, times(3)).getView();
    verifyNoMoreInteractions(row);
  }

  @Test
  public void toStringIsCorrect() {

    AbstractRow row = spy(new TestRow());

    doReturn(4).when(row).index();

    assertThat(row.toString()).isEqualTo("Row 4");

    verify(row, times(1)).index();
    verifyNoMoreInteractions(row);
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
  private static class Person {

    private Gender gender;
    private String name;

  }
}
