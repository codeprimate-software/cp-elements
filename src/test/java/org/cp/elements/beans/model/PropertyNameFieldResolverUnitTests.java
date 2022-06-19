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
package org.cp.elements.beans.model;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.lang.reflect.Field;

import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.junit.Test;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NonNull;
import lombok.ToString;

/**
 * Unit Tests for {@link PropertyNameFieldResolver}.
 *
 * @author John Blum
 * @see java.lang.reflect.Field
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.beans.model.Property
 * @see org.cp.elements.beans.model.PropertyNameFieldResolver
 * @since 1.0.0
 */
public class PropertyNameFieldResolverUnitTests {

  @Test
  public void getDeclaredFieldFromProperty() {

    Person jonDoe = Person.as("jonDoe");

    Property mockProperty = mock(Property.class);

    doReturn(jonDoe).when(mockProperty).getTargetObject();
    doReturn("name").when(mockProperty).getName();

    Field field = new PropertyNameFieldResolver().resolve(mockProperty);

    assertThat(field).isNotNull();
    assertThat(field.getName()).isEqualTo("name");

    verify(mockProperty, times(1)).getTargetObject();
    verify(mockProperty, times(1)).getName();
    verifyNoMoreInteractions(mockProperty);
  }

  @Test
  public void getInheritedFieldFromProperty() {

    Customer janeDoe = Customer.as("janeDoe");

    Property mockProperty = mock(Property.class);

    doReturn(janeDoe).when(mockProperty).getTargetObject();
    doReturn("name").when(mockProperty).getName();

    Field field = new PropertyNameFieldResolver().resolve(mockProperty);

    assertThat(field).isNotNull();
    assertThat(field.getName()).isEqualTo("name");

    verify(mockProperty, times(1)).getTargetObject();
    verify(mockProperty, times(1)).getName();
    verifyNoMoreInteractions(mockProperty);
  }

  @Test
  public void getNonExistingFieldFromProperty() {

    Person pieDoe = Person.as("pieDoe");

    Property mockProperty = mock(Property.class);

    doReturn(pieDoe).when(mockProperty).getTargetObject();
    doReturn("age").when(mockProperty).getName();

    Field field = new PropertyNameFieldResolver().resolve(mockProperty);

    assertThat(field).isNull();

    verify(mockProperty, times(1)).getTargetObject();
    verify(mockProperty, times(1)).getName();
    verifyNoMoreInteractions(mockProperty);
  }

  @Test
  public void getFieldFromNullProperty() {
    assertThat(new PropertyNameFieldResolver().resolve(null)).isNull();
  }

  @Getter
  @EqualsAndHashCode
  @ToString(of = "name")
  private static class Person {

    private static Person as(@NotNull String name) {
      return new Person(name);
    }

    @NonNull
    private final String name;

    private Person(@NotNull String name) {
      this.name = ObjectUtils.requireObject(name, "Name is required");
    }
  }

  private static class Customer extends Person {

    private static Customer as(@NotNull String name) {
      return new Customer(name);
    }

    private Customer(@NotNull String name) {
      super(name);
    }
  }
}
