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
package org.cp.elements.beans.model.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.beans.PropertyDescriptor;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.SortedSet;
import java.util.function.Consumer;

import org.cp.elements.beans.model.BeanModel;
import org.cp.elements.beans.model.Property;
import org.cp.elements.security.model.User;
import org.junit.Test;

/**
 * Unit Tests for {@link ListProperty}.
 *
 * @author John Blum
 * @see java.util.List
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.beans.model.support.ListProperty
 * @since 1.0.0
 */
public class ListPropertyUnitTests {

  private PropertyDescriptor mockPropertyDescriptor(Class<?> propertyType) {

    PropertyDescriptor mockPropertyDescriptor = mock(PropertyDescriptor.class);

    doReturn(propertyType).when(mockPropertyDescriptor).getPropertyType();

    return mockPropertyDescriptor;
  }

  private ListProperty newListProperty() {
    return new ListProperty(mock(BeanModel.class), mockPropertyDescriptor(List.class));
  }

  private void testIsListTypedProperty(Class<?> propertyType) {
    testIsListTypedWithPropertyOfType(propertyType, propertyDescriptor ->
      assertThat(ListProperty.isListType(propertyDescriptor)).isTrue());
  }

  private void testIsNotListTypedProperty(Class<?> propertyType) {
    testIsListTypedWithPropertyOfType(propertyType, propertyDescriptor ->
      assertThat(ListProperty.isListType(propertyDescriptor)).isFalse());
  }

  private void testIsListTypedWithPropertyOfType(Class<?> propertyType,
      Consumer<PropertyDescriptor> propertyDescriptorAssertion) {

    PropertyDescriptor mockPropertyDescriptor = mockPropertyDescriptor(propertyType);

    propertyDescriptorAssertion.accept(mockPropertyDescriptor);

    verify(mockPropertyDescriptor, times(1)).getPropertyType();
    verifyNoMoreInteractions(mockPropertyDescriptor);
  }

  @Test
  public void assertListTypeWithListBasedPropertyDescriptor() {

    PropertyDescriptor mockPropertyDescriptor = mockPropertyDescriptor(List.class);

    assertThat(ListProperty.assertListType(mockPropertyDescriptor)).isSameAs(mockPropertyDescriptor);

    verify(mockPropertyDescriptor, times(1)).getPropertyType();
    verifyNoMoreInteractions(mockPropertyDescriptor);
  }

  @Test
  public void assertListTypeWithNonListBasedPropertyDescriptor() {

    PropertyDescriptor mockPropertyDescriptor = mockPropertyDescriptor(Object.class);

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ListProperty.assertListType(mockPropertyDescriptor))
      .withMessage("Property [%s] must be a List", mockPropertyDescriptor)
      .withNoCause();

    verify(mockPropertyDescriptor, times(1)).getPropertyType();
    verifyNoMoreInteractions(mockPropertyDescriptor);
  }

  @Test
  public void assertListTypeWithNullThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ListProperty.assertListType(null))
      .withMessage("PropertyDescriptor is required")
      .withNoCause();
  }

  @Test
  public void isListTypedWithArrayListProperty() {
    testIsListTypedProperty(ArrayList.class);
  }

  @Test
  public void isListTypedWithListProperty() {
    testIsListTypedProperty(List.class);
  }

  @Test
  public void isNotListTypedWithNullTypedProperty() {
    testIsNotListTypedProperty(null);
  }

  @Test
  public void isNotListTypedWithObjectProperty() {
    testIsNotListTypedProperty(Object.class);
  }

  @Test
  public void isNotListTypedWithSetProperty() {
    testIsNotListTypedProperty(SortedSet.class);
  }

  @Test
  public void fromBeanModelAndPropertyDescriptor() {

    BeanModel mockBeanModel = mock(BeanModel.class);

    PropertyDescriptor mockPropertyDescriptor = mockPropertyDescriptor(List.class);

    ListProperty property = ListProperty.from(mockBeanModel, mockPropertyDescriptor);

    assertThat(property).isNotNull();
    assertThat(property.getBeanModel()).isEqualTo(mockBeanModel);
    assertThat(property.getDescriptor()).isEqualTo(mockPropertyDescriptor);

    verify(mockPropertyDescriptor, times(1)).getPropertyType();
    verifyNoMoreInteractions(mockPropertyDescriptor);
    verifyNoInteractions(mockBeanModel);
  }

  @Test
  public void fromProperty() {

    BeanModel mockBeanModel = mock(BeanModel.class);

    PropertyDescriptor mockPropertyDescriptor = mockPropertyDescriptor(ArrayList.class);

    Property mockProperty = mock(Property.class);

    doReturn(mockBeanModel).when(mockProperty).getBeanModel();
    doReturn(mockPropertyDescriptor).when(mockProperty).getDescriptor();

    ListProperty property = ListProperty.from(mockProperty);

    assertThat(property).isNotNull();
    assertThat(property.getBeanModel()).isEqualTo(mockBeanModel);
    assertThat(property.getDescriptor()).isEqualTo(mockPropertyDescriptor);

    verify(mockProperty, times(1)).getBeanModel();
    verify(mockProperty, times(1)).getDescriptor();
    verify(mockPropertyDescriptor, times(1)).getPropertyType();
    verifyNoMoreInteractions(mockPropertyDescriptor, mockProperty);
    verifyNoInteractions(mockBeanModel);
  }

  @Test
  public void fromNullProperty() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ListProperty.from(null))
      .withMessage("Property is required")
      .withNoCause();
  }

  @Test
  public void constructNewListProperty() {

    BeanModel mockBeanModel = mock(BeanModel.class);

    PropertyDescriptor mockPropertyDescriptor = mockPropertyDescriptor(List.class);

    ListProperty property = new ListProperty(mockBeanModel, mockPropertyDescriptor);

    assertThat(property.getBeanModel()).isEqualTo(mockBeanModel);
    assertThat(property.getDescriptor()).isEqualTo(mockPropertyDescriptor);

    verify(mockPropertyDescriptor, times(1)).getPropertyType();
    verifyNoMoreInteractions(mockPropertyDescriptor);
    verifyNoInteractions(mockBeanModel);
  }

  @Test
  public void constructNewListPropertyWithNullBeanModel() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new ListProperty(null, mockPropertyDescriptor(List.class)))
      .withMessage("BeanModel is required")
      .withNoCause();
  }

  @Test
  public void constructNewListPropertyWithNullPropertyDescriptor() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new ListProperty(mock(BeanModel.class), null))
      .withMessage("PropertyDescriptor is required")
      .withNoCause();
  }

  @Test
  public void constructNewListPropertyWithNonListTypedPropertyDescriptor() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new ListProperty(mock(BeanModel.class), mockPropertyDescriptor(User.class)))
      .withMessageMatching("Property \\[.*\\] must be a List")
      .withNoCause();
  }

  @Test
  public void listIndexIsCorrect() {
    assertThat(newListProperty().assertListIndex(Arrays.asList(1, 2, 3), 2)).isEqualTo(2);
  }

  @Test
  public void negativeListIndexIsNotValid() {

    assertThatExceptionOfType(IndexOutOfBoundsException.class)
      .isThrownBy(() -> newListProperty().assertListIndex(Collections.singletonList("mock"), -1))
      .withMessage("List index [-1] must be greater than equal to 0 and less than [1]")
      .withNoCause();
  }

  @Test
  public void overflowListIndexIsNotValid() {

    assertThatExceptionOfType(IndexOutOfBoundsException.class)
      .isThrownBy(() -> newListProperty().assertListIndex(Collections.emptyList(), 0))
      .withMessage("List index [0] must be greater than equal to 0 and less than [0]")
      .withNoCause();
  }
}
