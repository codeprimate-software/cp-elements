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
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.beans.PropertyDescriptor;

import org.cp.elements.beans.model.BeanModel;
import org.cp.elements.beans.model.Property;
import org.cp.elements.security.model.User;
import org.junit.Test;

/**
 * Unit Tests for {@link ArrayProperty}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.beans.model.support.ArrayProperty
 * @since 1.0.0
 */
public class ArrayPropertyUnitTests {

  private PropertyDescriptor mockPropertyDescriptor(Class<?> propertyType) {

    PropertyDescriptor mockPropertyDescriptor = mock(PropertyDescriptor.class);

    doReturn(propertyType).when(mockPropertyDescriptor).getPropertyType();

    return mockPropertyDescriptor;
  }

  private ArrayProperty newArrayProperty() {
    return new ArrayProperty(mock(BeanModel.class), mockPropertyDescriptor(Object[].class));
  }

  private void testIsArrayTypeForArrayTypedProperty(Class<?> propertyType) {

    PropertyDescriptor mockPropertyDescriptor = mockPropertyDescriptor(propertyType);

    assertThat(ArrayProperty.isArrayType(mockPropertyDescriptor)).isTrue();

    verify(mockPropertyDescriptor, times(1)).getPropertyType();
    verifyNoMoreInteractions(mockPropertyDescriptor);
  }

  @Test
  public void isArrayTypeForArrayTypedPropertyDescriptor() {
    testIsArrayTypeForArrayTypedProperty(User[].class);
  }

  @Test
  public void isArrayTypeForNonArrayTypedPropertyDescriptor() {

    PropertyDescriptor mockPropertyDescriptor = mockPropertyDescriptor(User.class);

    assertThat(ArrayProperty.isArrayType(mockPropertyDescriptor)).isFalse();

    verify(mockPropertyDescriptor, times(1)).getPropertyType();
    verifyNoMoreInteractions(mockPropertyDescriptor);
  }

  @Test
  public void isArrayTypeForTwoDimensionalArrayPropertyDescriptor() {
    testIsArrayTypeForArrayTypedProperty(Object[][].class);
  }

  @Test
  public void assertArrayTypeWithArrayTypedPropertyDescriptor() {

    PropertyDescriptor mockPropertyDescriptor = mockPropertyDescriptor(Object[].class);

    assertThat(ArrayProperty.assertArrayType(mockPropertyDescriptor)).isSameAs(mockPropertyDescriptor);

    verify(mockPropertyDescriptor, times(1)).getPropertyType();
    verifyNoMoreInteractions(mockPropertyDescriptor);
  }

  @Test
  public void assertArrayTypeWithNonArrayTypedPropertyDescriptor() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ArrayProperty.assertArrayType(mockPropertyDescriptor(Object.class)))
      .withMessageMatching("Property \\[.*\\] must be an array")
      .withNoCause();
  }

  @Test
  public void fromBeanModelAndPropertyDescriptor() {

    BeanModel mockBeanModel = mock(BeanModel.class);

    PropertyDescriptor mockPropertyDescriptor = mockPropertyDescriptor(Object[].class);

    ArrayProperty property = ArrayProperty.from(mockBeanModel, mockPropertyDescriptor);

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

    PropertyDescriptor mockPropertyDescriptor = mockPropertyDescriptor(Object[].class);

    Property mockProperty = mock(Property.class);

    doReturn(mockBeanModel).when(mockProperty).getBeanModel();
    doReturn(mockPropertyDescriptor).when(mockProperty).getDescriptor();
    doCallRealMethod().when(mockProperty).getType();

    ArrayProperty property = ArrayProperty.from(mockProperty);

    assertThat(property).isNotNull();
    assertThat(property.getBeanModel()).isEqualTo(mockBeanModel);
    assertThat(property.getDescriptor()).isEqualTo(mockPropertyDescriptor);

    verify(mockProperty, times(1)).getBeanModel();
    verify(mockProperty, times(1)).getDescriptor();
    verify(mockPropertyDescriptor, times(1)).getPropertyType();
    verifyNoMoreInteractions(mockProperty, mockPropertyDescriptor);
    verifyNoInteractions(mockBeanModel);
  }

  @Test
  public void fromNullProperty() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ArrayProperty.from(null))
      .withMessage("Property is required")
      .withNoCause();
  }

  @Test
  public void constructNewArrayProperty() {

    BeanModel mockBeanModel = mock(BeanModel.class);

    PropertyDescriptor mockPropertyDescriptor = mockPropertyDescriptor(Object[].class);

    ArrayProperty property = new ArrayProperty(mockBeanModel, mockPropertyDescriptor);

    assertThat(property).isNotNull();
    assertThat(property.getBeanModel()).isEqualTo(mockBeanModel);
    assertThat(property.getDescriptor()).isEqualTo(mockPropertyDescriptor);

    verify(mockPropertyDescriptor, times(1)).getPropertyType();
    verifyNoMoreInteractions(mockPropertyDescriptor);
    verifyNoInteractions(mockBeanModel);
  }

  @Test
  public void constructNewArrayPropertyWithNullBeanModel() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new ArrayProperty(null, mockPropertyDescriptor(Object[].class)))
      .withMessage("BeanModel is required")
      .withNoCause();
  }

  @Test
  public void constructNewArrayPropertyWithNullPropertyDescriptor() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new ArrayProperty(mock(BeanModel.class), null))
      .withMessage("PropertyDescriptor is required")
      .withNoCause();
  }

  @Test
  public void constructNewArrayPropertyWithNonArrayTypedPropertyDescriptor() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new ArrayProperty(mock(BeanModel.class), mockPropertyDescriptor(User.class)))
      .withMessageMatching("Property \\[.*\\] must be an array")
      .withNoCause();
  }

  @Test
  public void arrayIndexIsValid() {
    assertThat(newArrayProperty().assertArrayIndex(new Object[] { 1, 2, 3 }, 1)).isEqualTo(1);
  }

  @Test
  public void negativeArrayIndexIsNotValid() {

    assertThatExceptionOfType(ArrayIndexOutOfBoundsException.class)
      .isThrownBy(() -> newArrayProperty().assertArrayIndex(new Object[] { 1, 2, 3 }, -1))
      .withMessage("Array index [-1] must be greater than equal to 0 and less than [3]")
      .withNoCause();
  }

  @Test
  public void overflowArrayIndexIsNotValid() {

    assertThatExceptionOfType(ArrayIndexOutOfBoundsException.class)
      .isThrownBy(() -> newArrayProperty().assertArrayIndex(new Object[] { 1, 2, 3 }, 3))
      .withMessage("Array index [3] must be greater than equal to 0 and less than [3]")
      .withNoCause();
  }
}
