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
import java.util.Collections;
import java.util.List;
import java.util.NavigableSet;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentSkipListSet;

import org.cp.elements.beans.model.BeanModel;
import org.cp.elements.beans.model.Property;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.util.CollectionUtils;
import org.junit.Test;

/**
 * Unit Tests for {@link SortedSetProperty}.
 *
 * @author John Blum
 * @see java.beans.PropertyDescriptor
 * @see java.util.SortedSet
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.beans.model.BeanModel
 * @see org.cp.elements.beans.model.Property
 * @see org.cp.elements.beans.model.support.SortedSetProperty
 * @since 1.0.0
 */
public class SortedSetPropertyUnitTests {

  private @NotNull PropertyDescriptor mockPropertyDescriptor(@NotNull Class<?> propertyType) {

    PropertyDescriptor mockPropertyDescriptor = mock(PropertyDescriptor.class);

    doReturn(propertyType).when(mockPropertyDescriptor).getPropertyType();

    return mockPropertyDescriptor;
  }

  private SortedSetProperty newSortedSetProperty() {
    return new SortedSetProperty(mock(BeanModel.class), mockPropertyDescriptor(SortedSet.class));
  }

  @Test
  public void assertSortedSetTypeWithSortedSetPropertyDescriptor() {

    PropertyDescriptor mockPropertyDescriptor = mockPropertyDescriptor(TreeSet.class);

    assertThat(SortedSetProperty.assertSortedSetType(mockPropertyDescriptor)).isEqualTo(mockPropertyDescriptor);

    verify(mockPropertyDescriptor, times(1)).getPropertyType();
    verifyNoMoreInteractions(mockPropertyDescriptor);
  }

  @Test
  public void assertSortedSetTypeWithNullThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> SortedSetProperty.assertSortedSetType(null))
      .withMessage("PropertyDescriptor is required")
      .withNoCause();
  }

  @Test
  public void assertSortedSetTypeWithNonSortedSetPropertyDescriptor() {

    PropertyDescriptor mockPropertyDescriptor = mockPropertyDescriptor(Set.class);

    assertThatIllegalArgumentException()
      .isThrownBy(() -> SortedSetProperty.assertSortedSetType(mockPropertyDescriptor))
      .withMessage("Property [%s] must be a SortedSet", mockPropertyDescriptor)
      .withNoCause();

    verify(mockPropertyDescriptor, times(1)).getPropertyType();
    verifyNoMoreInteractions(mockPropertyDescriptor);
  }

  @Test
  public void isSortedSetTypeWithConcurrentSkipListSetReturnsTrue() {

    PropertyDescriptor mockPropertyDescriptor = mockPropertyDescriptor(ConcurrentSkipListSet.class);

    assertThat(SortedSetProperty.isSortedSetType(mockPropertyDescriptor)).isTrue();

    verify(mockPropertyDescriptor, times(1)).getPropertyType();
    verifyNoMoreInteractions(mockPropertyDescriptor);
  }

  @Test
  public void isSortedSetTypeWithNavigableSetReturnsTrue() {

    PropertyDescriptor mockPropertyDescriptor = mockPropertyDescriptor(NavigableSet.class);

    assertThat(SortedSetProperty.isSortedSetType(mockPropertyDescriptor)).isTrue();

    verify(mockPropertyDescriptor, times(1)).getPropertyType();
    verifyNoMoreInteractions(mockPropertyDescriptor);
  }

  @Test
  public void isSortedSetTypeWithSortedSetReturnsTrue() {

    PropertyDescriptor mockPropertyDescriptor = mockPropertyDescriptor(SortedSet.class);

    assertThat(SortedSetProperty.isSortedSetType(mockPropertyDescriptor)).isTrue();

    verify(mockPropertyDescriptor, times(1)).getPropertyType();
    verifyNoMoreInteractions(mockPropertyDescriptor);
  }

  @Test
  public void isSortedSetTypeWithTreeSetReturnsTrue() {

    PropertyDescriptor mockPropertyDescriptor = mockPropertyDescriptor(TreeSet.class);

    assertThat(SortedSetProperty.isSortedSetType(mockPropertyDescriptor)).isTrue();

    verify(mockPropertyDescriptor, times(1)).getPropertyType();
    verifyNoMoreInteractions(mockPropertyDescriptor);
  }

  @Test
  public void isSortedSetTypeWithListReturnsFalse() {

    PropertyDescriptor mockPropertyDescriptor = mockPropertyDescriptor(List.class);

    assertThat(SortedSetProperty.isSortedSetType(mockPropertyDescriptor)).isFalse();

    verify(mockPropertyDescriptor, times(1)).getPropertyType();
    verifyNoMoreInteractions(mockPropertyDescriptor);
  }

  @Test
  public void isSortedSetTypeWithNullIsNullSafeReturnsFalse() {
    assertThat(SortedSetProperty.isSortedSetType(null)).isFalse();
  }

  @Test
  public void isSortedSetTypeWithObjectReturnsFalse() {

    PropertyDescriptor mockPropertyDescriptor = mockPropertyDescriptor(Object.class);

    assertThat(SortedSetProperty.isSortedSetType(mockPropertyDescriptor)).isFalse();

    verify(mockPropertyDescriptor, times(1)).getPropertyType();
    verifyNoMoreInteractions(mockPropertyDescriptor);
  }

  @Test
  public void isSortedSetTypeWithSetReturnsFalse() {

    PropertyDescriptor mockPropertyDescriptor = mockPropertyDescriptor(Set.class);

    assertThat(SortedSetProperty.isSortedSetType(mockPropertyDescriptor)).isFalse();

    verify(mockPropertyDescriptor, times(1)).getPropertyType();
    verifyNoMoreInteractions(mockPropertyDescriptor);
  }

  @Test
  public void fromBeanModelAndPropertyDescriptor() {

    BeanModel mockBeanModel = mock(BeanModel.class);

    PropertyDescriptor mockPropertyDescriptor = mockPropertyDescriptor(SortedSet.class);

    SortedSetProperty property = SortedSetProperty.from(mockBeanModel, mockPropertyDescriptor);

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

    PropertyDescriptor mockPropertyDescriptor = mockPropertyDescriptor(NavigableSet.class);

    Property mockProperty = mock(Property.class);

    doReturn(mockBeanModel).when(mockProperty).getBeanModel();
    doReturn(mockPropertyDescriptor).when(mockProperty).getDescriptor();

    SortedSetProperty property = SortedSetProperty.from(mockProperty);

    assertThat(property).isNotNull();
    assertThat(property.getBeanModel()).isEqualTo(mockBeanModel);
    assertThat(property.getDescriptor()).isEqualTo(mockPropertyDescriptor);

    verify(mockPropertyDescriptor, times(1)).getPropertyType();
    verify(mockProperty, times(1)).getBeanModel();
    verify(mockProperty, times(1)).getDescriptor();
    verifyNoMoreInteractions(mockProperty, mockPropertyDescriptor);
    verifyNoInteractions(mockBeanModel);
  }

  @Test
  public void fromNullProperty() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> SortedSetProperty.from(null))
      .withMessage("Property is required")
      .withNoCause();
  }

  @Test
  public void constructNewSortedSetProperty() {

    BeanModel mockBeanModel = mock(BeanModel.class);

    PropertyDescriptor mockPropertyDescriptor = mockPropertyDescriptor(SortedSet.class);

    SortedSetProperty property = new SortedSetProperty(mockBeanModel, mockPropertyDescriptor);

    assertThat(property).isNotNull();
    assertThat(property.getBeanModel()).isEqualTo(mockBeanModel);
    assertThat(property.getDescriptor()).isEqualTo(mockPropertyDescriptor);

    verify(mockPropertyDescriptor, times(1)).getPropertyType();
    verifyNoMoreInteractions(mockPropertyDescriptor);
    verifyNoInteractions(mockBeanModel);
  }

  @Test
  public void constructNewSortedSetPropertyWithNullBeanModel() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new SortedSetProperty(null, mockPropertyDescriptor(SortedSet.class)))
      .withMessage("BeanModel is required")
      .withNoCause();
  }

  @Test
  public void constructNewSortedSetPropertyWithNullPropertyDescriptor() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new SortedSetProperty(mock(BeanModel.class), null))
      .withMessage("PropertyDescriptor is required")
      .withNoCause();
  }

  @Test
  public void constructNewSortedSetPropertyWithNonSortedSetPropertyDescriptor() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new SortedSetProperty(mock(BeanModel.class), mockPropertyDescriptor(List.class)))
      .withMessageMatching("Property \\[.*\\] must be a SortedSet")
      .withNoCause();
  }

  @Test
  public void assertNegativeSetIndexIsNotValid() {

    assertThatExceptionOfType(IndexOutOfBoundsException.class)
      .isThrownBy(() -> newSortedSetProperty().assertSetIndex(Collections.singleton("test"), -1))
      .withMessage("[-1] is not a valid index in Set with size [1]")
      .withNoCause();
  }

  @Test
  public void assertOutOfBoundsSetIndexIsNotValid() {

    assertThatExceptionOfType(IndexOutOfBoundsException.class)
      .isThrownBy(() -> newSortedSetProperty().assertSetIndex(Collections.singleton(2), 1))
      .withMessage("[1] is not a valid index in Set with size [1]")
      .withNoCause();
  }

  @Test
  public void assertSetIndexIsValid() {

    Set<?> set = CollectionUtils.asSet(1, 2, 3);

    assertThat(newSortedSetProperty().assertSetIndex(set, 1)).isOne();
  }
}
