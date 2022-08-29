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
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.beans.PropertyDescriptor;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.cp.elements.beans.model.BeanModel;
import org.cp.elements.beans.model.Property;
import org.cp.elements.data.struct.KeyValue;
import org.junit.Test;

/**
 * Unit Tests for {@link MapProperty}.
 *
 * @author John Blum
 * @see java.beans.PropertyDescriptor
 * @see java.util.Map
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.beans.model.support.MapProperty
 * @since 1.0.0
 */
public class MapPropertyUnitTests {

  private PropertyDescriptor mockPropertyDescriptor(Class<?> propertyType) {

    PropertyDescriptor mockPropertyDescriptor = mock(PropertyDescriptor.class);

    doReturn(propertyType).when(mockPropertyDescriptor).getPropertyType();

    return mockPropertyDescriptor;
  }

  @Test
  public void assertMapTypeWithMapTypedPropertyDescriptor() {

    PropertyDescriptor mockPropertyDescriptor = mockPropertyDescriptor(Map.class);

    assertThat(MapProperty.assertMapType(mockPropertyDescriptor)).isSameAs(mockPropertyDescriptor);

    verify(mockPropertyDescriptor, times(1)).getPropertyType();
    verifyNoMoreInteractions(mockPropertyDescriptor);
  }

  @Test
  public void assertMapTypeWithNonMapTypedPropertyDescriptor() {

    PropertyDescriptor mockPropertyDescriptor = mockPropertyDescriptor(KeyValue.class);

    assertThatIllegalArgumentException()
      .isThrownBy(() -> MapProperty.assertMapType(mockPropertyDescriptor))
      .withMessage("Property [%s] must be a Map", mockPropertyDescriptor)
      .withNoCause();

    verify(mockPropertyDescriptor, times(1)).getPropertyType();
    verifyNoMoreInteractions(mockPropertyDescriptor);
  }

  @Test
  public void assertMapTypeWithNull() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> MapProperty.assertMapType(null))
      .withMessage("PropertyDescriptor is required")
      .withNoCause();
  }

  @Test
  public void isMapTypeWithMapTypedBeanProperty() {
    assertThat(MapProperty.isMapType(mockPropertyDescriptor(Map.class))).isTrue();
  }

  @Test
  public void isMapTypeWithTreeMapTypedBeanProperty() {
    assertThat(MapProperty.isMapType(mockPropertyDescriptor(TreeMap.class))).isTrue();
  }

  @Test
  public void isMapTypeWithSetTypedBeanProperty() {
    assertThat(MapProperty.isMapType(mockPropertyDescriptor(Set.class))).isFalse();
  }

  @Test
  public void isMapTypeWithListTypedBeanProperty() {
    assertThat(MapProperty.isMapType(mockPropertyDescriptor(List.class))).isFalse();
  }

  @Test
  public void isMapTypeWithKeyValueTypedBeanProperty() {
    assertThat(MapProperty.isMapType(mockPropertyDescriptor(KeyValue.class))).isFalse();
  }

  @Test
  public void isMapTypeWithArrayTypedBeanProperty() {
    assertThat(MapProperty.isMapType(mockPropertyDescriptor(Object[].class))).isFalse();
  }

  @Test
  public void isMapTypeWithObjectTypedBeanProperty() {
    assertThat(MapProperty.isMapType(mockPropertyDescriptor(Object.class))).isFalse();
  }

  @Test
  public void isMapTypeWithNullIsNullSafe() {
    assertThat(MapProperty.isMapType(mockPropertyDescriptor(null))).isFalse();
  }

  @Test
  public void fromBeanModelAndPropertyDescriptor() {

    BeanModel mockBeanModel = mock(BeanModel.class);

    PropertyDescriptor mockPropertyDescriptor = mockPropertyDescriptor(Map.class);

    MapProperty property = MapProperty.from(mockBeanModel, mockPropertyDescriptor);

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

    PropertyDescriptor mockPropertyDescriptor = mockPropertyDescriptor(Map.class);

    Property mockProperty = mock(Property.class);

    doReturn(mockBeanModel).when(mockProperty).getBeanModel();
    doReturn(mockPropertyDescriptor).when(mockProperty).getDescriptor();

    MapProperty mapProperty = MapProperty.from(mockProperty);

    assertThat(mapProperty).isNotNull();
    assertThat(mapProperty.getBeanModel()).isEqualTo(mockBeanModel);
    assertThat(mapProperty.getDescriptor()).isEqualTo(mockPropertyDescriptor);

    verify(mockProperty, times(1)).getBeanModel();
    verify(mockProperty, times(1)).getDescriptor();
    verify(mockPropertyDescriptor, times(1)).getPropertyType();
    verifyNoMoreInteractions(mockProperty, mockPropertyDescriptor);
    verifyNoInteractions(mockBeanModel);
  }

  @Test
  public void fromNullProperty() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> MapProperty.from(null))
      .withMessage("Property is required")
      .withNoCause();
  }

  @Test
  public void constructNewMapProperty() {

    BeanModel mockBeanModel = mock(BeanModel.class);

    PropertyDescriptor mockPropertyDescriptor = mockPropertyDescriptor(Map.class);

    MapProperty property = new MapProperty(mockBeanModel, mockPropertyDescriptor);

    assertThat(property.getBeanModel()).isEqualTo(mockBeanModel);
    assertThat(property.getDescriptor()).isEqualTo(mockPropertyDescriptor);

    verify(mockPropertyDescriptor, times(1)).getPropertyType();
    verifyNoMoreInteractions(mockPropertyDescriptor);
    verifyNoInteractions(mockBeanModel);
  }

  @Test
  public void constructNewMapPropertyWithNullBeanModel() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new MapProperty(null, mockPropertyDescriptor(Map.class)))
      .withMessage("BeanModel is required")
      .withNoCause();
  }

  @Test
  public void constructNewMapPropertyWithNullPropertyDescriptor() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new MapProperty(mock(BeanModel.class), null))
      .withMessage("PropertyDescriptor is required")
      .withNoCause();
  }

  @Test
  public void constructNewMapPropertyWithNonMapTypedPropertyDescriptor() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new MapProperty(mock(BeanModel.class), mockPropertyDescriptor(KeyValue.class)))
      .withMessageMatching("Property \\[.*] must be a Map")
      .withNoCause();
  }
}
