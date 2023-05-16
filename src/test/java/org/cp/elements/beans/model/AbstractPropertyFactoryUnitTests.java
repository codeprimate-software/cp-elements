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
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.beans.PropertyDescriptor;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.function.Consumer;

import org.cp.elements.beans.model.support.ArrayProperty;
import org.cp.elements.beans.model.support.ListProperty;
import org.cp.elements.beans.model.support.MapProperty;
import org.cp.elements.beans.model.support.SortedSetProperty;
import org.cp.elements.security.model.User;
import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link AbstractPropertyFactory}.
 *
 * @author John Blum
 * @see java.beans.PropertyDescriptor
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.beans.model.AbstractPropertyFactory
 * @since 1.0.0
 */
public class AbstractPropertyFactoryUnitTests {

  private void testPropertyType(Class<?> propertyType, Consumer<Property> propertyAssertion) {

    BeanModel mockBeanModel = mock(BeanModel.class);

    PropertyDescriptor mockPropertyDescriptor = mock(PropertyDescriptor.class);

    doReturn(propertyType).when(mockPropertyDescriptor).getPropertyType();

    Property property = AbstractPropertyFactory.create(mockBeanModel, mockPropertyDescriptor);

    propertyAssertion.accept(property);

    verify(mockPropertyDescriptor, atLeast(2)).getPropertyType();
    verifyNoMoreInteractions(mockPropertyDescriptor);
    verifyNoInteractions(mockBeanModel);
  }

  private void testPropertyTypeIsEqualTo(Class<?> propertyType, Class<?> propertyInstanceType) {
    testPropertyType(propertyType, property -> assertThat(property.getClass()).isEqualTo(propertyInstanceType));
  }

  private void testPropertyTypeIsInstanceOf(Class<?> propertyType, Class<?> propertyInstanceType) {
    testPropertyType(propertyType, property -> assertThat(property).isInstanceOf(propertyInstanceType));
  }

  @Test
  public void constructPropertyForBooleanIsCorrect() {
    testPropertyTypeIsEqualTo(Boolean.class, Property.class);
  }

  @Test
  public void constructPropertyForIntIsCorrect() {
    testPropertyTypeIsEqualTo(Integer.TYPE, Property.class);
  }

  @Test
  public void constructPropertyForIntegerIsCorrect() {
    testPropertyTypeIsEqualTo(Integer.class, Property.class);
  }

  @Test
  public void constructPropertyForObjectIsCorrect() {
    testPropertyTypeIsEqualTo(Set.class, Property.class);
  }

  @Test
  public void constructPropertyForSetIsCorrect() {
    testPropertyTypeIsEqualTo(Set.class, Property.class);
  }

  @Test
  public void constructPropertyForStringIsCorrect() {
    testPropertyTypeIsEqualTo(String.class, Property.class);
  }

  @Test
  public void constructPropertyForUserIsCorrect() {
    testPropertyTypeIsEqualTo(User.class, Property.class);
  }

  @Test
  public void constructArrayPropertyIsCorrect() {
    testPropertyTypeIsInstanceOf(User[].class, ArrayProperty.class);
  }

  @Test
  public void constructListPropertyIsCorrect() {
    testPropertyTypeIsInstanceOf(List.class, ListProperty.class);
  }

  @Test
  public void constructMapPropertyIsCorrect() {
    testPropertyTypeIsInstanceOf(Map.class, MapProperty.class);
  }

  @Test
  public void constructSortedSetPropertyIsCorrect() {
    testPropertyTypeIsInstanceOf(SortedSet.class, SortedSetProperty.class);
  }

  @Test
  public void constructStandardPropertyIsCorrect() {
    testPropertyTypeIsEqualTo(User.class, Property.class);
  }
}
