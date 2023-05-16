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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.beans.PropertyDescriptor;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentMap;
import java.util.function.BiFunction;

import org.cp.elements.beans.model.BeanModel;
import org.cp.elements.beans.model.Property;
import org.cp.elements.beans.model.support.AbstractIndexedProperty.IndexedValue;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.security.model.User;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link AbstractIndexedProperty}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.beans.model.Property
 * @see org.cp.elements.beans.model.support.AbstractIndexedProperty
 * @since 1.0.0
 */
public class AbstractIndexedPropertyUnitTests {

  private void testIsIndexedProperty(Class<?> propertyType, boolean indexed) {

    Property mockProperty = mock(Property.class);

    doReturn(propertyType).when(mockProperty).getType();

    assertThat(AbstractIndexedProperty.isIndexed(mockProperty)).isEqualTo(indexed);

    verify(mockProperty, times(1)).getType();
    verifyNoMoreInteractions(mockProperty);

  }

  private void testIsIndexedPropertyDescriptor(Class<?> propertyType, boolean indexed) {

    PropertyDescriptor mockPropertyDescriptor = mock(PropertyDescriptor.class);

    doReturn(propertyType).when(mockPropertyDescriptor).getPropertyType();

    assertThat(AbstractIndexedProperty.isIndexed(mockPropertyDescriptor)).isEqualTo(indexed);

    verify(mockPropertyDescriptor, times(1)).getPropertyType();
    verifyNoMoreInteractions(mockPropertyDescriptor);

  }

  private void testPropertyIsIndexed(Class<?> propertyType) {
    testIsIndexedProperty(propertyType, true);
  }

  private void testPropertyIsNotIndexed(Class<?> propertyType) {
    testIsIndexedProperty(propertyType, false);
  }

  private void testPropertyDescriptorIsIndexed(Class<?> propertyType) {
    testIsIndexedPropertyDescriptor(propertyType, true);
  }

  private void testPropertyDescriptorIsNotIndexed(Class<?> propertyType) {
    testIsIndexedPropertyDescriptor(propertyType, false);
  }

  @Test
  public void arrayPropertyIsIndexed() {
    testPropertyIsIndexed(User[].class);
  }

  @Test
  public void listPropertyIsIndexed() {
    testPropertyIsIndexed(ArrayList.class);
  }

  @Test
  public void mapPropertyIsIndexed() {
    testPropertyIsIndexed(ConcurrentMap.class);
  }

  @Test
  public void setPropertyIsIndexed() {
    testPropertyIsIndexed(TreeSet.class);
  }

  @Test
  public void booleanPropertyIsNotIndexed() {
    testPropertyIsNotIndexed(Boolean.class);
  }

  @Test
  public void dateTimePropertyIsNotIndexed() {
    testPropertyIsNotIndexed(LocalDateTime.class);
  }

  @Test
  public void intPropertyIsNotIndexed() {
    testPropertyIsNotIndexed(Integer.TYPE);
  }

  @Test
  public void integerPropertyIsNotIndexed() {
    testPropertyIsNotIndexed(Integer.class);
  }

  @Test
  public void objectPropertyIsNotIndexed() {
    testPropertyIsNotIndexed(Object.class);
  }

  @Test
  public void setPropertyIsNotIndexed() {
    testPropertyIsNotIndexed(Set.class);
  }

  @Test
  public void stringPropertyIsNotIndexed() {
    testPropertyIsNotIndexed(String.class);
  }

  @Test
  public void arrayPropertyDescriptorIsIndexed() {
    testPropertyDescriptorIsIndexed(Object[][].class);
  }

  @Test
  public void listPropertyDescriptorIsIndexed() {
    testPropertyDescriptorIsIndexed(List.class);
  }

  @Test
  public void mapPropertyDescriptorIsIndexed() {
    testPropertyDescriptorIsIndexed(Map.class);
  }

  @Test
  public void setPropertyDescriptorIsIndexed() {
    testPropertyDescriptorIsIndexed(SortedSet.class);
  }

  @Test
  public void userPropertyDescriptorIsNotIndexed() {
    testPropertyDescriptorIsNotIndexed(User.class);
  }

  @Test
  public void nullSafeTypeWithNonNullType() {
    assertThat(AbstractIndexedProperty.nullSafeType(User.class)).isEqualTo(User.class);
  }

  @Test
  public void nullSafeTypeWithNullType() {
    assertThat(AbstractIndexedProperty.nullSafeType(null)).isEqualTo(Object.class);
  }

  @Test
  public void constructAbstractIndexedProperty() {

    BeanModel mockBeanModel = mock(BeanModel.class);

    PropertyDescriptor mockPropertyDescriptor = mock(PropertyDescriptor.class);

    AbstractIndexedProperty<?> property = new TestIndexedProperty(mockBeanModel, mockPropertyDescriptor);

    assertThat(property).isNotNull();
    assertThat(property.getBeanModel()).isSameAs(mockBeanModel);
    assertThat(property.getDescriptor()).isSameAs(mockPropertyDescriptor);

    verifyNoInteractions(mockBeanModel, mockPropertyDescriptor);
  }

  @Test
  public void constructAbstractIndexedPropertyWithNullBeanModel() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new TestIndexedProperty(null, mock(PropertyDescriptor.class)))
      .withMessage("BeanModel is required")
      .withNoCause();
  }

  @Test
  public void constructAbstractIndexedPropertyWithNullPropertyDescriptor() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new TestIndexedProperty(mock(BeanModel.class), null))
      .withMessage("PropertyDescriptor is required")
      .withNoCause();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void getValueDelegatesToValueAccessorFunction() {

    BiFunction<Object, Integer, Object> mockFunction = mock(BiFunction.class);

    AbstractIndexedProperty<Integer> property = mock(AbstractIndexedProperty.class);

    doCallRealMethod().when(property).getValue(anyInt());
    doReturn(Collections.emptyList()).when(property).getValue();
    doReturn(mockFunction).when(property).getValueAccessorFunction();
    doReturn("mock").when(mockFunction).apply(any(), anyInt());

    assertThat(property.getValue(2)).isEqualTo("mock");

    verify(property, times(1)).getValue(eq(2));
    verify(property, times(1)).getValue();
    verify(property, times(1)).getValueAccessorFunction();
    verify(mockFunction, times(1)).apply(isA(List.class), eq(2));
    verifyNoMoreInteractions(mockFunction, property);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void setValueDelegatesToValueMutatorFunction() {

    BiFunction<Object, IndexedValue<Integer, Object>, Object> mockFunction = mock(BiFunction.class);

    AbstractIndexedProperty<Integer> property = mock(AbstractIndexedProperty.class);

    doCallRealMethod().when(property).setValue(anyInt(), any());
    doReturn(Collections.emptyList()).when(property).getValue();
    doReturn(mockFunction).when(property).getValueMutatorFunction();

    doAnswer(invocation -> {

      List<Object> list = invocation.getArgument(0);
      IndexedValue<Integer, Object> indexedValue = invocation.getArgument(1);

      assertThat(list).isNotNull();
      assertThat(indexedValue).isNotNull();
      assertThat(indexedValue.getIndex()).isEqualTo(4);
      assertThat(indexedValue.getValue(null)).isEqualTo("mock");

      return "test";

    }).when(mockFunction).apply(any(), any());

    assertThat(property.setValue(4, "mock")).isEqualTo("test");

    verify(property, times(1)).setValue(eq(4), eq("mock"));
    verify(property, times(1)).getValue();
    verify(property, times(1)).getValueMutatorFunction();
    verify(mockFunction, times(1)).apply(isA(List.class), isA(IndexedValue.class));
    verifyNoMoreInteractions(mockFunction, property);
  }

  @Test
  public void newIndexedValueAtIndex() {

    IndexedValue<Integer, Object> indexedValue = IndexedValue.at(2);

    assertThat(indexedValue).isNotNull();
    assertThat(indexedValue.getIndex()).isEqualTo(2);
    assertThat(indexedValue.getKey()).isEqualTo(indexedValue.getIndex());
    assertThat(indexedValue.getValue()).isNotPresent();
    assertThat(indexedValue.isSet()).isFalse();
  }

  @Test
  public void newIndexedValueFromIndexAndValue() {

    IndexedValue<Integer, Object> indexedValue = IndexedValue.from(4, "mock");

    assertThat(indexedValue).isNotNull();
    assertThat(indexedValue.getIndex()).isEqualTo(4);
    assertThat(indexedValue.getKey()).isEqualTo(indexedValue.getIndex());
    assertThat(indexedValue.getValue(null)).isEqualTo("mock");
    assertThat(indexedValue.isSet()).isTrue();
  }

  @Test
  public void newIndexedValueWithNoIndex() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> IndexedValue.from(null, "test"))
      .withMessage("Index is required")
      .withNoCause();
  }

  @Test
  public void getKeyReturnsGetIndex() {

    IndexedValue<Integer, Object> indexedValue = spy(IndexedValue.at(8));

    assertThat(indexedValue).isNotNull();
    assertThat(indexedValue.getKey()).isEqualTo(8);

    verify(indexedValue, times(1)).getKey();
    verify(indexedValue, times(1)).getIndex();
    verifyNoMoreInteractions(indexedValue);
  }

  @Test
  public void indexedValueWithValue() {

    IndexedValue<Integer, Object> indexedValue = IndexedValue.at(2).with("mock");

    assertThat(indexedValue).isNotNull();
    assertThat(indexedValue.getIndex()).isEqualTo(2);
    assertThat(indexedValue.getValue(null)).isEqualTo("mock");

    assertThat(indexedValue.with(null)).isSameAs(indexedValue);
    assertThat(indexedValue.getIndex()).isEqualTo(2);
    assertThat(indexedValue.getValue(null)).isNull();

    assertThat(indexedValue.with("test")).isSameAs(indexedValue);
    assertThat(indexedValue.getIndex()).isEqualTo(2);
    assertThat(indexedValue.getValue(null)).isEqualTo("test");
  }

  @Test
  @SuppressWarnings("all")
  public void indexedValueEqualsSelf() {

    IndexedValue<Integer, Object> indexedValue = IndexedValue.at(2).with("mock");

    assertThat(indexedValue.equals(indexedValue)).isTrue();
  }

  @Test
  public void indexedValuesAreEqual() {

    IndexedValue<Integer, Object> indexedValueOne = IndexedValue.at(2).with("mock");
    IndexedValue<Integer, Object> indexedValueTwo = IndexedValue.at(2).with("mock");

    assertThat(indexedValueOne).isEqualTo(indexedValueTwo);
  }

  @Test
  public void indexedValuesAreNotEqual() {

    IndexedValue<Integer, Object> indexedValueOne = IndexedValue.at(2).with("mock");
    IndexedValue<Long, Object> indexedValueTwo = IndexedValue.at(2L).with("mock");
    IndexedValue<String, Object> indexedValueThree = IndexedValue.at("2").with("mock");
    IndexedValue<Integer, Object> indexedValueFour = IndexedValue.at(22).with("mock");
    IndexedValue<Integer, Object> indexedValueFive = IndexedValue.at(2).with("test");
    IndexedValue<Object, Object> indexedValueSix = IndexedValue.<Object, Object>at("myKey").with(LocalDateTime.now());

    assertThat(indexedValueOne).isNotEqualTo(indexedValueTwo);
    assertThat(indexedValueOne).isNotEqualTo(indexedValueThree);
    assertThat(indexedValueOne).isNotEqualTo(indexedValueFour);
    assertThat(indexedValueOne).isNotEqualTo(indexedValueFive);
    assertThat(indexedValueOne).isNotEqualTo(indexedValueSix);
  }

  @Test
  @SuppressWarnings("all")
  public void indexedValueIsNotEqualToNull() {
    assertThat(IndexedValue.at(2).with("mock").equals(null)).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void indexedValueIsNotEqualToObject() {
    assertThat(IndexedValue.at(2).with("mock").equals(new Object())).isFalse();
  }

  @Test
  public void indexedValueHashCodeIsCorrect() {

    IndexedValue<Integer, Object> indexedValue = IndexedValue.at(2).with("mock");

    assertThat(indexedValue).isNotNull();
    assertThat(indexedValue.hashCode()).isNotZero();
    assertThat(indexedValue.hashCode()).isEqualTo(indexedValue.hashCode());
  }

  @Test
  public void indexedValueToString() {
    assertThat(IndexedValue.at(2).with("mock").toString()).isEqualTo("Value [mock] at Index [2]");
  }

  static final class TestIndexedProperty extends AbstractIndexedProperty<Integer> {

    TestIndexedProperty(@NotNull BeanModel beanModel, @NotNull PropertyDescriptor propertyDescriptor) {
      super(beanModel, propertyDescriptor);
    }

    @Override
    protected BiFunction<Object, Integer, Object> getValueAccessorFunction() {
      throw new UnsupportedOperationException("Not Implemented");
    }

    @Override
    protected BiFunction<Object, IndexedValue<Integer, Object>, Object> getValueMutatorFunction() {
      throw new UnsupportedOperationException("Not Implemented");
    }
  }
}
