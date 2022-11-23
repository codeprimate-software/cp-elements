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

import java.beans.PropertyDescriptor;
import java.lang.reflect.Array;
import java.util.function.BiFunction;

import org.cp.elements.beans.model.BeanModel;
import org.cp.elements.beans.model.BeanUtils;
import org.cp.elements.beans.model.Property;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Abstract Data Type (ADT) and extension of {@link Property} used to model an array-typed bean property.
 *
 * @author John Blum
 * @see java.beans.PropertyDescriptor
 * @see java.lang.reflect.Array
 * @see org.cp.elements.beans.model.BeanModel
 * @see org.cp.elements.beans.model.Property
 * @see org.cp.elements.beans.model.support.AbstractIndexedProperty
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ArrayProperty extends AbstractIndexedProperty<Integer> {

  /**
   * Asserts that the {@link PropertyDescriptor} describes an array-typed, bean property.
   *
   * @param propertyDescriptor {@link PropertyDescriptor} to assert.
   * @return the given {@link PropertyDescriptor}.
   * @see #isArrayType(PropertyDescriptor)
   * @see java.beans.PropertyDescriptor
   */
  static @NotNull PropertyDescriptor assertArrayType(@NotNull PropertyDescriptor propertyDescriptor) {

    Assert.notNull(propertyDescriptor, "PropertyDescriptor is required");

    Assert.argument(propertyDescriptor, ArrayProperty::isArrayType,
      "Property [%s] must be an array", propertyDescriptor);

    return propertyDescriptor;
  }

  /**
   * Determines whether the given {@link PropertyDescriptor} describes an array-typed, bean property.
   *
   * @param propertyDescriptor {@link PropertyDescriptor} to evaluate.
   * @return a boolean value indicating whether the given {@link PropertyDescriptor}
   * describes an array-typed, bean property.
   * @see java.beans.PropertyDescriptor
   */
  @NullSafe
  public static boolean isArrayType(@Nullable PropertyDescriptor propertyDescriptor) {
    return IS_ARRAY.test(BeanUtils.resolveType(propertyDescriptor));
  }

  /**
   * Factory method used to construct a new instance of {@link ArrayProperty} initialized with the given,
   * required {@link BeanModel} modeling the bean containing this array property along with the given,
   * required {@link PropertyDescriptor} describing this array property.
   *
   * @param beanModel {@link BeanModel} modeling the bean containing this property; must not be {@literal null}.
   * @param propertyDescriptor {@link PropertyDescriptor} describing the property; must not be {@literal null}.
   * @return a new {@link ArrayProperty} initialized with the given {@link BeanModel} and {@link PropertyDescriptor}.
   * @throws IllegalArgumentException if the {@link BeanModel} or {@link PropertyDescriptor} are {@literal null},
   * or the {@link PropertyDescriptor} describing the bean property is not an array.
   * @see #ArrayProperty(BeanModel, PropertyDescriptor)
   * @see org.cp.elements.beans.model.BeanModel
   * @see java.beans.PropertyDescriptor
   */
  public static @NotNull ArrayProperty from(@NotNull BeanModel beanModel, @NotNull PropertyDescriptor propertyDescriptor) {
    return new ArrayProperty(beanModel, propertyDescriptor);
  }

  /**
   * Factory method used to construct a new instance of {@link ArrayProperty} initialized with the given,
   * required {@link Property} modeling the array-typed, bean property.
   *
   * @param property {@link Property} modeling the array-typed, bean property; must not be {@literal null}.
   * @return a new {@link ArrayProperty} initialized with the given {@link Property}.
   * @throws IllegalArgumentException if the {@link Property} is {@literal null}, or the bean {@link Property}
   * is not an array.
   * @see org.cp.elements.beans.model.Property
   * @see #from(BeanModel, PropertyDescriptor)
   */
  public static @NotNull ArrayProperty from(@NotNull Property property) {

    Assert.notNull(property, "Property is required");

    return from(property.getBeanModel(), property.getDescriptor());
  }

  /**
   * Constructs a new instance of {@link ArrayProperty} initialized with the given, required {@link BeanModel}
   * modeling the bean containing this array property along with the given, required {@link PropertyDescriptor}
   * describing this array property.
   *
   * @param beanModel {@link BeanModel} modeling the bean containing this property; must not be {@literal null}.
   * @param propertyDescriptor {@link PropertyDescriptor} describing this property; must not be {@literal null}.
   * @throws IllegalArgumentException if the {@link BeanModel} or {@link PropertyDescriptor} are {@literal null},
   * or the {@link PropertyDescriptor} describing the bean property is not an array.
   * @see org.cp.elements.beans.model.BeanModel
   * @see java.beans.PropertyDescriptor
   */
  protected ArrayProperty(@NotNull BeanModel beanModel, @NotNull PropertyDescriptor propertyDescriptor) {
    super(beanModel, assertArrayType(propertyDescriptor));
  }

  /**
   * Asserts that the given array index is valid.
   *
   * @param array {@link Object} array to used in the assertion.
   * @param index {@link Integer#TYPE} to evaluate as an array index.
   * @return the given array index if valid.
   * @throws ArrayIndexOutOfBoundsException if the array index is less than {@literal 0}
   * or greater than equal to {@literal array.length}.
   * @throws IllegalArgumentException if the {@code array} argument is not an actual array.
   */
  protected int assertArrayIndex(@NotNull Object array, int index) {

    int arrayLength = Array.getLength(array);

    if (index < 0 || index >= arrayLength) {
      String message = "Array index [%d] must be greater than equal to 0 and less than [%d]";
      throw new ArrayIndexOutOfBoundsException(String.format(message, index, arrayLength));
    }

    return index;
  }

  @Override
  protected BiFunction<Object, Integer, Object> getValueAccessorFunction() {
    return (array, index) -> Array.get(array, assertArrayIndex(array, index));
  }

  @Override
  protected BiFunction<Object, IndexedValue<Integer, Object>, Object> getValueMutatorFunction() {

    return (array, indexedValue) -> {
      int index = assertArrayIndex(array, indexedValue.getIndex());
      Object oldValue = Array.get(array, index);
      Object newValue = indexedValue.getValue(null);
      Array.set(array, index, newValue);
      return oldValue;
    };
  }
}
