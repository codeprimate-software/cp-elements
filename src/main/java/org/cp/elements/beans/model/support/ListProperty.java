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

import static org.cp.elements.lang.RuntimeExceptionsFactory.newIndexOutOfBoundsException;

import java.beans.PropertyDescriptor;
import java.util.List;
import java.util.function.BiFunction;

import org.cp.elements.beans.model.BeanModel;
import org.cp.elements.beans.model.BeanUtils;
import org.cp.elements.beans.model.Property;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.util.CollectionUtils;

/**
 * Abstract Data Type (ADT) and extension of {@link Property} used to model a {@link List List-typed} bean property.
 *
 * @author John Blum
 * @see java.beans.PropertyDescriptor
 * @see java.util.List
 * @see org.cp.elements.beans.model.BeanModel
 * @see org.cp.elements.beans.model.Property
 * @see org.cp.elements.beans.model.support.AbstractIndexedProperty
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ListProperty extends AbstractIndexedProperty<Integer> {

  /**
   * Asserts that the {@link PropertyDescriptor} describes a {@link List List-based} bean property.
   *
   * @param propertyDescriptor {@link PropertyDescriptor} to assert.
   * @return the given {@link PropertyDescriptor}.
   * @throws IllegalArgumentException if the {@link PropertyDescriptor} is {@literal null}
   * or the {@link PropertyDescriptor} does not describe a {@link List List-based} bean property.
   * @see #isListType(PropertyDescriptor)
   * @see java.beans.PropertyDescriptor
   */
  static @NotNull PropertyDescriptor assertListType(@Nullable PropertyDescriptor propertyDescriptor) {

    Assert.notNull(propertyDescriptor, "PropertyDescriptor is required");

    Assert.argument(propertyDescriptor, ListProperty::isListType,
      "Property [%s] must be a List", propertyDescriptor);

    return propertyDescriptor;
  }

  /**
   * Determines whether the given {@link PropertyDescriptor} describes a {@link List List-based} bean property.
   *
   * @param propertyDescriptor {@link PropertyDescriptor} to evaluate.
   * @return a boolean value indicating whether the given {@link PropertyDescriptor}
   * describes a {@link List List-based} bean property.
   * @see java.beans.PropertyDescriptor
   */
  @NullSafe
  public static boolean isListType(@Nullable PropertyDescriptor propertyDescriptor) {
    return IS_LIST.test(BeanUtils.resolveType(propertyDescriptor));
  }

  /**
   * Factory method used to construct a new instance of {@link ListProperty} initialized with
   * the given, required {@link BeanModel} modeling the bean containing the {@link List} property
   * along with the given, required {@link PropertyDescriptor} describing the {@link List} property.
   *
   * @param beanModel {@link BeanModel} modeling the bean containing the property; must not be {@literal null}.
   * @param propertyDescriptor {@link PropertyDescriptor} describing the property; must not be {@literal null}.
   * @return a new {@link ListProperty} initialized with the given {@link BeanModel} and {@link PropertyDescriptor}.
   * @throws IllegalArgumentException if the {@link BeanModel} or {@link PropertyDescriptor} are {@literal null},
   * or the {@link PropertyDescriptor} describing the bean property is not a {@link List}.
   * @see #ListProperty(BeanModel, PropertyDescriptor)
   * @see org.cp.elements.beans.model.BeanModel
   * @see java.beans.PropertyDescriptor
   */
  public static @NotNull ListProperty from(@NotNull BeanModel beanModel, @NotNull PropertyDescriptor propertyDescriptor) {
    return new ListProperty(beanModel, propertyDescriptor);
  }

  /**
   * Factory method used to construct a new instance of {@link ListProperty} initialized with
   * the given, required {@link Property} modeling the {@link List List-based} bean property.
   *
   * @param property {@link Property} modeling the {@link List List-based} bean property; must not be {@literal null}.
   * @return a new {@link ListProperty} initialized with the given {@link Property}.
   * @throws IllegalArgumentException if the {@link Property} is {@literal null}, or the bean {@link Property}
   * is not a {@link List}.
   * @see org.cp.elements.beans.model.Property
   * @see #from(BeanModel, PropertyDescriptor)
   */
  public static @NotNull ListProperty from(@NotNull Property property) {

    Assert.notNull(property, "Property is required");

    return from(property.getBeanModel(), property.getDescriptor());
  }

  /**
   * Constructs a new instance of {@link ListProperty} initialized with the given, required {@link BeanModel}
   * modeling the bean containing the property along with a given, required {@link PropertyDescriptor}
   * describing the property.
   *
   * @param beanModel {@link BeanModel} modeling the bean containing the property; must not be {@literal null}.
   * @param propertyDescriptor {@link PropertyDescriptor} describing the property; must not be {@literal null}.
   * @throws IllegalArgumentException if the {@link BeanModel} or {@link PropertyDescriptor} are {@literal null},
   * or the {@link PropertyDescriptor} describing the bean property is not a {@link List}.
   * @see org.cp.elements.beans.model.BeanModel
   * @see java.beans.PropertyDescriptor
   */
  protected ListProperty(@NotNull BeanModel beanModel, @NotNull PropertyDescriptor propertyDescriptor) {
    super(beanModel, assertListType(propertyDescriptor));
  }

  /**
   * Asserts that the given {@link List} index is valid.
   *
   * @param list {@link List} used in the assertion.
   * @param index {@link Integer#TYPE} value to evaluate as a valid {@link List} index.
   * @return the given {@link List} index if valid.
   * @throws IndexOutOfBoundsException if the {@link List} index is less than {@literal 0}
   * or greater than equal to {@link List#size()}.
   * @see java.util.List
   */
  @NullSafe
  protected int assertListIndex(@Nullable List<?> list, int index) {

    int listSize = CollectionUtils.nullSafeSize(list);

    if (index < 0 || index >= listSize) {
      String message = "List index [%d] must be greater than equal to [0] and less than [%d]";
      throw newIndexOutOfBoundsException(message, index, listSize);
    }

    return index;
  }

  @Override
  protected BiFunction<Object, Integer, Object> getValueAccessorFunction() {

    return (list, index) -> {
      List<?> resolvedList = (List<?>) list;
      return resolvedList.get(assertListIndex(resolvedList, index));
    };
  }

  @Override
  @SuppressWarnings("unchecked")
  protected BiFunction<Object, IndexedValue<Integer, Object>, Object> getValueMutatorFunction() {

    return (list, indexedValue) -> {
      List<Object> resolvedList = (List<Object>) list;
      return resolvedList.set(assertListIndex(resolvedList, indexedValue.getIndex()),
        indexedValue.getValue(null));
    };
  }
}
