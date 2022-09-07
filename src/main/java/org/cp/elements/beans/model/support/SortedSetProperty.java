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
import java.util.Set;
import java.util.SortedSet;
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
 * Abstract Data Type (ADT) and extension of {@link Property} used to model a {@link SortedSet SortedSet-typed}
 * bean property.
 *
 * @author John Blum
 * @see java.beans.PropertyDescriptor
 * @see java.util.Set
 * @see java.util.SortedSet
 * @see org.cp.elements.beans.model.BeanModel
 * @see org.cp.elements.beans.model.Property
 * @see org.cp.elements.beans.model.support.AbstractIndexedProperty
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class SortedSetProperty extends AbstractIndexedProperty<Integer> {

  /**
   * Asserts that the {@link PropertyDescriptor} describes a {@link SortedSet SortedSet-based} bean property.
   *
   * @param propertyDescriptor {@link PropertyDescriptor} to assert.
   * @return the given {@link PropertyDescriptor}.
   * @throws IllegalArgumentException if the {@link PropertyDescriptor} is {@literal null}
   * or the {@link PropertyDescriptor} does not describe a {@link SortedSet SortedSet-based} bean property.
   * @see #isSortedSetType(PropertyDescriptor)
   * @see java.beans.PropertyDescriptor
   * @see java.util.SortedSet
   */
  static @NotNull PropertyDescriptor assertSortedSetType(@Nullable PropertyDescriptor propertyDescriptor) {

    Assert.notNull(propertyDescriptor, "PropertyDescriptor is required");

    Assert.argument(propertyDescriptor, SortedSetProperty::isSortedSetType,
      "Property [%s] must be a SortedSet", propertyDescriptor);

    return propertyDescriptor;
  }

  /**
   * Determines whether the given {@link PropertyDescriptor} describes a {@link SortedSet SortedSet-based} bean property.
   *
   * @param propertyDescriptor {@link PropertyDescriptor} to evaluate.
   * @return a boolean value indicating whether the given {@link PropertyDescriptor}
   * describes a {@link SortedSet SortedSet-based} bean property.
   * @see java.beans.PropertyDescriptor
   * @see java.util.SortedSet
   */
  @NullSafe
  public static boolean isSortedSetType(@Nullable PropertyDescriptor propertyDescriptor) {
    return IS_SORTED_SET.test(BeanUtils.resolveType(propertyDescriptor));
  }

  /**
   * Factory method used to construct a new instance of {@link SortedSetProperty} initialized with the given, required
   * {@link BeanModel} modeling the bean containing the {@link SortedSet} property along with a given, required
   * {@link PropertyDescriptor} describing the {@link SortedSet} property.
   *
   * @param beanModel {@link BeanModel} modeling the bean containing the property; must not be {@literal null}.
   * @param propertyDescriptor {@link PropertyDescriptor} describing the property; must not be {@literal null}.
   * @return a new {@link SortedSetProperty} initialized with the given {@link BeanModel} and {@link PropertyDescriptor}.
   * @throws IllegalArgumentException if the {@link BeanModel} or {@link PropertyDescriptor} are {@literal null}
   * or the {@link PropertyDescriptor} does not describe a {@link SortedSet} bean property.
   * @see #SortedSetProperty(BeanModel, PropertyDescriptor)
   * @see java.beans.PropertyDescriptor
   * @see java.util.SortedSet
   */
  public static @NotNull SortedSetProperty from(@NotNull BeanModel beanModel, @NotNull PropertyDescriptor propertyDescriptor) {
    return new SortedSetProperty(beanModel, propertyDescriptor);
  }

  /**
   * Factory method used to construct a new instance of {@link SortedSetProperty} initialized with the given, required
   * {@link BeanModel} modeling the bean containing the {@link SortedSet} property along with a given, required
   * {@link PropertyDescriptor} describing the {@link SortedSet} property.
   *
   * @param property {@link Property} modeling the {@link SortedSet} bean property; must not be {@literal null}.
   * @return a new {@link SortedSetProperty} initialized with the given {@link Property}.
   * @throws IllegalArgumentException if the {@link Property} is {@literal null} or the {@link Property}
   * does not model a {@link SortedSet} bean property.
   * @see org.cp.elements.beans.model.Property
   * @see #from(BeanModel, PropertyDescriptor)
   * @see java.util.SortedSet
   */
  public static @NotNull SortedSetProperty from(@NotNull Property property) {

    Assert.notNull(property, "Property is required");

    return from(property.getBeanModel(), property.getDescriptor());
  }

  /**
   * Constructs a new instance of {@link SortedSetProperty} initialized with the given, required {@link BeanModel}
   * modeling the bean containing the {@link SortedSet} property along with a given, required {@link PropertyDescriptor}
   * describing the {@link SortedSet} property.
   *
   * @param beanModel {@link BeanModel} modeling the bean containing the property; must not be {@literal null}.
   * @param propertyDescriptor {@link PropertyDescriptor} describing the property; must not be {@literal null}.
   * @throws IllegalArgumentException if the {@link BeanModel} or {@link PropertyDescriptor} are {@literal null}
   * or the {@link PropertyDescriptor} does not describe a {@link SortedSet} bean property.
   * @see #assertSortedSetType(PropertyDescriptor)
   * @see java.beans.PropertyDescriptor
   * @see java.util.SortedSet
   */
  protected SortedSetProperty(@NotNull BeanModel beanModel, @NotNull PropertyDescriptor propertyDescriptor) {
    super(beanModel, assertSortedSetType(propertyDescriptor));
  }

  @Override
  protected BiFunction<Object, Integer, Object> getValueAccessorFunction() {
    return (set, index) -> getElementAt((SortedSet<?>) set, index);
  }

  @Override
  @SuppressWarnings("unchecked")
  protected BiFunction<Object, IndexedValue<Integer, Object>, Object> getValueMutatorFunction() {

    return (set, indexedValue) -> {
      SortedSet<Object> sortedSet = (SortedSet<Object>) set;
      Object element = getElementAt(sortedSet, indexedValue.getIndex());
      sortedSet.remove(element);
      return sortedSet.add(indexedValue.getValue(null));
    };
  }

  /**
   * Asserts the given {@link Integer#TYPE index} is valid for the given {@link Set}.
   *
   * For any {@link Set} the index must simply be greater than equal to {@literal 0}
   * and less than the {@link Set#size()}.
   *
   * @param set {@link Set} against which the index is validated.
   * @param index {@link Integer#TYPE} specifying the index in the {@link Set}.
   * @return the given {@link Integer#TYPE index}.
   * @throws IndexOutOfBoundsException if the {@link Integer#TYPE index} is not valid.
   * @see java.util.Set
   */
  protected int assertSetIndex(@Nullable Set<?> set, int index) {

    int setSize = CollectionUtils.nullSafeSize(set);

    if (index < 0 || index >= setSize) {
      throw newIndexOutOfBoundsException("[%d] is not a valid index in Set with size [%d]", index, setSize);
    }

    return index;
  }

  /**
   * Gets the {@link Object} at the given {@link Integer#TYPE index} from the given {@link SortedSet}.
   *
   * @param set {@link SortedSet} from which to get the {@link Object value} at the given {@link Integer#TYPE index}.
   * @param index {@link Integer#TYPE} referring to the {@literal index} in the {@link SortedSet} from which to get
   * the {@link Object value}.
   * @return the {@link Object value} at the given {@link Integer#TYPE index} from the given {@link SortedSet}.
   * @throws IndexOutOfBoundsException if the {@link Integer#TYPE index} is not valid.
   * @see #assertSetIndex(Set, int)
   * @see java.util.SortedSet
   */
  protected @Nullable Object getElementAt(@NotNull SortedSet<?> set, int index) {

    assertSetIndex(set, index);

    Object result = null;
    int currentIndex = 0;

    for (Object element : set) {

      if (currentIndex == index) {
        result = element;
        break;
      }

      currentIndex++;
    }

    return result;
  }
}
