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
import java.util.Map;
import java.util.function.BiFunction;

import org.cp.elements.beans.model.BeanModel;
import org.cp.elements.beans.model.BeanUtils;
import org.cp.elements.beans.model.Property;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Abstract Data Type (ADT) and extension of {@link Property} used to model a {@link Map Map-typed} bean property.
 *
 * @author John Blum
 * @see java.beans.PropertyDescriptor
 * @see java.util.Map
 * @see org.cp.elements.beans.model.BeanModel
 * @see org.cp.elements.beans.model.Property
 * @see org.cp.elements.beans.model.support.AbstractIndexedProperty
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class MapProperty extends AbstractIndexedProperty<Object> {

  /**
   * Asserts that the given {@link PropertyDescriptor} describes a {@link Map Map-typed} bean property.
   *
   * @param propertyDescriptor {@link PropertyDescriptor} describing the bean property to evaluate.
   * @return the given {@link PropertyDescriptor}.
   * @throws IllegalArgumentException if the {@link PropertyDescriptor} is {@literal null}
   * or the {@link PropertyDescriptor} does not describe a {@link Map Map-typed} bean property.
   * @see #isMapType(PropertyDescriptor)
   * @see java.beans.PropertyDescriptor
   */
  static @NotNull PropertyDescriptor assertMapType(@Nullable PropertyDescriptor propertyDescriptor) {

    Assert.notNull(propertyDescriptor, "PropertyDescriptor is required");

    Assert.argument(propertyDescriptor, MapProperty::isMapType,
      "Property [%s] must be a Map", propertyDescriptor);

    return propertyDescriptor;
  }

  /**
   * Determines whether the given {@link PropertyDescriptor} describes a {@link Map Map-typed} bean property.
   *
   * @param propertyDescriptor {@link PropertyDescriptor} describing the bean property.
   * @return a boolean value indicating whether the given {@link PropertyDescriptor}
   * describes a {@link Map Map-typed} bean property.
   * @see java.beans.PropertyDescriptor
   */
  @NullSafe
  public static boolean isMapType(@Nullable PropertyDescriptor propertyDescriptor) {
    return IS_MAP.test(BeanUtils.resolveType(propertyDescriptor));
  }

  /**
   * Factory method used to construct a new instance of {@link MapProperty} initialized with the given,
   * required {@link BeanModel} modeling the bean containing the property along with the given, required
   * {@link PropertyDescriptor} describing the {@link Map Map-typed} bean property.
   *
   * @param beanModel {@link BeanModel} modeling the bean containing the property; must not be {@literal null}.
   * @param propertyDescriptor {@link PropertyDescriptor} describing the bean property; must not be {@literal null}.
   * @return a new {@link MapProperty}.
   * @throws IllegalArgumentException if the {@link BeanModel} or {@link PropertyDescriptor} are {@literal null}
   * or the {@link PropertyDescriptor} does not describe a {@link Map Map-typed} bean property.
   * @see #MapProperty(BeanModel, PropertyDescriptor)
   * @see org.cp.elements.beans.model.BeanModel
   * @see java.beans.PropertyDescriptor
   */
  public static @NotNull MapProperty from(@NotNull BeanModel beanModel,
      @NotNull PropertyDescriptor propertyDescriptor) {

    return new MapProperty(beanModel, propertyDescriptor);
  }

  /**
   * Factory method used to construct a new instance of {@link MapProperty} initialized with the given, required
   * {@link Property} modeling the {@link Map Map-typed} bean property.
   *
   * @param property {@link Property} of the bean.
   * @return a new {@link MapProperty}
   * @throws IllegalArgumentException if {@link Property} is {@literal null} or the {@link Property} does not model
   * a {@link Map Map-typed} bean property.
   * @see org.cp.elements.beans.model.Property
   * @see #from(BeanModel, PropertyDescriptor)
   */
  public static @NotNull MapProperty from(@NotNull Property property) {

    Assert.notNull(property, "Property is required");

    return from(property.getBeanModel(), property.getDescriptor());
  }

  /**
   * Constructs a new instance of {@link MapProperty} initialized with the given, required {@link BeanModel}
   * modeling the bean containing the property along with the given, required {@link PropertyDescriptor}
   * describing the {@link Map Map-typed} bean property.
   *
   * @param beanModel {@link BeanModel} modeling the bean containing the property; must not be {@literal null}.
   * @param propertyDescriptor {@link PropertyDescriptor} describing the bean property; must not be {@literal null}.
   * @throws IllegalArgumentException if the {@link BeanModel} or {@link PropertyDescriptor} are {@literal null}
   * or the {@link PropertyDescriptor} does not describe a {@link Map Map-typed} bean property.
   * @see org.cp.elements.beans.model.BeanModel
   * @see java.beans.PropertyDescriptor
   */
  protected MapProperty(@NotNull BeanModel beanModel, @NotNull PropertyDescriptor propertyDescriptor) {
    super(beanModel, assertMapType(propertyDescriptor));
  }

  @Override
  protected BiFunction<Object, Object, Object> getValueAccessorFunction() {
    return (map, index) -> ((Map<?, ?>) map).get(index);
  }

  @Override
  @SuppressWarnings("unchecked")
  protected BiFunction<Object, IndexedValue<Object, Object>, Object> getValueMutatorFunction() {
    return (map, indexedValue) -> ((Map<Object, Object>) map)
      .put(indexedValue.getIndex(), indexedValue.getValue(null));
  }
}
