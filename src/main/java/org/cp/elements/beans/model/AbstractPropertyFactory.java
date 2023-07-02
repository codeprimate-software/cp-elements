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

import java.beans.PropertyDescriptor;

import org.cp.elements.beans.model.support.ArrayProperty;
import org.cp.elements.beans.model.support.ListProperty;
import org.cp.elements.beans.model.support.MapProperty;
import org.cp.elements.beans.model.support.SortedSetProperty;
import org.cp.elements.lang.annotation.NotNull;

/**
 * {@literal Abstract Factory} responsible for creating an appropriate bean {@link Property} based on
 * the {@link Property} {@link Class type}.
 *
 * @author John Blum
 * @see org.cp.elements.beans.model.Property
 * @see <a href="https://en.wikipedia.org/wiki/Abstract_factory_pattern">Abstract Factory Software Design Pattern</a>
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractPropertyFactory {

  /**
   * Factory method used to construct a new typed {@link Property} based on
   * the {@link Property Property's} {@link Class type}.
   *
   * @param beanModel {@link BeanModel} modeling the bean contain the property; must not be {@literal null}.
   * @param propertyDescriptor {@link PropertyDescriptor} describing the bean property; must not be {@literal null}.
   * @return a new, typed {@link Property} based on the {@link Property Property's} {@link Class type}.
   * @throws IllegalArgumentException if the {@link BeanModel} or {@link PropertyDescriptor} are {@literal null}.
   * @see org.cp.elements.beans.model.BeanModel
   * @see org.cp.elements.beans.model.Property
   * @see java.beans.PropertyDescriptor
   */
  public static @NotNull Property create(@NotNull BeanModel beanModel,
      @NotNull PropertyDescriptor propertyDescriptor) {

    return ArrayProperty.isArrayType(propertyDescriptor) ? ArrayProperty.from(beanModel, propertyDescriptor)
      : ListProperty.isListType(propertyDescriptor) ? ListProperty.from(beanModel, propertyDescriptor)
      : MapProperty.isMapType(propertyDescriptor) ? MapProperty.from(beanModel, propertyDescriptor)
      : SortedSetProperty.isSortedSetType(propertyDescriptor) ? SortedSetProperty.from(beanModel, propertyDescriptor)
      : Property.from(beanModel, propertyDescriptor);
  }
}
