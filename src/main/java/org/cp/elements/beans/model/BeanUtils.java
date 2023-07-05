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

import static org.cp.elements.lang.ElementsExceptionsFactory.newBeanIntrospectionException;

import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyDescriptor;

import org.cp.elements.beans.BeansException;
import org.cp.elements.beans.PropertyNotFoundException;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Abstract utility class for processing a JavaBean, or {@link Object POJO}.
 *
 * @author John Blum
 * @see java.beans.BeanInfo
 * @see java.beans.Introspector
 * @see java.lang.Object
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class BeanUtils extends ObjectUtils {

  /**
   * Introspects the given, required {@link Object POJO} to get information as a JavaBean.
   *
   * @param bean {@link Object} to introspect; must not be {@literal null}.
   * @return {@link BeanInfo} containing information about the {@link Object POJO} as a JavaBean.
   * @throws BeansException if information could not be collected for the given {@link Object}.
   * @throws IllegalArgumentException if the {@link Object} is {@literal null}.
   * @see java.beans.BeanInfo
   */
  public static BeanInfo acquireBeanInformation(@NotNull Object bean) {

    Assert.notNull(bean, "Bean is required");

    Class<?> beanType = resolveType(bean);

    try {
      return Introspector.getBeanInfo(beanType, Object.class);
    }
    catch (IntrospectionException cause) {
      throw newBeanIntrospectionException(cause, "Failed to introspect bean of type [%s]",
        ObjectUtils.getName(beanType));
    }
  }

  /**
   * Utility method used to get a {@link Property} with the given, required {@link String name}
   * from the given, required target {@link Object}.
   *
   * @param <P> {@link Class type} of {@link Property}.
   * @param target {@link Object} from which to get the {@link Property}.
   * @param propertyName {@link String} containing the {@literal name} of the {@link Property}
   * to get from the target {@link Object}.
   * @return a {@link Property} with the given {@link String name} on the target {@link Object}.
   * @throws IllegalArgumentException if the {@link Object target} is {@literal null}.
   * @throws PropertyNotFoundException if a {@link Property} with the given {@link String name}
   * does not exist on the target {@link Object}.
   * @see org.cp.elements.beans.model.Property
   */
  @SuppressWarnings("unchecked")
  public static @NotNull <P extends Property> P getProperty(@NotNull Object target, @NotNull String propertyName) {
    return (P) BeanAdapter.from(target).getModel().getProperty(propertyName);
  }

  /**
   * Constructs a new {@link PropertyChangeEvent} initialized with the given, required
   * {@link Object source}, {@link String propertyName}, {@link Object old value} and {@link Object new value}.
   *
   * @param source {@link Object} source of the {@link PropertyChangeEvent}; must not be {@literal null}.
   * @param propertyName {@link String} containing the {@literal name} of the bean property that changed;
   * must not be {@literal null}.
   * @param oldValue {@link Object} referring to the old value.
   * @param newValue {@link Object} referring to the new value.
   * @return a new {@link PropertyChangeEvent}.
   * @throws IllegalArgumentException if the {@link Object source} is {@literal null}
   * or the {@link String propertyName} is {@literal null} or {@literal empty}.
   * @see java.beans.PropertyChangeEvent
   */
  public static @NotNull PropertyChangeEvent newPropertyChangeEvent(@NotNull Object source,
      @NotNull String propertyName, @Nullable Object oldValue, @Nullable Object newValue) {

    Assert.notNull(source, "Source is required");
    Assert.hasText(propertyName, "Property name [%s] is required", propertyName);

    return new PropertyChangeEvent(source, propertyName, oldValue, newValue);
  }

  /**
   * Resolves the {@link Class type} of the given {@link Object}.
   *
   * @param obj {@link Object} from which to resolve the {@link Class type}.
   * @return the {@link Class type} of the given {@link Object}. If the given {@link Object} is {@literal null}
   * then this method returns {@link Class Object.class}.
   */
  public static @NotNull Class<?> resolveType(@NotNull Object obj) {

    return obj instanceof BeanAdapter ? ((BeanAdapter) obj).getTarget().getClass()
      : obj instanceof PropertyDescriptor ? ((PropertyDescriptor) obj).getPropertyType()
      : obj instanceof Property ? ((Property) obj).getType()
      : obj instanceof Class ? (Class<?>) obj
      : obj != null ? obj.getClass()
      : Object.class;
  }
}
