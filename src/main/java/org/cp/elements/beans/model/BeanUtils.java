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

import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;

import org.cp.elements.beans.BeansException;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;

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
      throw new BeansException(String.format("Failed to introspect bean of type [%s]",
        ObjectUtils.getName(beanType))) { };
    }
  }

  /**
   * Resolves the {@link Class type} of the given {@link Object}.
   *
   * @param obj {@link Object} from which to resolve the {@link Class type}.
   * @return the {@link Class type} of the given {@link Object}.
   */
  public static @NotNull Class<?> resolveType(@NotNull Object obj) {

    return obj instanceof BeanAdapter ? ((BeanAdapter) obj).getTarget().getClass()
      : obj instanceof PropertyDescriptor ? ((PropertyDescriptor) obj).getPropertyType()
      : obj instanceof Property ? ((Property) obj).getType()
      : obj != null ? obj.getClass()
      : Object.class;
  }
}
