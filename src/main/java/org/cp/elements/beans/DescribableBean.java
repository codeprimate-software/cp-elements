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
package org.cp.elements.beans;

import java.beans.BeanDescriptor;
import java.beans.BeanInfo;

import org.cp.elements.beans.model.BeanUtils;
import org.cp.elements.lang.Describable;

/**
 * Interface defining a contract to describe a JavaBean ({@link Object POJO}).
 *
 * @author John J. Blum
 * @see java.beans.BeanInfo
 * @see java.beans.BeanDescriptor
 * @see org.cp.elements.lang.Describable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface DescribableBean extends Describable<BeanDescriptor> {

  /**
   * Returns a {@link String description} of this JavaBean ({@link Object POJO}).
   *
   * @return a {@link String} describing this JavaBean ({@link Object POJO}).
   * @see java.beans.BeanDescriptor#getShortDescription()
   * @see #getSummary()
   */
  default String getDescription() {
    return getDescriptor().getShortDescription();
  }

  /**
   * Gets a {@link BeanDescriptor} for this JavaBean ({@link Object POJO}).
   *
   * @return the {@link BeanDescriptor} for this JavaBean ({@link Object POJO}).
   * @see java.beans.BeanDescriptor
   * @see #describe()
   */
  @Override
  default BeanDescriptor getDescriptor() {
    return describe().getBeanDescriptor();
  }

  /**
   * Returns a {@link String summary} of this JavaBean ({@link Object POJO}).
   *
   * @return a {@link String} summarizing this JavaBean ({@link Object POJO}).
   * @see java.beans.BeanDescriptor#getName()
   * @see #getDescription()
   */
  default String getSummary() {
    return getDescriptor().getName();
  }

  /**
   * Returns the {@link BeanInfo} for this Java bean (POJO).
   *
   * @return an instance of {@link BeanInfo} to describe this Java bean.
   * @throws IllegalStateException if this Java beans failed to be described.
   * @see java.beans.Introspector#getBeanInfo(Class)
   * @see java.beans.BeanInfo
   */
  default BeanInfo describe() {
    return BeanUtils.acquireBeanInformation(this);
  }
}
