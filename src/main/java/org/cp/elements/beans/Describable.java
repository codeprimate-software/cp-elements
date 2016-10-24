/*
 * Copyright 2016 Author or Authors.
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

import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;

/**
 * The {@link Describable} interface describes a Java Bean (POJO).
 *
 * @author John J. Blum
 * @see java.beans.BeanInfo
 * @see java.beans.Introspector
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Describable {

  /**
   * Returns a description of this Java Bean (POJO).
   *
   * @return a {@link String} describing this Java Bean.
   */
  String getDescription();

  /**
   * Returns a summary of this Java Bean (POJO).
   *
   * @return a {@link String} summarizing this Java Bean.
   */
  String getSummary();

  /**
   * Returns the {@link BeanInfo} for this Java bean (POJO).
   *
   * @return an instance of {@link BeanInfo} to describe this Java bean.
   * @throws IllegalStateException if this Java beans failed to be described.
   * @see java.beans.Introspector#getBeanInfo(Class)
   * @see java.beans.BeanInfo
   */
  default BeanInfo describe() {
    try {
      return Introspector.getBeanInfo(getClass());
    }
    catch (IntrospectionException e) {
      throw new IllegalStateException(String.format("Failed to describe this bean [%s]", this), e);
    }
  }
}
