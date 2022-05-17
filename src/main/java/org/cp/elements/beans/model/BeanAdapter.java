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

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Adapter for a given Java Bean, or {@link Object POJO}.
 *
 * @author John Blum
 * @see java.lang.Object
 * @since 1.0.0
 */
public class BeanAdapter {

  /**
   * Factory method used to construct a new instance of {@link BeanAdapter} initialized with the given,
   * required {@link Object} to adapt.
   *
   * @param target {@link Object} to adapt as a Java Bean.
   * @return a new {@link BeanAdapter} initialized with the given, required {@link Object} (POJO).
   * @throws IllegalArgumentException if the target {@link Object} is {@literal null}.
   * @see #BeanAdapter(Object)
   * @see java.lang.Object
   */
  public static BeanAdapter from(@NotNull Object target) {
    return new BeanAdapter(target);
  }

  private final Object target;

  /**
   * Constructs a new instance of {@link BeanAdapter} initialized with the given, required {@link Object} to adapt.
   *
   * @param target {@link Object} to adapt as a Java Bean.
   * @throws IllegalArgumentException if the target {@link Object} is {@literal null}.
   * @see java.lang.Object
   */
  public BeanAdapter(@NotNull Object target) {

    Assert.notNull(target, "The target object to adapt is required");

    this.target = target;
  }

  /**
   * Returns a reference to the target {@link Object} adapted as a Java Bean.
   *
   * @return a reference to the target {@link Object}.
   */
  protected Object getTarget() {
    return this.target;
  }

  public <T> T getProperty(@NotNull String propertyName) {

    Assert.hasText(propertyName, "The name [%s] of the property is required", propertyName);

    return null;
  }

  public <T> void setProperty(@NotNull String propertyName, @Nullable T value) {

    Assert.notNull(propertyName, "The name [%s] of the property to set is required", propertyName);

  }
}
