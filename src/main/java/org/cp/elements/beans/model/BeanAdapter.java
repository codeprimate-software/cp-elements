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

import java.util.concurrent.atomic.AtomicReference;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Adapter for a given {@link Object POJO} used as a {@literal JavaBean}.
 *
 * @author John Blum
 * @see java.lang.Object
 * @since 1.0.0
 */
public class BeanAdapter {

  /**
   * Factory method used to construct a new instance of {@link BeanAdapter} initialized with the given,
   * required {@link Object POJO} to adapt as a {@literal JavaBean}.
   *
   * @param target {@link Object} to adapt as a {@literal JavaBean}; must not be {@literal null}.
   * @return a new {@link BeanAdapter} initialized with the given, required {@link Object POJO}.
   * @throws IllegalArgumentException if the target {@link Object} is {@literal null}.
   * @see #BeanAdapter(Object)
   * @see java.lang.Object
   */
  public static BeanAdapter from(@NotNull Object target) {
    return new BeanAdapter(target);
  }

  private final AtomicReference<BeanModel> beanModelReference = new AtomicReference<>(null);

  private final Object target;

  /**
   * Constructs a new instance of {@link BeanAdapter} initialized with the given, required {@link Object POJO}
   * to adapt as a {@literal JavaBean}.
   *
   * @param target {@link Object} adapted as a {@literal JavaBean}.
   * @throws IllegalArgumentException if the target {@link Object} is {@literal null}.
   * @see java.lang.Object
   */
  public BeanAdapter(@NotNull Object target) {
    this.target = ObjectUtils.requireObject(target, "The target object to adapt as a JavaBean is required");
  }

  /**
   * Lazily gets the {@link BeanModel} modeling this bean.
   *
   * @return the {@link BeanModel} modeling this bean.
   * @see org.cp.elements.beans.model.BeanModel
   */
  protected @NotNull BeanModel getModel() {
    return this.beanModelReference.updateAndGet(model -> model != null ? model : BeanModel.from(this));
  }

  /**
   * Returns a reference to the target {@link Object POJO} adapted as a {@literal JavaBean}.
   *
   * @return a reference to the target {@link Object POJO} adapted as a {@literal JavaBean}.
   * @see java.lang.Object
   */
  protected @NotNull Object getTarget() {
    return this.target;
  }

  /**
   * Get the {@link Object value} of the property identified by {@link String name}.
   *
   * @param <T> {@link Class type} of the property {@link Object value}.
   * @param propertyName {@link String} containing the {@literal name} of the property.
   * @return the {@link Object value} of the property identified by {@link String name}.
   * @throws IllegalArgumentException if the {@link String property name} is {@literal null},
   * {@literal blank} or {@literal empty}.
   * @see #setPropertyValue(String, Object)
   */
  @SuppressWarnings("unchecked")
  public @Nullable <T> T getPropertyValue(@NotNull String propertyName) {

    Assert.hasText(propertyName, "The name [%s] of the property is required", propertyName);

    return (T) getModel().getProperty(propertyName).getValue();
  }

  /**
   * Set the property identified by {@link String name} to the given {@link Object value}.
   *
   * @param propertyName {@link String} containing the {@literal name} of the property.
   * @throws IllegalArgumentException if the {@link String property name} is {@literal null},
   * {@literal blank} or {@literal empty}.
   * @see #getPropertyValue(String)
   */
  public void setPropertyValue(@NotNull String propertyName, @Nullable Object value) {

    Assert.hasText(propertyName, "The name [%s] of the property to set is required", propertyName);

    getModel().getProperty(propertyName).setValue(value);
  }
}
