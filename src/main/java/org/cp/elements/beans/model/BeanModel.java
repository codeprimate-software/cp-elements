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
import java.util.Map;
import java.util.WeakHashMap;

import org.cp.elements.beans.BeansException;
import org.cp.elements.beans.PropertyNotFoundException;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.Dsl;
import org.cp.elements.lang.annotation.FluentApi;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Abstract Data Type (ADT) and model for a {@link Object POJO} represented as a {@literal JavaBean}.
 *
 * @author John Blum
 * @see java.beans.BeanInfo
 * @see org.cp.elements.beans.model.BeanAdapter
 * @see org.cp.elements.lang.annotation.FluentApi
 * @since 1.0.0
 */
@SuppressWarnings("unused")
@FluentApi
public class BeanModel {

  private static final Map<BeanModelCacheKey, BeanModel> beanModelCache = new WeakHashMap<>();

  /**
   * Factory method used to construct a new {@link BeanModel} used to model the given,
   * required {@link BeanAdapter bean}.
   *
   * @param bean {@link BeanAdapter} to model; must not be {@literal null}.
   * @return a new {@link BeanModel} modeling the given, required {@link BeanAdapter bean}.
   * @throws BeansException if information about the {@link BeanAdapter bean} could not be acquired.
   * @throws IllegalArgumentException if the {@link BeanAdapter bean} is {@literal null}.
   * @see org.cp.elements.beans.model.BeanAdapter
   * @see org.cp.elements.lang.annotation.Dsl
   * @see #BeanModel(BeanAdapter)
   */
  @Dsl
  public static @NotNull BeanModel from(@NotNull BeanAdapter bean) {
    return beanModelCache.computeIfAbsent(BeanModelCacheKey.from(bean), key -> new BeanModel(key.getBean()));
  }

  private final BeanAdapter bean;

  private final BeanInfo beanInfo;

  private final Properties beanProperties;

  /**
   * Constructs a new {@link BeanModel} initialized with the given, required {@link BeanAdapter bean}.
   *
   * @param bean {@link BeanAdapter} to model; must not be {@literal null}.
   * @throws BeansException if information about the {@link BeanAdapter bean} could not be acquired.
   * @throws IllegalArgumentException if the {@link BeanAdapter bean} is {@literal null}.
   * @see org.cp.elements.beans.model.BeanAdapter
   */
  protected BeanModel(@NotNull BeanAdapter bean) {

    this.bean = ObjectUtils.requireObject(bean, "Bean is required");
    this.beanInfo = BeanUtils.acquireBeanInformation(this.bean);
    this.beanProperties = Properties.from(this);
  }

  /**
   * Gets a reference to the {@link BeanAdapter bean} for which this {@link BeanModel model} is based.
   *
   * @return a reference to the {@link BeanAdapter bean} for which this {@link BeanModel model} is based.
   * @see org.cp.elements.beans.model.BeanAdapter
   * @see org.cp.elements.lang.annotation.Dsl
   */
  @Dsl
  public @NotNull BeanAdapter getBean() {
    return this.bean;
  }

  /**
   * Gets the {@link BeanInfo} for the {@link BeanAdapter bean} represented by this {@link BeanModel}.
   *
   * @return the {@link BeanInfo} for the {@link BeanAdapter bean} represented by this {@link BeanModel}.
   * @see java.beans.BeanInfo
   */
  public @NotNull BeanInfo getBeanInfo() {
    return this.beanInfo;
  }

  /**
   * Gets the {@link Properties} of the {@link BeanAdapter bean}.
   *
   * @return the {@link Properties} of the {@link BeanAdapter bean}.
   * @see org.cp.elements.beans.model.Properties
   * @see org.cp.elements.lang.annotation.Dsl
   */
  @Dsl
  public @NotNull Properties getProperties() {
    return this.beanProperties;
  }

  /**
   * Gets a single {@link Property} identified by the given {@link String name}.
   *
   * @param name {@link String} containing the {@literal name} of the {@link Property} on the {@link BeanAdapter bean}.
   * @return a single {@link Property} identified by the given {@link String name}.
   * @throws PropertyNotFoundException if a {@link Property} with the given {@link String name} does not exist
   * on the {@link BeanAdapter bean} modeled by this {@link BeanModel}.
   * @see org.cp.elements.beans.model.Property
   * @see org.cp.elements.lang.annotation.Dsl
   * @see #getProperties()
   */
  @Dsl
  public @NotNull Property getProperty(@Nullable String name) {
    return getProperties().findByName(name);
  }

  /**
   * Returns the underlying {@link Object POJO} backing the {@link BeanAdapter bean}.
   *
   * @return the underlying {@link Object POJO} backing the {@link BeanAdapter bean}.
   * @see org.cp.elements.beans.model.BeanAdapter#getTarget()
   * @see java.lang.Object
   * @see #getBean()
   */
  public @NotNull Object getTargetObject() {
    return getBean().getTarget();
  }

  /**
   * Gets the {@link Class type} of the {@link Object target}.
   *
   * @return the {@link Class type} of the {@link Object target}.
   * @see java.lang.Object#getClass()
   * @see #getTargetObject()
   * @see java.lang.Class
   */
  public @NotNull Class<?> getTargetType() {
    return getTargetObject().getClass();
  }

  /**
   * Abstract Data Type (ADT) used to model the {@literal key} in the {@link BeanModel} {@literal cache}.
   */
  protected static class BeanModelCacheKey {

    /**
     * Factory method used to construct a new {@link BeanModelCacheKey} initialized with the given,
     * required {@link BeanAdapter bean}.
     *
     * @param bean {@link BeanAdapter} used as a key component in the {@link BeanModel} {@literal cache};
     * must not be {@literal null}.
     * @return a new {@link BeanModelCacheKey}.
     * @throws IllegalArgumentException if the {@link BeanAdapter bean} is {@literal null}.
     * @see org.cp.elements.beans.model.BeanAdapter
     * @see #BeanModelCacheKey(BeanAdapter)
     */
    protected static @NotNull BeanModelCacheKey from(@NotNull BeanAdapter bean) {
      return new BeanModelCacheKey(bean);
    }

    private final BeanAdapter bean;

    /**
     * Constructs a new {@link BeanModelCacheKey} initialized with the given,
     * required {@link BeanAdapter bean}.
     *
     * @param bean {@link BeanAdapter} used as a key component in the {@link BeanModel} {@literal cache};
     * must not be {@literal null}.
     * @throws IllegalArgumentException if the {@link BeanAdapter bean} is {@literal null}.
     * @see org.cp.elements.beans.model.BeanAdapter
     */
    protected BeanModelCacheKey(@NotNull BeanAdapter bean) {
      this.bean = ObjectUtils.requireObject(bean, "Bean is required");
    }

    /**
     * Gets a reference to the {@link BeanAdapter bean} used as a key component
     * in the {@link BeanModel} {@literal cache}.
     *
     * @return the {@link BeanAdapter bean} used as a key component in the {@link BeanModel} {@literal cache}.
     * @see org.cp.elements.beans.model.BeanAdapter
     */
    protected @NotNull BeanAdapter getBean() {
      return this.bean;
    }

    @Override
    public boolean equals(Object obj) {

      if (this == obj) {
        return true;
      }

      if (!(obj instanceof BeanModelCacheKey)) {
        return false;
      }

      BeanModelCacheKey that = (BeanModelCacheKey) obj;

      return this.getBean().equals(that.getBean());
    }

    @Override
    public int hashCode() {
      return ObjectUtils.hashCodeOf(getBean());
    }

    @Override
    public String toString() {
      return String.valueOf(getBean());
    }
  }
}
