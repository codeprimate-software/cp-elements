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

import org.cp.elements.beans.BeansException;
import org.cp.elements.beans.PropertyNotFoundException;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Abstract Data Type (ADT) and model for a {@link Object POJO} represented as a JavaBean.
 *
 * @author John Blum
 * @see java.beans.BeanInfo
 * @see org.cp.elements.beans.model.BeanAdapter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class BeanModel {

  /**
   * Factory method used to construct a new instance of {@link BeanModel} used to model the given,
   * required {@link BeanAdapter bean}.
   *
   * @param bean {@link BeanAdapter} to model; must not be {@literal null}.
   * @return a new {@link BeanModel} modeling the given, required {@link BeanAdapter bean}.
   * @throws BeansException if information about the {@link BeanAdapter bean} could not be acquired.
   * @throws IllegalArgumentException if the {@link BeanAdapter bean} is {@literal null}.
   * @see org.cp.elements.beans.model.BeanAdapter
   * @see #BeanModel(BeanAdapter)
   */
  public static @NotNull BeanModel from(@NotNull BeanAdapter bean) {
    return new BeanModel(bean);
  }

  private final BeanAdapter bean;

  private final BeanInfo beanInfo;

  private final Properties beanProperties;

  /**
   * Constructs a new instance of {@link BeanModel} initialized with the given, required {@link BeanAdapter bean}.
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
   */
  protected @NotNull BeanAdapter getBean() {
    return this.bean;
  }

  /**
   * Gets the {@link BeanInfo} for the {@link BeanAdapter bean} represented by this {@link BeanModel model}.
   *
   * @return the {@link BeanInfo} for the {@link BeanAdapter bean} represented by this {@link BeanModel model}.
   * @see java.beans.BeanInfo
   */
  protected @NotNull BeanInfo getBeanInfo() {
    return this.beanInfo;
  }

  /**
   * Gets the {@link Properties} of the {@link BeanAdapter bean}.
   *
   * @return the {@link Properties} of the {@link BeanAdapter bean}.
   * @see org.cp.elements.beans.model.Properties
   */
  public @NotNull Properties getProperties() {
    return this.beanProperties;
  }

  /**
   * Gets a single {@link Property} identified by the given {@link String name}.
   *
   * @param name {@link String} containing the {@literal name} of the {@link Property} on this bean.
   * @return a single {@link Property} identified by the given {@link String name}.
   * @throws PropertyNotFoundException if a {@link Property} with the given {@link String name} does not exist
   * on the bean modeled by this model.
   * @see org.cp.elements.beans.model.Property
   * @see #getProperties()
   */
  public @NotNull Property getProperty(@Nullable String name) {
    return getProperties().findBy(name);
  }

  /**
   * Returns the underlying {@link Object POJO} backing this bean.
   *
   * @return the underlying {@link Object POJO} backing this bean.
   * @see java.lang.Object
   */
  public @NotNull Object getTargetObject() {
    return getBean().getTarget();
  }
}
