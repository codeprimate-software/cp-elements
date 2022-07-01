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
import java.lang.reflect.AnnotatedElement;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Predicate;

import org.cp.elements.beans.PropertyReadException;
import org.cp.elements.beans.PropertyWriteException;
import org.cp.elements.beans.annotation.Required;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Nameable;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.lang.annotation.Transient;
import org.cp.elements.lang.reflect.ModifierUtils;

/**
 * Abstract Data Type (ADT) modeling a {@link Object bean} property.
 *
 * @author John Blum
 * @see java.beans.PropertyDescriptor
 * @see java.lang.Comparable
 * @see java.lang.reflect.Field
 * @see java.lang.reflect.Method
 * @see org.cp.elements.lang.Nameable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class Property implements Comparable<Property>, Nameable<String> {

  protected static final Predicate<AnnotatedElement> IS_REQUIRED = element ->
    element != null && element.isAnnotationPresent(Required.class);

  protected static final Predicate<AnnotatedElement> IS_TRANSIENT_ANNOTATED = element -> element != null
    && (element.isAnnotationPresent(Transient.class) || element.isAnnotationPresent(java.beans.Transient.class));

  protected static final Predicate<AnnotatedElement> IS_TRANSIENT_FIELD = element ->
    element instanceof Field && ModifierUtils.isTransient(element);

  protected static final Predicate<AnnotatedElement> IS_TRANSIENT = IS_TRANSIENT_FIELD.or(IS_TRANSIENT_ANNOTATED);

  /**
   * Factory method used to construct a new instance of {@link Property} from the given, required {@link BeanModel}
   * modeling the bean containing the property and a given, required {@link PropertyDescriptor}
   * describing the bean property.
   *
   * @param beanModel {@link BeanModel} modeling the bean containing the property.
   * @param propertyDescriptor {@link PropertyDescriptor} describing the bean property.
   * @return a new {@link Property} modeling the bean property.
   * @throws IllegalArgumentException if the {@link BeanModel} or {@link PropertyDescriptor} are {@literal null}.
   * @see #Property(BeanModel, PropertyDescriptor)
   * @see org.cp.elements.beans.model.BeanModel
   * @see java.beans.PropertyDescriptor
   */
  public static @NotNull Property from(@NotNull BeanModel beanModel, @NotNull PropertyDescriptor propertyDescriptor) {
    return new Property(beanModel, propertyDescriptor);
  }

  private final AtomicReference<Field> fieldReference = new AtomicReference<>(null);

  private final FieldResolver fieldResolver = new PropertyNameFieldResolver();

  private final BeanModel beanModel;

  private final PropertyDescriptor propertyDescriptor;

  /**
   * Constructs a new instance of {@link Property} initialized with the given, required {@link BeanModel}
   * modeling the bean containing this {@link Property} along with the given, required {@link PropertyDescriptor}
   * describing the bean {@link Property}.
   *
   * @param beanModel {@link BeanModel} modeling the bean that contains this property; must not be {@literal null}.
   * @param propertyDescriptor {@link PropertyDescriptor} describing the bean property; must not be {@literal null}.
   * @throws IllegalArgumentException if the {@link BeanModel} or {@link PropertyDescriptor} are {@literal null}.
   * @see org.cp.elements.beans.model.BeanModel
   * @see java.beans.PropertyDescriptor
   */
  protected Property(@NotNull BeanModel beanModel, @NotNull PropertyDescriptor propertyDescriptor) {

    this.beanModel = ObjectUtils.requireObject(beanModel, "BeanModel is required");
    this.propertyDescriptor = ObjectUtils.requireObject(propertyDescriptor, "PropertyDescriptor is required");
  }

  /**
   * Gets the {@link BeanAdapter bean} to which this {@link Property} belongs.
   *
   * @return the {@link BeanAdapter bean} to which this {@link Property} belongs.
   * @see org.cp.elements.beans.model.BeanAdapter
   */
  protected @NotNull BeanAdapter getBean() {
    return getBeanModel().getBean();
  }

  /**
   * Gets the {@link BeanModel} modeling the bean containing this {@link Property}.
   *
   * @return the {@link BeanModel} modeling the bean containing this {@link Property}.
   * @see org.cp.elements.beans.model.BeanModel
   */
  protected @NotNull BeanModel getBeanModel() {
    return this.beanModel;
  }

  /**
   * Gets the {@literal JavaBeans} {@link PropertyDescriptor} describing this bean {@link Property}.
   *
   * @return the {@literal JavaBeans} {@link PropertyDescriptor} describing this bean {@link Property}.
   * @see java.beans.PropertyDescriptor
   */
  protected @NotNull PropertyDescriptor getDescriptor() {
    return this.propertyDescriptor;
  }

  /**
   * Gets the {@link Object} {@link Field} backing this bean {@link Property} if not derived.
   *
   * The {@link Object} {@link Field} may be {@literal null} if this bean {@link Property} is derived
   * from some other {@link Object} {@link Field}. Think of an age property on a Person type derived
   * from a person's date of birth.
   *
   * @return the {@link Object} {@link Field} backing this bean {@link Property} if not derived.
   * @see org.cp.elements.beans.model.FieldResolver#resolve(Property)
   * @see java.lang.reflect.Field
   */
  protected @Nullable Field getField() {
    return fieldReference.updateAndGet(field -> field != null ? field
      : this.fieldResolver.resolve(this));
  }

  /**
   * Gets the {@link Method} used to read from this {@link Property}.
   *
   * @return the {@link Method} used to read from this {@link Property};
   * may return {@literal null} if the {@link Property} cannot be read.
   * @see java.lang.reflect.Method
   * @see #getWriteMethod()
   */
  protected @Nullable Method getReadMethod() {
    return getDescriptor().getReadMethod();
  }

  /**
   * Gets the underlying {@link Object POJO} backing the bean for this {@link Property}.
   *
   * @return the underlying {@link Object POJO} backing the bean for this {@link Property}.
   * @see java.lang.Object
   */
  protected @NotNull Object getTargetObject() {
    return getBean().getTarget();
  }

  /**
   * Gets the {@link Method} used to write to this {@link Property}.
   *
   * @return the {@link Method} used to write to this {@link Property};
   * may return {@literal null} if the {@link Property} cannot be written,
   * such as for a read-only {@link Property}.
   * @see java.lang.reflect.Method
   * @see #getReadMethod()
   */
  protected @Nullable Method getWriteMethod() {
    return getDescriptor().getWriteMethod();
  }

  /**
   * Determines whether this {@link Property} is derived from another bean {@link Property}.
   *
   * This means this {@link Property} is not backed by an {@link Object} {@link Field}.
   * For example, this {@link Property} may represent the {@literal age} property of a {@literal Person} type
   * where age is computed from the person's {@literal date of birth} using the {@literal birthdate} {@link Field}.
   *
   * @return a boolean value indicating whether this {@link Property} is derived from
   * another bean {@link Property}.
   * @see #getField()
   */
  public boolean isDerived() {
    return getField() == null;
  }

  /**
   * Determine whether this {@link Property} can be read.
   *
   * @return a boolean value indicating whether this {@link Property} can be read.
   * @see #getReadMethod()
   */
  public boolean isReadable() {
    return getReadMethod() != null;
  }

  /**
   * Determines whether this {@link Property} is required.
   *
   * A {@link Property} is {@literal required} if the {@link Field} backing this {@link Property},
   * the {@link Method getter method} or the {@link Method setter method} are annotated with
   * the Element's {@link Required} annotation.
   *
   * @return a boolean value indicating whether this {@link Property} is required.
   */
  public boolean isRequired() {

    return IS_REQUIRED.test(getField())
      || IS_REQUIRED.test(getReadMethod())
      || IS_REQUIRED.test(getWriteMethod());
  }

  /**
   * Determines whether this {@link Property} is transient.
   *
   * A {@link Property} is {@literal transient} if the {@link Field} backing this {@link Property} is declared with
   * the {@link transient} keyword, or the {@link Method getter method} or {@link Method setter method}
   * are annotated with the Elements {@link Transient} annotation or the JavaBeans {@link java.beans.Transient}
   * annotation.
   *
   * @return a boolean value indicating whether this {@link Property} is transient.
   */
  public boolean isTransient() {

    return IS_TRANSIENT.test(getField())
      || IS_TRANSIENT.test(getReadMethod())
      || IS_TRANSIENT.test(getWriteMethod());
  }

  /**
   * Determine whether this {@link Property} can be written.
   *
   * @return a boolean value indicating whether this {@link Property} can be written.
   * @see #getWriteMethod()
   */
  public boolean isWritable() {
    return getWriteMethod() != null;
  }

  /**
   * Gets the {@link String name} of this {@link Property}.
   *
   * @return the {@link String name} of this {@link Property}.
   * @see java.lang.String
   */
  public @NotNull String getName() {
    return getDescriptor().getName();
  }

  /**
   * Gets the {@link Class type} of this {@link Property}.
   *
   * @return the {@link Class type} of this {@link Property}.
   * @see java.lang.Class
   */
  @SuppressWarnings("unchecked")
  public @NotNull Class<?> getType() {
    return ObjectUtils.returnFirstNonNullValue(getDescriptor().getPropertyType(), Object.class);
  }

  /**
   * Gets the {@link Object value} of this {@link Property}.
   *
   * @return the {@link Object value} of this {@link Property}.
   * @throws PropertyReadException if this {@link Property} cannot be read.
   * @see java.lang.Object
   */
  public @Nullable Object getValue() {

    Assert.state(isReadable(), new PropertyReadException(String.format("Property [%s] of bean [%s] is not readable",
      getName(), getBean())));

    return ObjectUtils.invoke(getTargetObject(), getReadMethod(), new Object[0], Object.class);
  }

  /**
   * Sets the {@link Object value} of this {@link Property}.
   *
   * @param value {@link Object value} to set for this {@link Property}.
   * @throws PropertyWriteException if this {@link Property} cannot be written.
   * @see java.lang.Object
   */
  public void setValue(Object value) {

    Assert.state(isWritable(), new PropertyWriteException(String.format("Property [%s] of bean [%s] is not writable",
      getName(), getBean())));

    ObjectUtils.invoke(getTargetObject(), getWriteMethod(), new Object[] { value }, Void.class);
  }

  /**
   * Compares this {@link Property} to the given, required {@link Property} to determine (sort) order.
   *
   * {@link Property Properties} are ordered by {@link #getName()}.
   *
   * @param property {@link Property} to compare with this {@link Property}.
   * @return an {@link Integer} value indicating the order of  this {@link Property} relative to the given,
   * required {@link Property}. Returns less than zero if this {@link Property} is sorted before the given
   * {@link Property}, greater than zero if this {@link Property} is sorted after the given {@link Property}
   * and zero if this {@link Property} is equal to the given {@link Property}.
   * @see #getName()
   */
  @Override
  public int compareTo(@NotNull Property property) {
    return this.getName().compareTo(property.getName());
  }

  /**
   * Determines whether this {@link Property} is equal to the given {@link Object}.
   *
   * @param obj {@link Object} to evaluate for equality with this {@link Property}.
   * @return a boolean value indicating whether this {@link Property} is equal to the given {@link Object}.
   * @see #getName()
   */
  @Override
  public boolean equals(Object obj) {

    if (this == obj) {
      return true;
    }

    if (!(obj instanceof Property)) {
      return false;
    }

    Property that = (Property) obj;

    return ObjectUtils.equals(this.getName(), that.getName());
  }

  /**
   * Computes the {@literal hash value} for this {{@link Property}.
   *
   * @return an {@link Integer} value with the hash code of this {@link Property}.
   * @see #getName()
   */
  @Override
  public int hashCode() {
    return ObjectUtils.hashCodeOf(getName());
  }

  /**
   * Returns the {@link String name} of this {@link Property}.
   *
   * @return the {@link String name} of this {@link Property}.
   * @see #getName()
   */
  @Override
  public String toString() {
    return getName();
  }
}
