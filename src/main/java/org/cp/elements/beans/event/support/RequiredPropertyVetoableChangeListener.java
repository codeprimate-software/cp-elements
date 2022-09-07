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
package org.cp.elements.beans.event.support;

import static org.cp.elements.lang.ElementsExceptionsFactory.newPropertyNotSetException;

import java.beans.PropertyChangeEvent;
import java.beans.VetoableChangeListener;
import java.util.Objects;

import org.cp.elements.beans.PropertyNotFoundException;
import org.cp.elements.beans.PropertyNotSetException;
import org.cp.elements.beans.annotation.Required;
import org.cp.elements.beans.event.AbstractVetoableChangeListener;
import org.cp.elements.beans.model.BeanAdapter;
import org.cp.elements.beans.model.Property;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * A {@link VetoableChangeListener} implementation used to enforce {@literal required} properties
 * declared on a JavaBean/POJO contain an actual {@link Object value} when set.
 *
 * {@literal Required} properties are declared with the Elements {@link Required} annotation on a JavaBean/POJO
 * {@link java.lang.reflect.Field} or {@literal accessor/mutator} {@link java.lang.reflect.Method}.
 *
 * @author John Blum
 * @see java.beans.PropertyChangeEvent
 * @see java.beans.VetoableChangeListener
 * @see org.cp.elements.beans.PropertyNotSetException
 * @see org.cp.elements.beans.annotation.Required
 * @see org.cp.elements.beans.event.AbstractVetoableChangeListener
 * @see org.cp.elements.beans.model.Property
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class RequiredPropertyVetoableChangeListener extends AbstractVetoableChangeListener {

  public static final RequiredPropertyVetoableChangeListener INSTANCE = new RequiredPropertyVetoableChangeListener();

  /**
   * Enforces that the property identified in the {@link PropertyChangeEvent} is set if the {@link Property}
   * of the JavaBean/POJO is {@link Property#isRequired()}.
   *
   * @param event {@link PropertyChangeEvent} to process; never {@literal null}.
   * @throws IllegalArgumentException if the {@link Object source} of the {@link PropertyChangeEvent}
   * is {@literal null}.
   * @throws PropertyNotFoundException if the {@link Property} identified by
   * the {@link PropertyChangeEvent#getPropertyName()} cannot be found.
   * @throws PropertyNotSetException if the {@link PropertyChangeEvent#getPropertyName() identified property}
   * is {@link Property#isRequired()} but not set to a valid {@link PropertyChangeEvent#getNewValue() new value}.
   * @see java.beans.PropertyChangeEvent
   */
  @Override
  protected void doHandle(@NotNull PropertyChangeEvent event) {

    Object source = event.getSource();

    BeanAdapter bean = BeanAdapter.from(source);

    Property property = bean.getModel().getProperty(event.getPropertyName());

    if (property.isRequired()) {

      boolean propertySet = isSet(property, event.getNewValue());

      Assert.isTrue(propertySet, newPropertyNotSetException("Property [%s] not set", property.getName()));
    }
  }

  private boolean isSet(@NotNull Property property, @Nullable Object value) {
    return Objects.nonNull(value) && (isNotStringType(property) || isValuableString(value));
  }

  private boolean isNotStringType(@NotNull Property property) {
    return !isStringType(property);
  }

  private boolean isStringType(@NotNull Property property) {
    return String.class.equals(property.getType());
  }

  private boolean isValuableString(Object value) {
    return isValuableString((String) value);
  }

  private boolean isValuableString(String value) {
    return StringUtils.hasText(value);
  }
}
