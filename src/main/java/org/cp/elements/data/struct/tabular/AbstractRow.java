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

package org.cp.elements.data.struct.tabular;

import static org.cp.elements.lang.ElementsExceptionsFactory.newMappingException;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalStateException;

import java.beans.BeanInfo;
import java.beans.Introspector;
import java.lang.reflect.Constructor;
import java.util.Arrays;
import java.util.Optional;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.util.ArrayUtils;

/**
 * The AbstractRow class is an abstract base class implementing the Row interface encapsulating functionality common
 * to all Row implementations.
 *
 * @author John J. Blum
 * @see Column
 * @see Row
 * @see Table
 * @see View
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractRow implements Row {

  /**
   * Constructs a new {@link AbstractRow} initialized with given array of {@link Object values}
   * used as {@link Object values} for each {@link Column} of the new {@link AbstractRow}.
   *
   * @param values array of {@link Object values} used as {@link Object values} for each {@link Column}
   * of the new {@link AbstractRow}.
   * @return a new {@link AbstractRow} initialized with the given array of {@link Object values}.
   */
  @NullSafe
  public static AbstractRow of(Object[] values) {

    Object[] rowValues = ArrayUtils.nullSafeArray(values);

    return new AbstractRow() {

      @Override
      @SuppressWarnings("unchecked")
      public <T> T getValue(int columnIndex) {
        return (T) rowValues[columnIndex];
      }

      @Override
      @SuppressWarnings("unchecked")
      public <T> T setValue(int columnIndex, T value) {

        T currentValue = (T) rowValues[columnIndex];

        rowValues[columnIndex] = value;

        return currentValue;
      }

      @Override
      public Object[] values() {
        return rowValues;
      }
    };
  }

  private View view;

  /**
   * Sets the {@link View} to which this {@link Row} is associated.
   *
   * @param view {@link View} containing this {@link Row}.
   * @see org.cp.elements.data.struct.tabular.View
   * @see #getView()
   */
  public void setView(View view) {
    this.view = view;
  }

  /**
   * Returns an {@link Optional} {@link View} to which this {@link Row} is associated.
   *
   * @return an {@link Optional} {@link View} containing this {@link Row}.
   * @see org.cp.elements.data.struct.tabular.View
   * @see java.util.Optional
   */
  @Override
  public Optional<View> getView() {
    return Optional.ofNullable(this.view);
  }

  /**
   * Maps the {@link Object values} of this {@link Row} to the given {@link Object}.
   *
   * The {@link Class type} must have a default, public no argument {@link Constructor}.
   *
   * @param <T> {@link Class type} of the {@link Object} to map with the {@link Object value} from this {@link Row}.
   * @param type {@link Class type} of the {@link Object} to map with the {@link Object values} from this {@link Row}.
   * @return the {@link Object} populated with the {@link Object values} of this {@link Row}.
   * @see java.lang.Class
   */
  @Override
  public <T> T map(Class<T> type) {

    Assert.notNull(type, "Class type of the object to map is required");

    return getView().map(view -> {

      try {

        BeanInfo beanInfo = Introspector.getBeanInfo(type);

        T instance = type.newInstance();

        Arrays.stream(beanInfo.getPropertyDescriptors()).forEach(propertyDescriptor ->
          Optional.ofNullable(propertyDescriptor.getWriteMethod()).ifPresent(writeMethod -> {

            String propertyName = propertyDescriptor.getName();

            view.getColumn(propertyName).ifPresent(column -> {

              Object value = getValue(column);

              ObjectUtils.invoke(instance, writeMethod, ArrayUtils.asArray(value), Void.class);

            });
          }));

        return instance;
      }
      catch (Exception cause) {
        throw newMappingException(cause, "Failed to map object of type [%s] with values from this Row [%s]",
          type.getName(), this);
      }
    })
    .orElseThrow(() -> newIllegalStateException("This Row [%s] is not associated with a View", this));
  }

  /**
   * Returns a {@link String} representation of this {@link Row}.
   *
   * @return a {@link String} describing this {@link Row}.
   * @see java.lang.String
   */
  @Override
  public String toString() {
    return String.format("Row %d", index());
  }

  /**
   * Returns the {@link Object values} of this {@link Row} in an array.
   *
   * @return the {@link Object values} of this {@link Row} in an array.
   */
  @Override
  public Object[] values() {
    return ObjectUtils.EMPTY_OBJECT_ARRAY;
  }
}
