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

import java.util.Optional;

import org.cp.elements.beans.model.BeanAdapter;
import org.cp.elements.beans.model.BeanModel;
import org.cp.elements.beans.model.Property;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.util.ArrayUtils;

/**
 * Abstract base class implementing the {@link Row} interface encapsulating functionality common to
 * all {@link Row} implementations.
 *
 * @author John J. Blum
 * @see org.cp.elements.data.struct.tabular.Column
 * @see org.cp.elements.data.struct.tabular.Row
 * @see org.cp.elements.data.struct.tabular.Table
 * @see org.cp.elements.data.struct.tabular.View
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractRow implements Row {

  protected static final String ROW_TO_STRING = "Row %d";

  /**
   * Constructs a new {@link AbstractRow} initialized with given array of {@link Object values}
   * used as {@link Object values} for each {@link Column} of the new {@link AbstractRow}.
   *
   * The new {@link AbstractRow} is backed by the given array of {@link Object values}. The array is not copied.
   *
   * @param values array of {@link Object values} used as {@link Object values} for each {@link Column}
   * of the new {@link AbstractRow}.
   * @return a new {@link AbstractRow} initialized with the given array of {@link Object values}.
   * @see org.cp.elements.data.struct.tabular.Row#setValue(int, Object)
   * @see org.cp.elements.data.struct.tabular.Row#values()
   */
  @NullSafe
  public static @NotNull AbstractRow of(@NotNull Object[] values) {

    Object[] rowValues = ArrayUtils.nullSafeArray(values);

    return new AbstractRow() {

      @Override
      @SuppressWarnings("unchecked")
      public <T> T setValue(int columnIndex, T newValue) {

        T currentValue = (T) rowValues[columnIndex];

        rowValues[columnIndex] = newValue;

        return currentValue;
      }

      @Override
      public Object[] values() {
        return rowValues;
      }
    };
  }

  private View view;

  @Override
  public Optional<View> getView() {
    return Optional.ofNullable(this.view);
  }

  /**
   * Sets the {@link View} containing this {@link Row}.
   *
   * @param view {@link View} containing this {@link Row}.
   * @see org.cp.elements.data.struct.tabular.View
   * @see #getView()
   */
  public void setView(@Nullable View view) {
    this.view = view;
  }

  @Override
  public @NotNull <T> T map(@NotNull Class<T> type) {

    Assert.notNull(type, "Class type of the object to map from this Row [%d] is required", index());

    return getView()
      .map(view -> {

        try {

          T instance = type.newInstance();

          BeanAdapter instanceBean = BeanAdapter.from(instance);
          BeanModel instanceModel = instanceBean.getModel();

          instanceModel.getProperties().stream()
            .filter(Property::isWritable)
            .forEach(property -> {

              // TODO: Handle bean property to table column mapping.
              // TODO: Handle non-present table columns for bean properties.
              // TODO: Handle table column value to bean property type conversions.
              view.getColumn(property.getName())
                .ifPresent(column -> property.setValue(getValue(column)));
            });

          return instance;
        }
        catch (Exception cause) {
          throw newMappingException(cause, "Failed to map object of type [%s] with values from this Row [%d]",
            type.getName(), index());
        }
      })
      .orElseThrow(() -> newIllegalStateException("Row [%d] is not associated with a View", index()));
  }

  @Override
  public @NotNull Row store(@NotNull Object target) {

    Assert.notNull(target, "Object to map to this Row [%d] is required", index());

    getView()
      .map(view -> {

        BeanAdapter targetBean = BeanAdapter.from(target);
        BeanModel targetModel = targetBean.getModel();

        targetModel.getProperties().stream()
          .filter(Property::isReadable)
          .forEach(property -> {

            // TODO: Handle bean property to table column mapping.
            // TODO: Handle bean property value to table column type conversions.
            // TODO: Handle missing bean properties for non-nullable table columns.
            view.getColumn(property.getName())
              .ifPresent(column -> setValue(column, property.getValue()));
          });

        return target;

      })
      .orElseThrow(() -> newIllegalStateException("Row [%d] is not associated with a View", index()));

    return this;
  }

  /**
   * Returns a {@link String} representation of this {@link Row}.
   *
   * @return a {@link String} describing this {@link Row}.
   * @see java.lang.Object#toString()
   */
  @Override
  public @NotNull String toString() {
    return String.format(ROW_TO_STRING, index());
  }

  @Override
  public Object[] values() {
    return ObjectUtils.EMPTY_OBJECT_ARRAY;
  }
}
