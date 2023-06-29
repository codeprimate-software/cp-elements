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

import org.cp.elements.beans.Bean;
import org.cp.elements.beans.model.BeanAdapter;
import org.cp.elements.beans.model.BeanModel;
import org.cp.elements.beans.model.Property;
import org.cp.elements.data.struct.tabular.support.BeanPropertyToTableColumnResolver;
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
   * <p>
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

  /**
   * Returns a reference to the configured {@link BeanPropertyToTableColumnResolver} to resolve
   * the {@link Table} {@link Column} for a given {@link Bean} {@link Property}.
   *
   * @return a reference tot he configured {@link BeanPropertyToTableColumnResolver}.
   * @see org.cp.elements.data.struct.tabular.support.BeanPropertyToTableColumnResolver
   */
  @SuppressWarnings("all")
  protected BeanPropertyToTableColumnResolver beanPropertyToTableColumnResolver() {

    return property -> {

      String propertyName = property.getName();
      String columnName = propertyName;

      return getView()
        .filter(view -> view.contains(columnName))
        .flatMap(view -> view.getColumn(columnName))
        .map(Column.class::cast);
    };
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

              // TODO: Handle missing bean properties for non-nullable table columns.
              // TODO: Handle non-existing table columns for required bean properties.
              beanPropertyToTableColumnResolver()
                .resolve(property)
                .ifPresent(column -> {

                  Object columnValue = getValue(column);
                  Object postProcessedColumnValue = postProcess(property, columnValue);

                  property.setValue(postProcessedColumnValue);
                });
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

  /**
   * Post processes the {@link Object value} accessed from the {@link Column} with respect to the given,
   * required {@link Property}.
   *
   * @param beanProperty {@link Bean} {@link Property} used to process the {@link Column} {@link Object value}.
   * @param columnValue {@link Object value} retrieved from the {@link Table} {@link Column}.
   * @return the processed {@link Column} {@link Object value}.
   * @see org.cp.elements.beans.model.Property
   */
  protected @Nullable Object postProcess(@NotNull Property beanProperty, @Nullable Object columnValue) {
    // TODO: Handle table column value to bean property type conversions.
    return columnValue;
  }

  /**
   * Post processes the {@link Object value} accessed from the {@link Bean} {@link Property} with respect to
   * the given, required {@link Table} {@link Column}.
   *
   * @param column {@link Table} {@link Column} used to process the {@link Bean} {@link Property} {@link Object value}.
   * @param propertyValue {@link Object value} retrieved from the {@link Bean} {@link Property}.
   * @return the processed {@link Column} {@link Object value}.
   * @see org.cp.elements.data.struct.tabular.Column
   */
  protected @Nullable Object postProcess(@NotNull Column<?> column, @Nullable Object propertyValue) {
    // TODO: Handle bean property value to table column type conversions.
    return propertyValue;
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

            // TODO: Handle missing bean properties for non-nullable table columns.
            // TODO: Handle non-existing table columns for required bean properties.
            beanPropertyToTableColumnResolver()
              .resolve(property)
              .ifPresent(column -> {

                Object propertyValue = property.getValue();
                Object postProcessedPropertyValue = postProcess(column, propertyValue);

                setValue(column, postProcessedPropertyValue);
              });
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
