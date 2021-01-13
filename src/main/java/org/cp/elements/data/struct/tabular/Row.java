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

import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalArgumentException;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalStateException;

import java.lang.reflect.Constructor;
import java.util.Optional;

/**
 * The {@link Row} interface is an Abstract Data Type (ADT) defining a row in a tabular data structure.
 *
 * @author John J. Blum
 * @see org.cp.elements.data.struct.tabular.Column
 * @see org.cp.elements.data.struct.tabular.Table
 * @see org.cp.elements.data.struct.tabular.View
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Row {

  /**
   * Returns the {@link Object value} at the given {@link Integer column index} in this {@link Row}.
   *
   * @param <T> {@link Class type} of the {@link Column} {@link Object value} at the given {@link Integer column index}
   * in this {@link Row}.
   * @param columnIndex {@link Integer index} of the {@link Column} from which to get the {@link Object value}.
   * @throws IndexOutOfBoundsException if the {@link Integer column index} is out of bounds.
   * @return the {@link Object value} at the given {@link Integer column index} in this {@link Row}.
   */
  <T> T getValue(int columnIndex);

  /**
   * Returns the {@link Object value} for the given {@link String named} {@link Column} in this {@link Row}.
   *
   * This {@link Row} must be associated with a {@link View} in order to get the {@link Object value}
   * by {@link Column} {@link String name}.
   *
   * @param <T> {@link Class type} of the {@link Object value} at the {@link String named} {@link Column}
   * in this {@link Row}.
   * @param columnName {@link String} containing the name of the {@link Column}.
   * @return the {@link Object value} for the given {@link String named} {@link Column}.
   * @throws IllegalStateException if this {@link Row} is not associated with a {@link View}.
   * @throws IndexOutOfBoundsException if the index of the {@link String named} {@link Column} is out of bounds.
   * @see org.cp.elements.data.struct.tabular.View#indexOf(String)
   * @see #getValue(int)
   * @see #getView()
   */
  default <T> T getValue(String columnName) {

    return getView()
      .map(view -> view.indexOf(columnName))
      .<T>map(this::getValue)
      .orElseThrow(() -> newIllegalStateException("This Row is not associated with a View"));
  }

  /**
   * Returns the {@link Object value} for the given {@link Column} in this {@link Row}.
   *
   * @param <T> {@link Class type} of the {@link Object value} at the {@link Column} in this {@link Row}.
   * @param column {@link Column} in this {@link Row}.
   * @return the {@link Object value} for the given {@link Column}.
   * @throws IllegalArgumentException if the {@link Column} is {@literal null} or the {@link Column}
   * is not a valid {@link String named} {@link Column} of this {@link Row}.
   * @throws IllegalStateException if this {@link Row} is not associated with a {@link View}.
   * @throws IndexOutOfBoundsException if the index of the given {@link Column} is out of bounds.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see #getValue(String)
   */
  default <T> T getValue(Column column) {

    return Optional.ofNullable(column)
      .map(Column::getName)
      .<T>map(this::getValue)
      .orElseThrow(() -> newIllegalArgumentException("[%s] is not a valid Column in this Row", column));
  }

  /**
   * Returns an {@link Optional} {@link View} to which this {@link Row} is associated.
   *
   * @return an {@link Optional} {@link View} containing this {@link Row}.
   * @see org.cp.elements.data.struct.tabular.View
   * @see java.util.Optional
   */
  Optional<View> getView();

  /**
   * Sets the {@link Object value} of the {@link Column} at the given {@link Integer index} in this {@link Row}.
   *
   * @param <T> {@link Class type} of the {@link Object value} at the {@link Column} in this {@link Row}.
   * @param columnIndex {@link Integer index} of the {@link Column} to set the given {@link Object value}.
   * @param value {@link Object value} to set at the given {@link Integer column index} in this {@link Row}.
   * @return the current {@link Object value} of the {@link Column} at the given {@link Integer index}
   * in this {@link Row}.
   * @throws IndexOutOfBoundsException if the {@link Integer column index} is out of bounds.
   */
  <T> T setValue(int columnIndex, T value);

  /**
   * Sets the {@link Object value} of the {@link String named} {@link Column} in this {@link Row}.
   *
   * @param <T> {@link Class type} of the {@link Object value} at the {@link String named} {@link Column}
   * in this {@link Row}.
   * @param columnName {@link String} containing the name of the {@link Column}.
   * @param value {@link Object value} to set for the given {@link String named} {@link Column}.
   * @return the current {@link Object value} of the {@link String named} {@link Column}.
   * @throws IllegalStateException if this {@link Row} is not associated with a {@link View}.
   * @throws IndexOutOfBoundsException if the {@link Integer column index} is out of bounds.
   * @see org.cp.elements.data.struct.tabular.View#indexOf(String)
   * @see #setValue(int, Object)
   * @see #getView()
   */
  default <T> T setValue(String columnName, T value) {

    return getView()
      .map(view -> view.indexOf(columnName))
      .map(index -> this.setValue(index, value))
      .orElseThrow(() -> newIllegalStateException("This Row is not associated with a View"));
  }

  /**
   * Sets the {@link Object value} of the given {@link Column} in this {@link Row}.
   *
   * @param <T> {@link Class type} of the {@link Object value} at the {@link Column} in this {@link Row}.
   * @param column {@link Column} in this {@link Row}.
   * @param value {@link Object value} to set for the given {@link Column}.
   * @return the current {@link Object value} for the given {@link Column} in this {@link Row}.
   * @throws IllegalArgumentException if the {@link Column} is not a valid {@link Column} in this {@link Row}.
   * @throws IllegalStateException if this {@link Row} is not associated with a {@link View}.
   * @throws IndexOutOfBoundsException if the {@link Integer column index} is out of bounds.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see #setValue(String, Object)
   */
  default <T> T setValue(Column column, T value) {

    return Optional.ofNullable(column)
      .map(Column::getName)
      .map(columnName -> this.setValue(columnName, value))
      .orElseThrow(() -> newIllegalArgumentException("[%s] is not a valid Column in this Row", column));
  }

  /**
   * Returns the {@link Integer index} of this {@link Row} in the associated {@link View}.
   *
   * @return the {@link Integer index} of this {@link Row} in the associated {@link View},
   * or a {@literal -1} this {@link Row} is not currently contained by a {@link View}.
   * @see org.cp.elements.data.struct.tabular.View#indexOf(Row)
   * @see #getView()
   */
  default int index() {

    return getView()
      .map(view -> view.indexOf(this))
      .orElse(-1);
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
  <T> T map(Class<T> type);

  /**
   * Returns the {@link Object values} of this {@link Row} in an array.
   *
   * @return the {@link Object values} of this {@link Row} in an array.
   */
  Object[] values();

}
