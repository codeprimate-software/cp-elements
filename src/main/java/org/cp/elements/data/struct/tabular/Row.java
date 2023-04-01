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
import static org.cp.elements.lang.RuntimeExceptionsFactory.newIndexOutOfBoundsException;

import java.lang.reflect.Constructor;
import java.util.Optional;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Integers;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Abstract Data Type (ADT) modeling a row in a tabular data structure.
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
   * Returns the {@link Object value} stored in the {@link Column} at the given {@link Integer column index}
   * in this {@link Row}.
   *
   * @param <T> {@link Class type} of {@link Object values} stored in the {@link Column}
   * at the given {@link Integer column index} in this {@link Row}.
   * @param columnIndex {@link Integer index} of the {@link Column} in this {@link Row}
   * from which to get the {@link Object value}.
   * @return the {@link Object value} stored in the {@link Column} at the given {@link Integer column index}
   * in this {@link Row}.
   * @throws IndexOutOfBoundsException if the given {@link Integer column index} is out of bounds.
   * The {@link Integer column index} must be greater than equal to {@literal 0}.
   * @see #values()
   */
  @SuppressWarnings("unchecked")
  default @Nullable <T> T getValue(int columnIndex) {

    Assert.isTrue(columnIndex > -1,
      newIndexOutOfBoundsException("Column index [%1$d] for Row [%2$d] must be greater than equal to 0",
        columnIndex, index()));

    return (T) values()[columnIndex];
  }

  /**
   * Returns the {@link Object value} for the given {@link String named} {@link Column} in this {@link Row}.
   *
   * This {@link Row} must be associated with a {@link View} in order to get the {@link Object value}
   * of the {@link Column} by {@link String name}.
   *
   * @param <T> {@link Class type} of {@link Object values} stored in the {@link String named} {@link Column}
   * in this {@link Row}.
   * @param columnName {@link String} containing the {@literal name} of the {@link Column}.
   * @return the {@link Object value} for the given {@link String named} {@link Column} in this {@link Row}.
   * @throws IllegalStateException if this {@link Row} is not associated with a {@link View}.
   * @throws IndexOutOfBoundsException if the {@link Integer index} of the {@link String named} {@link Column}
   * is out of bounds.
   * @see org.cp.elements.data.struct.tabular.View#indexOf(String)
   * @see #getValue(int)
   * @see #getView()
   */
  default @Nullable <T> T getValue(@NotNull String columnName) {

    return getView()
      .map(view -> view.indexOf(columnName))
      .<T>map(this::getValue)
      .orElseThrow(() -> newIllegalStateException("This Row [%d] is not associated with a View", index()));
  }

  /**
   * Returns the {@link Object value} for the given {@link Column} in this {@link Row}.
   *
   * @param <T> {@link Class type} of {@link Object values} stored in the {@link Column} in this {@link Row}.
   * @param column {@link Column} in this {@link Row}.
   * @return the {@link Object value} for the given {@link Column} in this {@link Row}.
   * @throws IllegalArgumentException if the given {@link Column} is {@literal null} or the {@link Column}
   * is not a {@link String named} {@link Column} of this {@link Row}.
   * @throws IllegalStateException if this {@link Row} is not associated with a {@link View}.
   * @throws IndexOutOfBoundsException if the {@link Integer index} of the given {@link Column} is out of bounds.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see #getValue(String)
   */
  default @Nullable <T> T getValue(@NotNull Column<?> column) {

    return Optional.ofNullable(column)
      .map(Column::getName)
      .<T>map(this::getValue)
      .orElseThrow(() -> newIllegalArgumentException("[%s] is not a Column in this Row [%d]", column, index()));
  }

  /**
   * Returns an {@link Optional} {@link View} to which this {@link Row} is associated.
   *
   * Returns {@link Optional#empty()} by default. In other words, this {@link Row}
   * is not automatically associated with a {@link View}.
   *
   * @return an {@link Optional} {@link View} containing this {@link Row}.
   * @see org.cp.elements.data.struct.tabular.View
   * @see java.util.Optional
   */
  default Optional<View> getView() {
    return Optional.empty();
  }

  /**
   * Sets the {@link Object value} for the {@link Column} at the given {@link Integer index} in this {@link Row}.
   *
   * @param <T> {@link Class type} of {@link Object values} stored in the {@link Column} in this {@link Row}.
   * @param columnIndex {@link Integer index} of the {@link Column} in this {@link Row} in which to set
   * the given {@link Object value}.
   * @param value {@link Object value} to set at the given {@link Integer column index} in this {@link Row}.
   * @return the current {@link Object value} for the {@link Column} at the given {@link Integer index}
   * in this {@link Row}.
   * @throws IndexOutOfBoundsException if the given {@link Integer column index} is out of bounds.
   */
  <T> T setValue(int columnIndex, T value);

  /**
   * Sets the {@link Object value} for the given {@link String named} {@link Column} in this {@link Row}.
   *
   * @param <T> {@link Class type} of {@link Object values} stored in the {@link String named} {@link Column}
   * in this {@link Row}.
   * @param columnName {@link String} containing the {@literal name} of the {@link Column}.
   * @param value {@link Object value} to set for the given {@link String named} {@link Column} in this {@link Row}.
   * @return the current {@link Object value} for the {@link String named} {@link Column} in this {@link Row}.
   * @throws IllegalStateException if this {@link Row} is not associated with a {@link View}.
   * @throws IndexOutOfBoundsException if the {@link Integer column index} is out of bounds.
   * @see org.cp.elements.data.struct.tabular.View#indexOf(String)
   * @see #setValue(int, Object)
   * @see #getView()
   */
  default @Nullable <T> T setValue(@NotNull String columnName, @Nullable T value) {

    return getView()
      .map(view -> view.indexOf(columnName))
      .filter(index -> index > -1)
      .map(index -> this.setValue(index, value))
      .orElseThrow(() -> newIllegalStateException("Row [%d] is not associated with a View", index()));
  }

  /**
   * Sets the {@link Object value} for the given {@link Column} in this {@link Row}.
   *
   * @param <T> {@link Class type} of {@link Object values} stored in the {@link Column} in this {@link Row}.
   * @param column {@link Column} in this {@link Row}.
   * @param value {@link Object value} to set for the given {@link Column} in this {@link Row}.
   * @return the current {@link Object value} for the given {@link Column} in this {@link Row}.
   * @throws IllegalArgumentException if the {@link Column} is not a {@link Column} in this {@link Row}.
   * @throws IllegalStateException if this {@link Row} is not associated with a {@link View}.
   * @throws IndexOutOfBoundsException if the {@link Integer column index} is out of bounds.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see #setValue(String, Object)
   */
  default @Nullable <T> T setValue(@NotNull Column<?> column, @Nullable T value) {

    return Optional.ofNullable(column)
      .map(Column::getName)
      .filter(StringUtils::hasText)
      .map(columnName -> this.setValue(columnName, value))
      .orElseThrow(() -> newIllegalArgumentException("[%s] is not a Column in this Row", column));
  }

  /**
   * Returns the {@link Integer index} of this {@link Row} in the associated {@link View}.
   *
   * @return the {@link Integer index} of this {@link Row} in the associated {@link View},
   * or {@literal -1} if this {@link Row} is not currently contained in a {@link View}.
   * @see org.cp.elements.data.struct.tabular.View#indexOf(Row)
   * @see #getView()
   */
  default int index() {

    return getView()
      .map(view -> view.indexOf(this))
      .orElse(Integers.MINUS_ONE);
  }

  /**
   * Maps the {@link Object values} of this {@link Row} to an {@link Object} of the given {@link Class type}.
   *
   * The {@link Class type} must have a default, public no argument {@link Constructor}.
   *
   * @param <T> {@link Class type} of {@link Object} to map the {@link Object values} from this {@link Row}.
   * @param type {@link Class type} of {@link Object} to map the {@link Object values} from this {@link Row}.
   * @return the {@link Object} populated with the {@link Object values} from this {@link Row}.
   * @see java.lang.Class
   * @see #store(Object)
   */
  <T> T map(Class<T> type);

  /**
   * Stores the values of the properties from the given {@link Object} in this {@link Row}.
   *
   * @param target {@link Object} to map to this {@link Row}.
   * @return this {@link Row}.
   * @see java.lang.Object
   * @see #map(Class)
   */
  Row store(Object target);

  /**
   * Returns the {@link Object values} from this {@link Row} in an array.
   *
   * @return the {@link Object values} from this {@link Row} in an array.
   */
  Object[] values();

}
