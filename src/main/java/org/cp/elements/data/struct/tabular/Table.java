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

import java.util.Iterator;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Predicate;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Integers;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Abstract Data Type (ADT) modeling a {@literal tabular}, or {@literal table} data structure,
 * containing {@literal rows} and {@literal columns}.
 *
 * @author John J. Blum
 * @see java.util.function.Predicate
 * @see org.cp.elements.data.struct.tabular.Column
 * @see org.cp.elements.data.struct.tabular.Row
 * @see org.cp.elements.data.struct.tabular.View
 * @since 1.0.0
 */
@SuppressWarnings({ "rawtypes", "unused" })
public interface Table extends View {

  /**
   * Adds the given {@link Column} to this {@link Table}.
   *
   * @param column {@link Column} to add.
   * @return a boolean value indicating whether the added {@link Column}
   * successfully modified the structure of this {@link Table}.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see #add(Row)
   */
  boolean add(Column column);

  /**
   * Adds the given {@link Row} to this {@link Table}.
   *
   * @param row {@link Row} to add.
   * @return a boolean value indicating whether the added {@link Row}
   * successfully modified the structure of this {@link Table}.
   * @see org.cp.elements.data.struct.tabular.Row
   * @see #add(Column)
   */
  boolean add(Row row);

  /**
   * Removes the {@link Column} at the given {@link Integer index} from this {@link Table}.
   *
   * @param index {@link Integer value} specifying the {@literal index} of the {@link Column} to remove.
   * @return a boolean value indicating whether the {@link Column} at {@link Integer index}
   * was successfully removed from this {@link Table}.
   * @throws IndexOutOfBoundsException if the given {@link Integer index} is not a valid {@link Column}
   * {@literal index} in this {@link Table}.
   * @see #removeColumn(String)
   * @see #remove(Column)
   */
  default boolean removeColumn(int index) {

    int columnIndex = 0;

    for (Iterator<Column<?>> columnIterator = columns().iterator(); columnIterator.hasNext(); ) {

      columnIterator.next();

      if (columnIndex++ == index) {
        columnIterator.remove();
        return true;
      }
    }

    return false;
  }

  /**
   * Removes the {@link Column} with the given {@link String name} from this {@link Table}.
   *
   * @param name {@link String} containing the {@literal name} of the {@link Column} to remove.
   * @return a boolean value indicating whether the given {@link Column} with {@link String name}
   * was successfully removed from this {@link Table}.
   * @see #indexOf(String)
   * @see #removeColumn(int)
   * @see #remove(Column)
   */
  @NullSafe
  default boolean removeColumn(@NotNull String name) {

    int columnIndex = indexOf(name);

    return columnIndex > Integers.MINUS_ONE && removeColumn(columnIndex);
  }

  /**
   * Removes the given {@link Column} from this {@link Table}.
   *
   * @param column {@link Column} to remove.
   * @return a boolean value indicating whether the given {@link Column}
   * was successfully removed from this {@link Table}.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see #removeColumn(String)
   * @see #removeColumn(int)
   * @see #indexOf(Column)
   */
  @NullSafe
  default boolean remove(@NotNull Column column) {

    int columnIndex = indexOf(column);

    return columnIndex > Integers.MINUS_ONE && removeColumn(columnIndex);
  }

  /**
   * Removes the given {@link Row} from this {@link Table}.
   *
   * @param row {@link Row} to remove.
   * @return a boolean value indicating whether the given {@link Row}
   * was successfully removed from this {@link Table}.
   * @see org.cp.elements.data.struct.tabular.Row
   * @see #removeRows(Predicate)
   * @see #removeRow(int)
   * @see #indexOf(Row)
   */
  @NullSafe
  default boolean remove(@NotNull Row row) {

    int rowIndex = indexOf(row);

    return rowIndex > Integers.MINUS_ONE && removeRow(rowIndex);
  }

  /**
   * Removes the {@link Row} at the given {@link Integer index} from this {@link Table}.
   *
   * @param index {@link Integer value} specifying the {@literal index} of the {@link Row}
   * to remove from this {@link Table}.
   * @return a boolean value indicating whether the given {@link Row} at {@link Integer index}
   * was successfully removed from this {@link Table}.
   * @see #removeRows(Predicate)
   * @see #remove(Row)
   */
  boolean removeRow(int index);

  /**
   * Removes all {@link Row Rows} from this {@link Table} matching the given {@link Predicate}.
   *
   * @param predicate {@link Predicate} used to match {@link Row Rows} to remove from this {@link Table};
   * must not be {@literal null}.
   * @return a boolean value indicating whether the remove operation resulted in removing {@link Row Rows}
   * and modified the structure of this {@link Table}.
   * @throws IllegalArgumentException if the given {@link Predicate} is {@literal null}.
   * @throws IllegalStateException if the removal of a {@link Row} matching the {@link Predicate} was not successful.
   * @see java.util.function.Predicate
   * @see #removeRow(int)
   * @see #remove(Row)
   * @see #rows()
   */
  default boolean removeRows(@NotNull Predicate<Row> predicate) {

    Assert.notNull(predicate, "Predicate is required");

    AtomicBoolean result = new AtomicBoolean(false);

    for (Iterator<Row> rows = rows().iterator(); rows.hasNext(); ) {

      Row row = rows.next();

      if (predicate.test(row)) {
        rows.remove();
        result.set(true);
      }
    }

    return result.get();
  }

  /**
   * Sets the {@link Object value} at the given {@link Row} and {@link Column} {@link Integer indexes}
   * in this {@link Table}.
   *
   * @param rowIndex {@link Integer value} specifying the {@link Row} {@literal index}.
   * @param columnIndex {@link Integer value} specifying the {@link Column} {@literal index}.
   * @param value {@link Object value} to set at the referenced {@link Row} and {@link Column} in this {@link Table}.
   * @return the current {@link Object value} at the given {@link Row} and {@link Column} in this {@link Table}.
   * @throws IndexOutOfBoundsException if either the {@link Row} or {@link Column} {@link Integer indexes} are invalid.
   * @see org.cp.elements.data.struct.tabular.Row#setValue(int, Object)
   * @see #setValue(int, String, Object)
   * @see #setValue(int, Column, Object)
   * @see #getRow(int)
   */
  default @Nullable Object setValue(int rowIndex, int columnIndex, @Nullable Object value) {
    return getRow(rowIndex).setValue(columnIndex, value);
  }

  /**
   * Sets the {@link Object value} at the given {@link Row} {@link Integer index}
   * and {@link String named} {@link Column} in this {@link Table}.
   *
   * @param rowIndex {@link Integer value} specifying the {@link Row} {@literal index}.
   * @param columnName {@link String} containing the {@literal name} of the {@link Column}.
   * @param value {@link Object value} to set at the referenced {@link Row} and {@link Column} in this {@link Table}.
   * @return the current {@link Object value} at the given {@link Row} and {@link Column} in this {@link Table}.
   * @throws IllegalArgumentException if the {@link String named} {@link Column}
   * is not a valid {@link Column} in this {@link Table}.
   * @throws IndexOutOfBoundsException if the {@link Row} {@link Integer index} is not valid.
   * @see #setValue(int, Column, Object)
   * @see #setValue(int, int, Object)
   * @see #indexOf(String)
   */
  @NullSafe
  default @Nullable Object setValue(int rowIndex, String columnName, @Nullable Object value) {

    return Optional.ofNullable(columnName)
      .filter(StringUtils::hasText)
      .map(this::indexOf)
      .filter(columnIndex -> columnIndex > Integers.MINUS_ONE)
      .map(columnIndex -> setValue(rowIndex, columnIndex, value))
      .orElseThrow(() -> newIllegalArgumentException("Column [%s] is not valid", columnName));
  }

  /**
   * Sets the {@link Object value} at the given {@link Row} {@link Integer index} and {@link Column}
   * in this {@link Table}.
   *
   * @param rowIndex {@link Integer value} specifying the {@link Row} {@literal index}.
   * @param column {@link Column} in this {@link Table}; must not be {@literal null}.
   * @param value {@link Object value} to set at the referenced {@link Row} and {@link Column} in this {@link Table}.
   * @return the current {@link Object value} at the given {@link Row} and {@link Column} from this {@link Table}.
   * @throws IllegalArgumentException if the given {@link Column} is {@literal null}
   * or is not a valid {@link Column} in this {@link Table}.
   * @throws IndexOutOfBoundsException if the given {@link Row} {@link Integer index} is not valid.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see #setValue(int, String, Object)
   * @see #setValue(int, int, Object)
   * @see #indexOf(Column)
   */
  @NullSafe
  default @Nullable Object setValue(int rowIndex, @NotNull Column column, @Nullable Object value) {

    return Optional.ofNullable(column)
      .map(this::indexOf)
      .filter(columnIndex -> columnIndex > Integers.MINUS_ONE)
      .map(columnIndex -> setValue(rowIndex, columnIndex, value))
      .orElseThrow(() -> newIllegalArgumentException("Column [%s] is not valid", column));
  }
}
