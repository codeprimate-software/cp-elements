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
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NullSafe;

/**
 * The {@link Table} interface is an Abstract Data Type (ADT) modeling a tabular data structure.
 *
 * @author John J. Blum
 * @see org.cp.elements.data.struct.tabular.Column
 * @see org.cp.elements.data.struct.tabular.Row
 * @see org.cp.elements.data.struct.tabular.View
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Table extends View {

  /**
   * Adds the given {@link Column} to this {@link Table}.
   *
   * @param column {@link Column} to add.
   * @return a boolean value indicating whether the added {@link Column}
   * successfully modified the structure of this {@link Table}.
   * @see org.cp.elements.data.struct.tabular.Column
   */
  boolean add(Column column);

  /**
   * Adds the given {@link Row} to this {@link Table}.
   *
   * @param row {@link Row} to add.
   * @return a boolean value indicating whether the added {@link Row}
   * successfully modified the structure of this {@link Table}.
   * @see org.cp.elements.data.struct.tabular.Row
   */
  boolean add(Row row);

  /**
   * Removes the {@link Column} at the given {@link Integer index} from this {@link Table}.
   *
   * @param index {@link Integer} value indicating the index of the {@link Column} to remove.
   * @return a boolean value indicating whether the {@link Column} at {@link Integer index}
   * was successfully removed.
   * @throws IndexOutOfBoundsException if the given {@link Integer index} is not a valid {@link Column} index
   * in this {@link Table}.
   */
  boolean removeColumn(int index);

  /**
   * Removes the {@link Column} with the given {@link String name} from this {@link Table}.
   *
   * @param name {@link String} containing the name of the {@link Column} to remove.
   * @return a boolean value indicating whether the given {@link Column} with {@link String name}
   * was successfully removed.
   * the {@link Column} {@link String name} is out of bounds.
   * @see #indexOf(String)
   * @see #removeColumn(int)
   */
  @NullSafe
  default boolean removeColumn(String name) {

    return Optional.of(indexOf(name))
      .filter(index -> index > -1)
      .map(this::removeColumn)
      .orElse(false);
  }

  /**
   * Removes the given {@link Column} from this {@link Table}.
   *
   * @param column {@link Column} to remove.
   * @return a boolean value indicating whether the given {@link Column} was successfully removed.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see #indexOf(Column)
   * @see #removeColumn(int)
   */
  @NullSafe
  default boolean remove(Column column) {

    return Optional.of(indexOf(column))
      .filter(index ->  index > -1)
      .map(this::removeColumn)
      .orElse(false);
  }

  /**
   * Removes the {@link Row} at the given {@link Integer index} from this {@link Table}.
   *
   * @param index {@link Integer} value indicating the index of the {@link Row} to remove.
   * @return a boolean value indicating whether the given {@link Row} at {@link Integer index}
   * was successfully removed.
   */
  boolean removeRow(int index);

  /**
   * Removes all {@link Row Rows} from this {@link Table} matching the given {@link Predicate}.
   *
   * @param predicate {@link Predicate} used to match {@link Row Rows} to remove from this {@link Table}.
   * @return a boolean value indicating whether the remove operation modified the structure of this {@link Table}.
   * @throws IllegalArgumentException if {@link Predicate} is {@literal null}.
   * @throws IllegalStateException if the removal of a {@link Row} matching the {@link Predicate}
   * was not successful.
   * @see java.util.function.Predicate
   * @see #remove(Row)
   * @see #rows()
   */
  default boolean removeRows(Predicate<Row> predicate) {

    Assert.notNull(predicate, "Predicate is required");

    AtomicBoolean result = new AtomicBoolean(false);

    for (Iterator<Row> rows = rows().iterator(); rows.hasNext(); ) {

      Optional.of(rows.next())
        .filter(predicate)
        .ifPresent(row -> {
          rows.remove();
          result.set(true);
        });
    }

    return result.get();
  }

  /**
   * Removes the given {@link Row} from this {@link Table}.
   *
   * @param row {@link Row} to remove.
   * @return a boolean value indicating whether the given {@link Row} was successfully removed.
   * @see org.cp.elements.data.struct.tabular.Row
   * @see #indexOf(Row)
   * @see #removeRow(int)
   */
  @NullSafe
  default boolean remove(Row row) {

    return Optional.ofNullable(row)
      .map(this::indexOf)
      .filter(index -> index > -1)
      .map(this::removeRow)
      .orElse(false);
  }

  /**
   * Sets the {@link Object value} at the given {@link Row} and {@link Column} {@link Integer indexes}
   * in this {@link Table}.
   *
   * @param rowIndex {@link Integer} value indicating the {@link Row} index.
   * @param columnIndex {@link Integer} value indicating the {@link Column} index.
   * @param value the {@link Object value} to set at the referenced {@link Row} and {@link Column}
   * in this {@link Table}.
   * @return the current {@link Object} value at the given {@link Row} and {@link Column}.
   * @throws IndexOutOfBoundsException if either the {@link Row} or {@link Column} {@link Integer index}
   * are invalid.
   * @see org.cp.elements.data.struct.tabular.Row#setValue(int, Object)
   * @see #getRow(int)
   */
  default Object setValue(int rowIndex, int columnIndex, Object value) {
    return getRow(rowIndex).setValue(columnIndex, value);
  }

  /**
   * Sets the {@link Object value} at the given {@link Row} {@link Integer index}
   * and {@link String named} {@link Column} in this {@link Table}.
   *
   * @param rowIndex {@link Integer} value indicating the {@link Row} index.
   * @param columnName {@link String} containing the name of the {@link Column}.
   * @param value the {@link Object value} to set at the referenced {@link Row} and {@link Column}
   * in this {@link Table}.
   * @return the current {@link Object} value at the given {@link Row} and {@link Column}.
   * @throws IllegalArgumentException if the {@link String named} {@link Column}
   * is not a valid {@link Column} in this {@link Table}.
   * @throws IndexOutOfBoundsException if the {@link Row} {@link Integer index} is not valid.
   * @see #setValue(int, int, Object)
   * @see #indexOf(String)
   */
  @NullSafe
  default Object setValue(int rowIndex, String columnName, Object value) {

    return Optional.ofNullable(columnName)
      .filter(StringUtils::hasText)
      .map(this::indexOf)
      .filter(columnIndex -> columnIndex > -1)
      .map(columnIndex -> setValue(rowIndex, columnIndex, value))
      .orElseThrow(() -> newIllegalArgumentException("Column [%s] is not valid", columnName));
  }

  /**
   * Sets the {@link Object value} at the given {@link Row} {@link Integer index} and {@link Column}
   * in this {@link Table}.
   *
   * @param rowIndex {@link Integer} value indicating the {@link Row} index.
   * @param column {@link Column} in this {@link Table}.
   * @param value the {@link Object value} to set at the referenced {@link Row} and {@link Column}
   * in this {@link Table}.
   * @return the current {@link Object} value at the given {@link Row} and {@link Column}.
   * @throws IllegalArgumentException if {@link Column} is {@literal null}
   * or is not a valid {@link Column} in this {@link Table}.
   * @throws IndexOutOfBoundsException if the {@link Row} {@link Integer index} is not valid.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see #setValue(int, int, Object)
   * @see #indexOf(Column)
   */
  @NullSafe
  default Object setValue(int rowIndex, Column column, Object value) {

    return Optional.ofNullable(column)
      .map(this::indexOf)
      .filter(columnIndex -> columnIndex > -1)
      .map(columnIndex -> setValue(rowIndex, columnIndex, value))
      .orElseThrow(() -> newIllegalArgumentException("Column [%s] is not valid", column));
  }
}
