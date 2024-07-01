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

import static org.cp.elements.lang.RuntimeExceptionsFactory.newIndexOutOfBoundsException;

import java.util.Comparator;
import java.util.Optional;
import java.util.function.Predicate;

import org.cp.elements.data.struct.tabular.query.Query;
import org.cp.elements.function.FunctionUtils;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Integers;
import org.cp.elements.lang.Nameable;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.util.stream.StreamUtils;

/**
 * Abstract Data Type (ADT) defining a limited view, projection, or image of a tabular data structure.
 *
 * @author John Blum
 * @see java.lang.Iterable
 * @see java.util.function.Predicate
 * @see org.cp.elements.data.struct.tabular.AbstractView
 * @see org.cp.elements.data.struct.tabular.Column
 * @see org.cp.elements.data.struct.tabular.Row
 * @see org.cp.elements.data.struct.tabular.Table
 * @see org.cp.elements.data.struct.tabular.query.Query
 * @see org.cp.elements.lang.Nameable
 * @since 1.0.0
 */
public interface View extends Iterable<Row>, Nameable<String> {

  /**
   * Returns an {@link Iterable} iterating over the {@link Column Columns} in this {@link View}.
   *
   * @return an {@link Iterable} iterating over the {@link Column Columns} in this {@link View}.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see java.lang.Iterable
   * @see #rows()
   */
  Iterable<Column<?>> columns();

  /**
   * Determines whether this {@link View} contains the given {@link Column}.
   *
   * @param column {@link Column} being evaluated.
   * @return a boolean value indicating whether this {@link View} contains the given {@link Column}.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see #contains(String)
   */
  @NullSafe
  default boolean contains(@NotNull Column<?> column) {
    return column != null && contains(column.getName());
  }

  /**
   * Determines whether this {@link View} contains a {@link Column} with the given {@link String name}.
   *
   * @param columnName {@link String} containing the {@literal name} of the {@link Column} to evaluate.
   * @return a boolean value indicating whether this {@link View} contains a {@link Column}
   * with the given {@link String name}.
   * @see #columns()
   */
  @NullSafe
  default boolean contains(@NotNull String columnName) {

    return StringUtils.hasText(columnName)
      && StreamUtils.stream(columns()).anyMatch(column -> column.getName().equals(columnName));
  }

  /**
   * Counts the {@link Row Rows} in this {@link View} that match the given, required {@link Predicate}.
   *
   * @param predicate {@link Predicate} used to match {@link Row Rows} included in the {@link Integer count};
   * must not be {@literal null}.
   * @return a {@link Integer count} of the number of {@link Row Rows} matching the given {@link Predicate}.
   * @throws IllegalArgumentException if the given {@link Predicate} is {@literal null}.
   * @see org.cp.elements.data.struct.tabular.Row
   * @see java.util.function.Predicate
   */
  default int count(@NotNull Predicate<Row> predicate) {

    Assert.notNull(predicate, "Predicate is required");

    long count = StreamUtils.stream(rows())
      .filter(predicate)
      .count();

    return Long.valueOf(count).intValue();
  }

  /**
   * Returns the {@link Column} in this {@link View} at the given {@link Integer index}.
   *
   * @param <T> {@link Class type} of {@link Object values} stored in the {@link Column} of this {@link View}.
   * @param index {@link Integer} declaring the {@literal index} of the {@link Column} to return from this {@link View}.
   * @return the {@link Column} in this {@link View} at the given {@link Integer index}.
   * @throws IllegalArgumentException if the given {@link Integer index} is less than {@literal 0}.
   * @throws IndexOutOfBoundsException if the given {@link Integer index} is greater than
   * the number of {@link Column Columns} in this {@link View}.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see #columns()
   */
  @SuppressWarnings("unchecked")
  default @NotNull <T> Column<T> getColumn(int index) {

    Assert.isTrue(index > Integers.MINUS_ONE, () -> String.format("Column index [%d] is not valid", index));

    int count = 0;

    for (Column<?> column : columns()) {
      if (count++ == index) {
        return (Column<T>) column;
      }
    }

    throw newIndexOutOfBoundsException("Index [%1$d] is greater than the number of columns [%2$d] in this View [%3$s]",
      index, count, getName());
  }

  /**
   * Optionally returns a {@link Column} with the given {@link String name} from this {@link View}.
   *
   * @param <T> {@link Class type} of {@link Object values} stored in the {@link Column} of this {@link View}.
   * @param name {@link String} containing the {@link String name} of the {@link Column} to return
   * from this {@link View}.
   * @return an {@link Optional} {@link Column} from this {@link View} with the given {@link String name}
   * or {@link Optional#empty()} if no {@link Column} in this {@link View} with the given {@link String name} exists.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see java.util.Optional
   * @see #columns()
   */
  @SuppressWarnings("unchecked")
  default <T> Optional<Column<T>> getColumn(@NotNull String name) {

    return StreamUtils.stream(columns())
      .filter(column -> column.getName().equals(name))
      .map(column -> (Column<T>) column)
      .findFirst();
  }

  /**
   * Returns the {@link Row} in this {@link View} at the given {@link Integer index}.
   *
   * @param index {@link Integer} declaring the {@literal index} of the {@link Row} to return from this {@link View}.
   * @return the {@link Row} in this {@link View} at the given {@link Integer index}.
   * @throws IllegalArgumentException if the given {@link Integer index} is less than {@literal 0}.
   * @throws IndexOutOfBoundsException if given {@link Integer index} is greater than the number of {@link Row Rows}
   * in this {@link View}.
   * @see org.cp.elements.data.struct.tabular.Row
   * @see #rows()
   * @see #size()
   */
  default @NotNull Row getRow(int index) {

    int size = size();

    Assert.isTrue(index > Integers.MINUS_ONE && index < size,
      () -> String.format("Row index [%1$d] is not valid; Row index must be greater than [-1] and less than [%2$d]",
        index, size));

    int count = 0;

    for (Row row : rows()) {
      if (count++ == index) {
        return row;
      }
    }

    throw newIndexOutOfBoundsException("Row index [%1$d] is greater than the number of rows [%2$d] in this View [%3$s]",
      index, count, getName());
  }

  /**
   * Optionally return the first {@link Row} in this {@link View} matching the given, required {@link Predicate}.
   *
   * @param predicate {@link Predicate} defining criteria used to match the {@link Row}; must not be {@literal null}.
   * @return the first {@link Row} in this {@link View} matching the given {@link Predicate}
   * or {@link Optional#empty()} if no {@link Row} in this {@link View} matches the given {@link Predicate}
   * or {@link Optional#empty()} if the given {@link Predicate} is {@literal null}.
   * @see org.cp.elements.data.struct.tabular.Row
   * @see java.util.function.Predicate
   * @see java.util.Optional
   * @see #rows()
   */
  default Optional<Row> getRow(@NotNull Predicate<Row> predicate) {

    Predicate<Row> resolvedPredicate = FunctionUtils.nullSafePredicateMatchingNone(predicate);

    return StreamUtils.stream(rows())
      .filter(resolvedPredicate)
      .findFirst();
  }

  /**
   * Returns the {@link Object value} in this {@link View} at the given {@link Integer row index}
   * and {@link Integer column index}.
   *
   * @param <T> {@link Class type} of the {@link Object value}.
   * @param rowIndex {@link Integer} declaring the {@literal index} of a {@link Row} in this {@link View}.
   * @param columnIndex {@link Integer} declaring the {@literal index} of a {@link Column} in this {@link View}.
   * @return the {@link Object value} in this {@link View} at the given {@link Integer row index}
   * and {@link Integer column index}.
   * @throws IllegalArgumentException if the given {@link Integer row index} or {@link Integer column index}
   * are less than {@literal 0}.
   * @throws IndexOutOfBoundsException if the {@link Integer row index} or {@link Integer column index}
   * are not valid indexes in this tabular {@link View}.
   * @see org.cp.elements.data.struct.tabular.Row#getValue(int)
   * @see #getRow(int)
   */
  default @Nullable <T> T getValue(int rowIndex, int columnIndex) {
    return getRow(rowIndex).getValue(columnIndex);
  }

  /**
   * Returns the {@link Object value} in this {@link View} at the given {@link Integer row index}
   * and {@link Column} with the given {@link String name}.
   *
   * @param <T> {@link Class type} of the {@link Object value}.
   * @param rowIndex {@link Integer} declaring the {@literal index} of a {@link Row} in this {@link View}.
   * @param columnName {@link String} containing the {@literal name} of the {@link Column} from this {@link View}
   * in which the {@link Object value} will be returned.
   * @return the {@link Object value} in this {@link View} at the given {@link Integer row index}
   * and {@link Column} with the given {@link String name}.
   * @throws IllegalArgumentException if the given {@link Integer row index} is less than {@literal 0}
   * or a {@link Column} with {@link String name} does not exist in this {@link View}.
   * @throws IndexOutOfBoundsException if the {@link Integer row index} is not valid index
   * in this tabular {@link View}.
   * @see #getValue(int, int)
   * @see #indexOf(String)
   */
  default @Nullable <T> T getValue(int rowIndex, @NotNull String columnName) {

    int columnIndex = indexOf(columnName);

    Assert.isTrue(columnIndex > Integers.MINUS_ONE,
      () -> String.format("Column with name [%1$s] does not exist in this View [%2$s]", columnName, getName()));

    return getValue(rowIndex, columnIndex);
  }

  /**
   * Returns the {@link Object value} in this {@link View} at the given {@link Integer row index} and {@link Column}.
   *
   * @param <T> {@link Class type} of the {@link Object value}.
   * @param rowIndex {@link Integer} declaring the {@literal index} of a {@link Row} in this {@link View}.
   * @param column {@link Column} in this {@link View} from which to get the {@link Object value}.
   * @return the {@link Object value} in this {@link View} at the given {@link Integer row index} and {@link Column}.
   * @throws IllegalArgumentException if the given {@link Integer row index} is less than {@literal 0}
   * or the given {@link Column} does not exist in this {@link View}.
   * @throws IndexOutOfBoundsException if the {@link Integer row index} is not a valid index
   * in this tabular {@link View}.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see #getValue(int, int)
   * @see #indexOf(Column)
   */
  default @Nullable <T> T getValue(int rowIndex, @NotNull Column<?> column) {

    int columnIndex = indexOf(column);

    Assert.isTrue(columnIndex > Integers.MINUS_ONE,
      () -> String.format("Column [%1$s] does not exist in this View [%2$s]", column, getName()));

    return getValue(rowIndex, columnIndex);
  }

  /**
   * Determines the {@link Integer index} of the given {@link Column} in this {@link View} if present.
   * <p>
   * The first {@link Column} is at {@link Integer index} {@literal 0}.
   *
   * @param column {@link Column} to evaluate.
   * @return the {@link Integer index} of the given {@link Column} in this {@link View},
   * or {@literal -1} if the {@link Column} is not contained in this {@link View}.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see #indexOf(String)
   */
  @NullSafe
  default int indexOf(@NotNull Column<?> column) {
    return column != null ? indexOf(column.getName()) : Integers.MINUS_ONE;
  }

  /**
   * Determines the {@link Integer index} of a {@link Column} with the given {@link String name} in this {@link View}
   * if present.
   * <p>
   * The first {@link Column} is at {@link Integer index} {@literal 0}.
   *
   * @param columnName {@link String} containing the {@literal name} of the {@link Column} to index.
   * @return the {@link Integer index} of a {@link Column} with the given {@link String name} in this {@link View},
   * or {@literal -1} if a {@link Column} with {@link String name} is not contained in this {@link View}.
   * @see #columns()
   */
  @NullSafe
  default int indexOf(@NotNull String columnName) {

    int index = 0;

    for (Column<?> column : columns()) {
      if (column.getName().equals(columnName)) {
        return index;
      }

      index++;
    }

    return Integers.MINUS_ONE;
  }

  /**
   * Determines the {@link Integer index} of the given {@link Row} in this {@link View} if present.
   *
   * @param row {@link Row} to evaluate.
   * @return the {@link Integer index} of the given {@link Row} in this {@link View},
   * or {@literal -1} if the {@link Row} is not contained in this {@link View}.
   * @see org.cp.elements.data.struct.tabular.Row
   * @see #rows()
   */
  @NullSafe
  default int indexOf(@NotNull Row row) {

    int index = 0;

    for (Row viewRow : rows()) {
      if (viewRow.equals(row)) {
        return index;
      }

      index++;
    }

    return Integers.MINUS_ONE;
  }

  /**
   * Determines whether this {@link View} is {@literal empty}, or whether this {@link View}
   * contains any {@link Row Rows}.
   * <p>
   * By default, this {@link View} is considered {@literal empty} if {@link #size()} is {@literal 0}.
   *
   * @return a boolean value indicating whether this {@link View} contains any {@link Row Rows}.
   * @see #size()
   */
  default boolean isEmpty() {
    return size() < Integers.ONE;
  }

  /**
   * Queries this {@link View} with the given {@link Query} and returns a new {@link View} from the result set.
   * <p>
   * The {@link Query} matches {@link Row Rows}, or {@literal data} contained in this {@link View}
   * defined by a {@link Predicate}, ordered/sorted by a {@link Comparator} and projected using an array
   * or {@link Iterable} of {@link Column Columns}.
   *
   * @param query {@link Query} object defining the {@literal query} on this {@link View};
   * must not be {@literal null}.
   * @return a new {@link View} from the result set produced by the given {@link Query}.
   * @throws IllegalArgumentException if the given {@link Query} is {@literal null}.
   * @see org.cp.elements.data.struct.tabular.query.Query
   * @see org.cp.elements.data.struct.tabular.View
   */
  default @NotNull View query(@NotNull Query query) {

    Assert.notNull(query, "Query is required");

    return query.from(this).execute();
  }

  /**
   * Returns an {@link Iterable} iterating over the {@link Row Rows} in this {@link View}.
   *
   * @return an {@link Iterable} iterating over the {@link Row Rows} in this {@link View}.
   * @see org.cp.elements.data.struct.tabular.Row
   * @see java.lang.Iterable
   * @see #columns()
   */
  default Iterable<Row> rows() {
    return this;
  }

  /**
   * Returns the {@link Integer number} of {@link Row Rows} in this {@link View}.
   *
   * @return the {@link Integer number} of {@link Row Rows} in this {@link View}.
   * @see #count(Predicate)
   */
  default int size() {
    return count(row -> true);
  }
}
