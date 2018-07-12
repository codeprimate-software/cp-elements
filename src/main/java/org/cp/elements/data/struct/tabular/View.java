/*
 * Copyright 2016 Author or Authors.
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
import java.util.stream.StreamSupport;

import org.cp.elements.data.struct.tabular.query.Query;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Nameable;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NullSafe;

/**
 * The {@link View} interface defines a limited view (or projection), or image of a tabular data structure.
 *
 * @author John J. Blum
 * @see java.lang.Iterable
 * @see java.util.Comparator
 * @see java.util.function.Predicate
 * @see org.cp.elements.data.struct.tabular.Column
 * @see org.cp.elements.data.struct.tabular.Row
 * @see org.cp.elements.data.struct.tabular.Table
 * @see org.cp.elements.data.struct.tabular.query.Query
 * @see org.cp.elements.lang.Nameable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface View extends Iterable<Row>, Nameable<String> {

  /**
   * Returns an {@link Iterable} to iterate over the {@link Column Columns} in this {@link View}.
   *
   * @return an {@link Iterable} to iterate over the {@link Column Columns} in this {@link View}.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see java.lang.Iterable
   */
  Iterable<Column> columns();

  /**
   * Determines whether this {@link View} contains the given {@link Column}.
   *
   * @param column {@link Column} being evaluated.
   * @return a boolean value indicating whether this {@link View} contains the given {@link Column}.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see #indexOf(Column)
   * @see #contains(String)
   */
  @NullSafe
  default boolean contains(Column column) {

    return Optional.ofNullable(column)
      .map(Column::getName)
      .filter(this::contains)
      .isPresent();
  }

  /**
   * Determines whether this {@link View} contains a {@link Column} with the given {@link String name}.
   *
   * @param columnName {@link String} containing the name of the {@link Column} to evaluate.
   * @return a boolean value indicating whether this {@link View} contains a {@link Column}
   * with the given {@link String name}.
   * @see #columns()
   */
  default boolean contains(String columnName) {

    return Optional.ofNullable(columnName)
      .filter(StringUtils::hasText)
      .filter(it -> StreamSupport.stream(columns().spliterator(), false)
        .anyMatch(column -> column.getName().equals(columnName)))
      .isPresent();
  }

  /**
   * Counts the {@link Row Rows} in this {@link View} that match the given {@link Predicate}.
   *
   * @param predicate {@link Predicate} used to match {@link Row Rows}.
   * @return a count of the number of {@link Row Rows} matching the given {@link Predicate}.
   * @throws IllegalArgumentException if {@link Predicate} is {@literal null}.
   * @see org.cp.elements.data.struct.tabular.Row
   * @see java.util.function.Predicate
   */
  default int count(Predicate<Row> predicate) {

    Assert.notNull(predicate, "Predicate is required");

    return Long.valueOf(StreamSupport.stream(rows().spliterator(), false)
      .filter(predicate)
      .count()).intValue();
  }

  /**
   * Returns the {@link Column} at the given {@link Integer#TYPE index} in this {@link View}.
   *
   * @param <T> {@link Class type} of {@link Object values} stored by the {@link Column}.
   * @param index {@link Integer} specifying the index of the {@link Column} in this {@link View} to return.
   * @return the {@link Column} in this {@link View} at the specified {@link Integer#TYPE index}.
   * @throws IllegalArgumentException if index is less than {@literal 0}.
   * @throws IndexOutOfBoundsException if the index is greater than the number of {@link Column Columns}
   * in this {@link View}.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see #columns()
   */
  @SuppressWarnings("unchecked")
  default <T> Column<T> getColumn(int index) {

    Assert.isTrue(index > -1, () -> String.format("Index [%d] is not valid", index));

    int count = 0;

    for (Column column : columns()) {
      if (count++ == index) {
        return column;
      }
    }

    throw newIndexOutOfBoundsException("Index [%1$d] is greater than the number of columns [%2$d]", index, count);
  }

  /**
   * Optionally returns a {@link Column} with the given {@link String name} from this {@link View}.
   *
   * @param <T> {@link Class type} of {@link Object values} stored by the {@link Column}.
   * @param name {@link String} containing the {@link String name} of the {@link Column} in this {@link View} to return.
   * @return an {@link Optional} {@link Column} with the given {@link String} name from this {@link View}
   * or an {@link Optional#EMPTY} if no {@link Column} with the given {@link String name} in this {@link View} exists.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see java.util.Optional
   * @see #indexOf(Column)
   * @see #getColumn(int)
   */
  default <T> Optional<Column<T>> getColumn(String name) {

    return Optional.of(indexOf(name))
      .filter(index -> index > -1)
      .map(this::getColumn);
  }

  /**
   * Returns the {@link Row} in this {@link View} at the given {@link Integer#TYPE index}.
   *
   * @param index {@link Integer} specifying the index of the {@link Row} in this {@link View} to return.
   * @return the {@link Row} at the given {@link Integer#TYPE index} from this {@link View}.
   * @throws IllegalArgumentException if index is less than {@literal 0}.
   * @throws IndexOutOfBoundsException if index is greater than the number of {@link Row Rows} in this {@link View}.
   * @see org.cp.elements.data.struct.tabular.Row
   * @see #rows()
   */
  default Row getRow(int index) {

    Assert.isTrue(index > -1 && index < size(),
      () -> String.format("Index [%d] is not valid; Index must be greater than -1 and less than %d", index, size()));

    int count = 0;

    for (Row row : rows()) {
      if (count++ == index) {
        return row;
      }
    }

    throw newIndexOutOfBoundsException("Index [%1$d] is greater than the number of rows [%2$d]", index, count);
  }

  /**
   * Optionally return the first {@link Row} in this {@link View} matching the specified {@link Predicate}.
   *
   * @param predicate {@link Predicate} defining the criteria used to match the {@link Row}.
   * @return the first {@link Row} in this {@link View} matching the specified {@link Predicate}
   * or an {@link Optional#EMPTY} if no {@link Row} matches the {@link Predicate}.
   * @throws IllegalArgumentException if {@link Predicate} is {@literal null}.
   * @see org.cp.elements.data.struct.tabular.Row
   * @see java.util.function.Predicate
   * @see java.util.Optional
   * @see #rows()
   */
  default Optional<Row> getRow(Predicate<Row> predicate) {

    Assert.notNull(predicate, "Predicate is required");

    return StreamSupport.stream(rows().spliterator(), false)
      .filter(predicate)
      .findFirst();
  }

  /**
   * Returns the {@link Object value} in this {@link View} at the specified {@link Row} and {@link Column} index.
   *
   * @param <T> {@link Class type} of the value.
   * @param rowIndex integer value indicating the {@link Row} index from which to get the {@link Object value}.
   * @param columnIndex integer value indicating the {@link Column} index from wihch to get the {@link Object value}.
   * @return the {@link Object value} at the specified {@link Row} and {@link Column} index in this {@link View}.
   * @see org.cp.elements.data.struct.tabular.Row#getValue(int)
   * @see #getRow(int)
   */
  default <T> T getValue(int rowIndex, int columnIndex) {
    return getRow(rowIndex).getValue(columnIndex);
  }

  /**
   * Returns the {@link Object value} in this {@link View} at the specified {@link Row} index
   * and {@link Column} with the given {@link String name}.
   *
   * @param <T> {@link Class type} of the value.
   * @param rowIndex integer value indicating the {@link Row} index from which to get the {@link Object value}.
   * @param columnName {@link String} containing the name of the {@link Column} from which to get
   * the {@link Object value}.
   * @return the {@link Object value} at the specified {@link Row} index and {@link String named} {@link Column}
   * from this {@link View}.
   * @see #indexOf(String)
   * @see #getValue(int, int)
   */
  @NullSafe
  default <T> T getValue(int rowIndex, String columnName) {
    return getValue(rowIndex, indexOf(columnName));
  }

  /**
   * Returns the {@link Object value} at the specified {@link Row} index and {@link Column} from this {@link View}.
   *
   * @param <T> {@link Class type} of the value.
   * @param rowIndex integer value indicating the {@link Row} index from which to get the {@link Object value}.
   * @param column {@link Column} from which to get the {@link Object value}.
   * @return the {@link Object value} at the specified {@link Row} index and {@link Column} from this {@link View}.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see #indexOf(Column)
   * @see #getValue(int, int)
   */
  @NullSafe
  default <T> T getValue(int rowIndex, Column column) {
    return getValue(rowIndex, indexOf(column));
  }

  /**
   * Determines the {@link Integer#TYPE index} of the given {@link Column} in this {@link View}.
   *
   * @param column {@link Column} to evaluate.
   * @return an integer value indicating the index of the given {@link Column} in this {@link View},
   * or a {@literal -1} if the {@link Column} is not contained in this {@link View}.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see #contains(String)
   */
  @NullSafe
  default int indexOf(Column column) {

    return Optional.ofNullable(column)
      .map(Column::getName)
      .map(this::indexOf)
      .orElse(-1);
  }

  /**
   * Determines the {@link Integer#TYPE index} of {@link Column column} with the given {@link String name}
   * in this {@link View}.
   *
   * The first {@link Column} is at index {@literal 0}.
   *
   * @param columnName {@link String} containing the name of the {@link Column}.
   * @return an integer value indicating the index of the {@link String named} {@link Column} in this {@link View},
   * or a {@literal -1} if the {@link String named} {@link Column} is not contained in this {@link View}.
   * @see #columns()
   */
  default int indexOf(String columnName) {

    int index = 0;

    for (Column column : columns()) {

      if (column.getName().equals(columnName)) {
        return index;
      }

      index++;
    }

    return -1;
  }

  /**
   * Determines the index of the given {@link Row} in this {@link View}.
   *
   * @param row {@link Row} to evaluate.
   * @return an integer value indicating the index of the {@link Row} in this {@link View},
   * or a {@literal -1} if the {@link Row} is not contained in this {@link View}.
   * @see org.cp.elements.data.struct.tabular.Row
   * @see #rows()
   */
  @NullSafe
  default int indexOf(Row row) {

    int index = 0;

    for (Row viewRow : rows()) {
      if (viewRow.equals(row)) {
        return index;
      }

      index++;
    }

    return -1;
  }

  /**
   * Determines whether this {@link View} is empty, i.e. contains any {@link Row Rows}.
   *
   * @return a boolean value indicating whether this {@link View} contains any {@link Row Rows}.
   * @see #size()
   */
  default boolean isEmpty() {
    return size() == 0;
  }

  /**
   * Queries this {@link View} using the given {@link Query Query definition} and returns a new {@link View}
   * from the result set.
   *
   * The {@link Query} matches {@link Row Rows} (data) in this {@link View} defined by a {@link Predicate},
   * ordered (sorted) by a {@link Comparator} and projected using an array or {@link Iterable}
   * of {@link Column Columns}.
   *
   * @param query {@link Query} object defining the query for this {@link View}.
   * @return a new {@link View} from the result set produced by the given {@link Query}.
   * @throws IllegalArgumentException if {@link Query} is {@literal null}.
   * @see org.cp.elements.data.struct.tabular.query.Query
   * @see org.cp.elements.data.struct.tabular.View
   */
  default View query(Query query) {

    Assert.notNull(query, "Query is required");

    return query.from(this).execute();
  }

  /**
   * Returns an {@link Iterable} to iterate over the {@link Row Rows} in this {@link View}.
   *
   * @return an {@link Iterable} to iterate over the {@link Row Rows} in this {@link View}.
   * @see org.cp.elements.data.struct.tabular.Row
   * @see java.lang.Iterable
   */
  default Iterable<Row> rows() {
    return this;
  }

  /**
   * Returns the number of {@link Row Rows} in this {@link View}.
   *
   * @return an integer value indicating the number of {@link Row Rows} in this {@link View}.
   * @see #count(Predicate)
   */
  default int size() {
    return count(row -> true);
  }
}
