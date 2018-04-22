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

import java.util.Iterator;

import org.cp.elements.lang.Assert;

/**
 * The AbstractView class is an abstract base class implementing the View interface in order to encapsulate
 * functionality common to all View implementations.
 *
 * @author John J. Blum
 * @see Column
 * @see Row
 * @see Table
 * @see View
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractView implements View {

  /**
   * Determines whether this View contains a Column with name.
   *
   * @param columnName a String value indicating the name of the Column in question.
   * @return a boolean value indicating whether this View contains a Column with the specified name.
   * @see #getColumn(String)
   */
  @Override
  public boolean contains(final String columnName) {
    return (getColumn(columnName) != null);
  }

  /**
   * Determines whether this View contains the specified Column.
   *
   * @param column the Column being determined for containment by this View.
   * @return a boolean value indicating whether this View contains the specified Column.
   * @see Column
   * @see #columns()
   */
  @Override
  public boolean contains(final Column column) {
    for (Column viewColumn : columns()) {
      if (viewColumn.equals(column)) {
        return true;
      }
    }

    return false;
  }

  /**
   * Gets the Column in this View at the specified index.
   *
   * @param index an integer value specifying the index of the Column in this View to return.
   * @return the Column in this View at the specified index.
   * @see Column
   * @see #columns()
   */
  @Override
  public Column<?> getColumn(final int index) {
    Assert.argument(index > -1, new IndexOutOfBoundsException(String.format(
      "Index (%1$d) must be greater than equal to 0!", index)));

    int currentIndex = 0;

    for (Column column : columns()) {
      if (currentIndex++ == index) {
        return column;
      }
    }

    throw new IndexOutOfBoundsException(String.format("Index (%1$d) exceeds the number of columns (%2$d) in this View!",
      index, currentIndex));
  }

  /**
   * Gets the Column with the specified name in this View.
   *
   * @param name a String value specifying the name of the Column in this View to return.
   * @return the Column in this View with the specified name or null if no Column with name exists in this View.
   * @see Column
   * @see #columns()
   */
  @Override
  public Column<?> getColumn(final String name) {
    for (Column column : columns()) {
      if (column.getName().equals(name)) {
        return column;
      }
    }

    return null;
  }

  /**
   * Gets the Row in this View at the specified index.
   *
   * @param index an integer value specifying the index of the Row in this View to return.
   * @return the Row in this View at the specified index.
   * @see Row
   * @see #rows()
   */
  @Override
  public Row getRow(final int index) {
    Assert.argument(index > -1, new IndexOutOfBoundsException(String.format(
      "Index (%1$d) must be greater than equal to 0!", index)));

    int currentIndex = 0;

    for (Row row : rows()) {
      if (currentIndex++ == index) {
        return row;
      }
    }

    throw new IndexOutOfBoundsException(String.format("Index (%1$d) exceeds the number of rows (%2$d) in this View!",
      index, currentIndex));
  }

  /**
   * Gets the value at the specified row and column index in this View.
   *
   * @param <T> the Class type of the value.
   * @param rowIndex an integer value indicating the row index of the value to get.
   * @param columnIndex an integer value indicating the column index of the value to get.
   * @return the value at the specified row and column index in this View.
   * @see #getRow(int)
   * @see Row#getValue(int)
   */
  @Override
  @SuppressWarnings("unchecked")
  public <T> T getValue(final int rowIndex, final int columnIndex) {
    return (T) getRow(rowIndex).getValue(columnIndex);
  }

  /**
   * Gets the value at the specified row index and named column in this View.
   *
   * @param <T> the Class type of the value.
   * @param rowIndex an integer value indicating the row index of the value to get.
   * @param columnName a String value indicating the name of the column from which to get the value.
   * @return the value at the specified row index and named column in this View.
   * @see #getRow(int)
   * @see Row#getValue(String)
   */
  @Override
  @SuppressWarnings("unchecked")
  public <T> T getValue(final int rowIndex, final String columnName) {
    return (T) getRow(rowIndex).getValue(columnName);
  }

  /**
   * Gets the value at the specified row index and Column in this View.
   *
   * @param <T> the Class type of the value.
   * @param rowIndex an integer value indicating the row index of the value to get.
   * @param column the Column from which to get the value.
   * @return the value at the specified row index and Column in this View.
   * @see #getRow(int)
   * @see Column
   * @see Row#getValue(Column)
   */
  @Override
  @SuppressWarnings("unchecked")
  public <T> T getValue(final int rowIndex, final Column column) {
    return (T) getRow(rowIndex).getValue(column);
  }

  /**
   * Determines whether a value exists at the specified row and column index in this View.
   *
   * @param rowIndex an integer value indicating the row index.
   * @param columnIndex an integer value indicating the column index.
   * @return a boolean value indicating whether a value exists at the specified row and column index in this View.
   * @see #getValue(int, int)
   */
  @Override
  public boolean hasValue(final int rowIndex, final int columnIndex) {
    return (getValue(rowIndex, columnIndex) != null);
  }

  /**
   * Determines whether a value exists at the specified row index and named column in this View.
   *
   * @param rowIndex an integer value indicating the row index.
   * @param columnName a String value indicating the name of the column.
   * @return a boolean value indicating whether a value exists at the specified row index and named column in this View.
   * @see #getValue(int, String)
   */
  @Override
  public boolean hasValue(final int rowIndex, final String columnName) {
    return (getValue(rowIndex, columnName) != null);
  }

  /**
   * Determines whether a value exists at the specified row index and Column in this View.
   *
   * @param rowIndex an integer value indicating the row index.
   * @param column a Column in this View.
   * @return a boolean value indicating whether a value exists at the specified row index and Column in this View.
   * @see Column
   * @see #getValue(int, Column)
   */
  @Override
  public boolean hasValue(final int rowIndex, final Column column) {
    return (getValue(rowIndex, column) != null);
  }

  /**
   * Determines the index of the named column in this View.
   *
   * @param columnName a String value indicating the name of the column.
   * @return an integer value indicating the index of the named column in this View, or -1 if the named column is not
   * contained in this View.
   * @see #columns()
   */
  @Override
  public int indexOf(final String columnName) {
    int index = 0;

    for (Column column : columns()) {
      if (column.getName().equals(columnName)) {
        return index;
      }
      else {
        index++;
      }
    }

    return -1;
  }

  /**
   * Determines the index of the Column in this View.
   *
   * @param column a Column in this View.
   * @return an integer value indicating the index of the Column in this View, or -1 if the Column is not contained
   * in this View.
   * @see Column
   * @see #columns()
   */
  @Override
  public int indexOf(final Column column) {
    int index = 0;

    for (Column viewColumn : columns()) {
      if (viewColumn.equals(column)) {
        return index;
      }
      else {
        index++;
      }
    }

    return -1;
  }

  /**
   * Determines the index of the Row in this View.
   *
   * @param row a Row in this View.
   * @return an integer value indicating the index of the Row in this View, or -1 if the Row is not contained
   * in this View.
   * @see Row
   * @see #rows()
   */
  @Override
  public int indexOf(final Row row) {
    int index = 0;

    for (Row viewRow : this) {
      if (viewRow.equals(row)) {
        return index;
      }
      else {
        index++;
      }
    }

    return -1;
  }

  /**
   * Gets an Iterator to iterate over all the Rows in this View.
   *
   * @return an Iterator object iterating over all the Rows in this View.
   * @see #rows()
   * @see java.lang.Iterable#iterator()
   * @see java.util.Iterator
   * @see Row
   */
  @Override
  public Iterator<Row> iterator() {
    return rows().iterator();
  }

  /**
   * Gets the number of Rows in this View.
   *
   * @return an integer value indicating the number of Rows in this View.
   * @see #rows()
   */
  @Override
  public int size() {
    int count = 0;

    for (Row row : this) {
      count++;
    }

    return count;
  }

}
