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

import java.util.Comparator;

import org.cp.elements.lang.Filter;

/**
 * The View interface defines a limited view (projection) of a tabular data structure.
 *
 * @author John J. Blum
 * @see Column
 * @see Row
 * @see Table
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface View extends Iterable<Row> {

  /**
   * Gets an Iterable object to iterate over the Columns in this View.
   *
   * @return an Iterable object to iterate over the Columns in this View.
   * @see java.lang.Iterable
   * @see Column
   */
  Iterable<Column> columns();

  /**
   * Determines whether this View contains a Column with name.
   *
   * @param columnName a String value indicating the name of the Column in question.
   * @return a boolean value indicating whether this View contains a Column with the specified name.
   */
  boolean contains(String columnName);

  /**
   * Determines whether this View contains the specified Column.
   *
   * @param column the Column being determined for containment by this View.
   * @return a boolean value indicating whether this View contains the specified Column.
   * @see Column
   */
  boolean contains(Column column);

  /**
   * Gets the Column in this View at the specified index.
   *
   * @param index an integer value specifying the index of the Column in this View to return.
   * @return the Column in this View at the specified index.
   * @see Column
   */
  Column<?> getColumn(int index);

  /**
   * Gets the Column with the specified name in this View.
   *
   * @param name a String value specifying the name of the Column in this View to return.
   * @return the Column in this View with the specified name or null if no Column with name exists in this View.
   * @see Column
   */
  Column<?> getColumn(String name);

  /**
   * Gets the Row in this View at the specified index.
   *
   * @param index an integer value specifying the index of the Row in this View to return.
   * @return the Row in this View at the specified index.
   * @see Row
   */
  Row getRow(int index);

  /**
   * Gets the value at the specified row and column index in this View.
   *
   * @param <T> the Class type of the value.
   * @param rowIndex an integer value indicating the row index of the value to get.
   * @param columnIndex an integer value indicating the column index of the value to get.
   * @return the value at the specified row and column index in this View.
   */
  <T> T getValue(int rowIndex, int columnIndex);

  /**
   * Gets the value at the specified row index and named column in this View.
   *
   * @param <T> the Class type of the value.
   * @param rowIndex an integer value indicating the row index of the value to get.
   * @param columnName a String value indicating the name of the column from which to get the value.
   * @return the value at the specified row index and named column in this View.
   */
  <T> T getValue(int rowIndex, String columnName);

  /**
   * Gets the value at the specified row index and Column in this View.
   *
   * @param <T> the Class type of the value.
   * @param rowIndex an integer value indicating the row index of the value to get.
   * @param column the Column from which to get the value.
   * @return the value at the specified row index and Column in this View.
   * @see Column
   */
  <T> T getValue(int rowIndex, Column column);

  /**
   * Determines whether a value exists at the specified row and column index in this View.
   *
   * @param rowIndex an integer value indicating the row index.
   * @param columnIndex an integer value indicating the column index.
   * @return a boolean value indicating whether a value exists at the specified row and column index in this View.
   */
  boolean hasValue(int rowIndex, int columnIndex);

  /**
   * Determines whether a value exists at the specified row index and named column in this View.
   *
   * @param rowIndex an integer value indicating the row index.
   * @param columnName a String value indicating the name of the column.
   * @return a boolean value indicating whether a value exists at the specified row index and named column in this View.
   */
  boolean hasValue(int rowIndex, String columnName);

  /**
   * Determines whether a value exists at the specified row index and Column in this View.
   *
   * @param rowIndex an integer value indicating the row index.
   * @param column a Column in this View.
   * @return a boolean value indicating whether a value exists at the specified row index and Column in this View.
   * @see Column
   */
  boolean hasValue(int rowIndex, Column column);

  /**
   * Determines the index of the named column in this View.
   *
   * @param columnName a String value indicating the name of the column.
   * @return an integer value indicating the index of the named column in this View, or -1 if the named column is not
   * contained in this View.
   */
  int indexOf(String columnName);

  /**
   * Determines the index of the Column in this View.
   *
   * @param column a Column in this View.
   * @return an integer value indicating the index of the Column in this View, or -1 if the Column is not contained
   * in this View.
   * @see Column
   */
  int indexOf(Column column);

  /**
   * Determines the index of the Row in this View.
   *
   * @param row a Row in this View.
   * @return an integer value indicating the index of the Row in this View, or -1 if the Row is not contained
   * in this View.
   * @see Row
   */
  int indexOf(Row row);

  /**
   * Queries this View, filtering data (rows) with the specified Filter (predicate), returning a View ordered
   * by the given Comparator (sort) and a projection specified by the array of Columns.
   *
   * @param predicate a Filter defining the predicate criteria for matching rows.
   * @param orderBy a Comparator defining the order of the data (rows) in the View.
   * @param projection an array of Columns projecting the data for the view.
   * @return a View based on the query of this Table.
   * @see java.util.Comparator
   * @see Column
   * @see Row
   * @see View
   */
  View query(Filter<Row> predicate, Comparator<Row> orderBy, Column... projection);

  /**
   * Gets an Iterable object to iterate over the Rows in this View.
   *
   * @return an Iterable object to iterate over the Rows in this View.
   * @see java.lang.Iterable
   * @see Row
   */
  Iterable<Row> rows();

  /**
   * Gets the number of Rows in this View.
   *
   * @return an integer value indicating the number of Rows in this View.
   */
  int size();

}
