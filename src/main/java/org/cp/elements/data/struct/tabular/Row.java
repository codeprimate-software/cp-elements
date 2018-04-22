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

/**
 * The Row interface defines a row in a Table data structure.
 *
 * @author John J. Blum
 * @see java.lang.Iterable
 * @see Column
 * @see Table
 * @see View
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Row extends Iterable<Object> {

  /**
   * Gets the View to which this Row belongs.
   *
   * @return the View to which this Row belongs or null if this Row is not part of any View.
   * @see View
   */
  View getView();

  /**
   * Determines whether this Row has a value at the given column index.
   *
   * @param columnIndex an integer value indicating the column index.
   * @return a boolean value indicating whether this Row has a value at the given column index.
   */
  boolean hasValue(int columnIndex);

  /**
   * Determines whether this Row has a value in the given named column.
   *
   * @param columnName a String value indicating the name of the column.
   * @return a boolean value indicating whether this Row has a value in the given named column.
   */
  boolean hasValue(String columnName);

  /**
   * Determines whether this Row has a value in the given Column.
   *
   * @param column the Column in this Row of the Table.
   * @return a boolean value indicating whether this Row has a value in the given Column.
   * @see Column
   */
  boolean hasValue(Column column);

  /**
   * Gets the value at the given column index in this Row.
   *
   * @param columnIndex an integer value indicating the index of the column.
   * @return an Object value at the given column index in this Row.
   */
  Object getValue(int columnIndex);

  /**
   * Gets the value at the given named column in this Row.
   *
   * @param columnName a String value indicating the name of the column.
   * @return an Object value at the given named column in this Row.
   */
  Object getValue(String columnName);

  /**
   * Gets the value at the given Column in this Row.
   *
   * @param column a Column in this Row of the Table.
   * @return an Object value at the given Column in this Row.
   * @see Column
   */
  Object getValue(Column column);

  /**
   * Sets the value in this Row at the given column index.
   *
   * @param columnIndex an integer value indicating the index of the column.
   * @param value the Object value to set at the given column index in this Row.
   * @return the original Object value at the given column index in this Row.
   */
  Object setValue(int columnIndex, Object value);

  /**
   * Sets the value in this Row in the given named column.
   *
   * @param columnName a String value indicating the name of the column.
   * @param value the Object value to set at the given named column in this Row.
   * @return the original Object value at the given named column in this Row.
   */
  Object setValue(String columnName, Object value);

  /**
   * Sets the value in this Row at the given Column.
   *
   * @param column the Column in this Row of the Table.
   * @param value the Object value to set at the given Column in this Row.
   * @return the original Object value at the given Column in this Row.
   * @see Column
   */
  Object setValue(Column column, Object value);

  /**
   * Gets the index of this Row in the View.
   *
   * @return an integer value indicating the index of this Row in the View.  Returns a -1 if this Row has not been
   * added to a View.
   */
  int index();

  /**
   * Determines the number of values in this Row (the same as the number of columns in this Row).
   *
   * @return an integer value indicating the number of values (columns) in this Row.
   */
  int size();

  /**
   * Returns this Row as an array of Object values.
   *
   * @return an array of Object values contained this Row.
   */
  Object[] values();

}
