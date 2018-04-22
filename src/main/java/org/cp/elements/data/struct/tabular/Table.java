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
 * The Table interface defines a tabular data structure.
 *
 * @author John J. Blum
 * @see Column
 * @see Row
 * @see View
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Table extends View {

  /**
   * Adds the specified Column to this Table.
   *
   * @param column the Column to add to this Table.
   * @return a boolean value indicating whether the added Column successfully modified the structure of this Table.
   * @see Column
   */
  public boolean add(Column column);

  /**
   * Adds the array of Object values as a new row in this Table.
   *
   * @param row an array of Objects containing the values (contents) for the new row of this Table.
   * @return a boolean value indicating whether the row added successfully modified the structure of this Table.
   */
  public boolean add(Object... row);

  /**
   * Adds the specified Row to this Table.
   *
   * @param row the Row to add to this Table.
   * @return a boolean value whether the added Row successfully modified the structure of this Table.
   * @see Row
   */
  public boolean add(Row row);

  /**
   * Removes the column at the specified index from this Table.
   *
   * @param index an integer value specifying the index of the column to remove from this Table.
   * @return a boolean value indicating whether the specified column at index was successfully removed.
   */
  public boolean removeColumn(int index);

  /**
   * Removes the column with the specified name from this Table.
   *
   * @param name a String value specifying the name of the column to remove from this Table.
   * @return a boolean value indicating whether the specified column with name was successfully removed.
   */
  public boolean removeColumn(String name);

  /**
   * Removes the specified Column from this Table.
   *
   * @param column the Column to remove from this Table.
   * @return a boolean value indicating whether the specified Column was successfully removed from this Table modifying
   * it's structure.
   * @see Column
   */
  public boolean remove(Column column);

  /**
   * Removes the row at the specified index from this Table.
   *
   * @param index an integer value specifying the index of the row to remove from this Table.
   * @return a boolean value indicating whether the specified row at index was successfully removed.
   */
  public boolean removeRow(int index);

  /**
   * Removes the specified Row from this Table.
   *
   * @param row the Row to remove from this Table.
   * @return a boolean value indicating whether the specified Row was successfully removed from this Table modifying
   * it's structure.
   * @see Row
   */
  public boolean remove(Row row);

  /**
   * Sets the value at the given row and column index in this Table.
   *
   * @param rowIndex an integer value indicating the row index.
   * @param columnIndex an integer value indicating the column index.
   * @param value the Object value to add at the given row and column in this Table.
   */
  public void setValue(int rowIndex, int columnIndex, Object value);

  /**
   * Sets the value at the given row index and named column in this Table.
   *
   * @param rowIndex an integer value indicating the row index.
   * @param columnName a String value indicating the column name.
   * @param value the Object value to add at the given row and column in this Table.
   */
  public void setValue(int rowIndex, String columnName, Object value);

  /**
   * Sets the value at the given row index and Column in this Table.
   *
   * @param rowIndex an integer value indicating the row index.
   * @param column a Column in this Table.
   * @param value the Object value to add at the given row and column in this Table.
   * @see Column
   */
  public void setValue(int rowIndex, Column column, Object value);

}
