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

import org.cp.elements.util.ArrayUtils;

/**
 * The AbstractTable class is an abstract base class implementation of the Table interface encapsulating functionality
 * common to all Table implementations.
 * <p/>
 * @author John J. Blum
 * @see AbstractView
 * @see Column
 * @see Row
 * @see Table
 * @see View
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractTable extends AbstractView implements Table {

  /**
   * Adds the array of Object values as a new row in this Table.
   *
   * @param row an array of Objects containing the values (contents) for the new row of this Table.
   * @return a boolean value indicating whether the row added successfully modified the structure of this Table.
   * @see AbstractRow
   */
  @Override
  public boolean add(final Object... row) {
    return add(new AbstractRow() {
      @Override public Object getValue(final int columnIndex) {
        return row[columnIndex];
      }

      @Override public Object setValue(final int columnIndex, final Object value) {
        Object currentValue = row[columnIndex];
        row[columnIndex] = value;
        return currentValue;
      }

      @Override
      public Iterator<Object> iterator() {
        return ArrayUtils.iterator(row);
      }
    });
  }

  /**
   * Removes the column at the specified index from this Table.
   *
   * @param index an integer value specifying the index of the column to remove from this Table.
   * @return a boolean value indicating whether the specified column at index was successfully removed.
   * @see #getColumn(int)
   * @see #remove(Column)
   */
  @Override
  public boolean removeColumn(final int index) {
    return remove(getColumn(index));
  }

  /**
   * Removes the column with the specified name from this Table.
   *
   * @param name a String value specifying the name of the column to remove from this Table.
   * @return a boolean value indicating whether the specified column with name was successfully removed.
   * @see #getColumn(String)
   * @see #remove(Column)
   */
  @Override
  public boolean removeColumn(final String name) {
    return remove(getColumn(name));
  }

  /**
   * Removes the specified Column from this Table.
   *
   * @param column the Column to remove from this Table.
   * @return a boolean value indicating whether the specified Column was successfully removed from this Table modifying
   * it's structure.
   * @see Column
   * @see java.util.Iterator#remove()
   * @see #columns()
   */
  @Override
  public boolean remove(final Column column) {
    for (Iterator<Column> columns = columns().iterator(); columns.hasNext(); ) {
      Column viewColumn = columns.next();
      if (viewColumn.equals(column)) {
        columns.remove();
        return true;
      }
    }

    return false;
  }

  /**
   * Removes the row at the specified index from this Table.
   *
   * @param index an integer value specifying the index of the row to remove from this Table.
   * @return a boolean value indicating whether the specified row at index was successfully removed.
   * @see #getRow(int)
   * @see #remove(Row)
   */
  @Override
  public boolean removeRow(final int index) {
    return remove(getRow(index));
  }

  /**
   * Removes the specified Row from this Table.
   *
   * @param row the Row to remove from this Table.
   * @return a boolean value indicating whether the specified Row was successfully removed from this Table modifying
   * it's structure.
   * @see Row
   * @see java.util.Iterator#remove()
   * @see #rows()
   */
  @Override
  public boolean remove(final Row row) {
    for (Iterator<Row> rows = rows().iterator(); rows.hasNext(); ) {
      Row viewRow = rows.next();
      if (viewRow.equals(row)) {
        rows.remove();
        return true;
      }
    }

    return false;
  }

  /**
   * Sets the value at the given row and column index in this Table.
   *
   * @param rowIndex an integer value indicating the row index.
   * @param columnIndex an integer value indicating the column index.
   * @param value the Object value to add at the given row and column in this Table.
   * @see #getRow(int)
   * @see Row#setValue(int, Object)
   */
  @Override
  public void setValue(final int rowIndex, final int columnIndex, final Object value) {
    getRow(rowIndex).setValue(columnIndex, value);
  }

  /**
   * Sets the value at the given row index and named column in this Table.
   *
   * @param rowIndex an integer value indicating the row index.
   * @param columnName a String value indicating the column name.
   * @param value the Object value to add at the given row and column in this Table.
   * @see #getRow(int)
   * @see Row#setValue(String, Object)
   */
  @Override
  public void setValue(final int rowIndex, final String columnName, final Object value) {
    getRow(rowIndex).setValue(columnName, value);
  }

  /**
   * Sets the value at the given row index and Column in this Table.
   *
   * @param rowIndex an integer value indicating the row index.
   * @param column a Column in this Table.
   * @param value the Object value to add at the given row and column in this Table.
   * @see Column
   * @see #getRow(int)
   * @see Row#setValue(Column, Object)
   */
  @Override
  public void setValue(final int rowIndex, final Column column, final Object value) {
    getRow(rowIndex).setValue(column, value);
  }

}
