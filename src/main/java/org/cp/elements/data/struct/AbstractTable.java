/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.data.struct;

import java.util.Iterator;

import org.cp.elements.util.ArrayUtils;

/**
 * The AbstractTable class is an abstract base class implementation of the Table interface encapsulating functionality
 * common to all Table implementations.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.data.struct.AbstractView
 * @see org.cp.elements.data.struct.Column
 * @see org.cp.elements.data.struct.Row
 * @see org.cp.elements.data.struct.Table
 * @see org.cp.elements.data.struct.View
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractTable extends AbstractView implements Table {

  /**
   * Adds the array of Object values as a new row in this Table.
   *
   * @param row an array of Objects containing the values (contents) for the new row of this Table.
   * @return a boolean value indicating whether the row added successfully modified the structure of this Table.
   * @see org.cp.elements.data.struct.AbstractRow
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
   * @see org.cp.elements.data.struct.Column
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
   * @see org.cp.elements.data.struct.Row
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
   * @see org.cp.elements.data.struct.Row#setValue(int, Object)
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
   * @see org.cp.elements.data.struct.Row#setValue(String, Object)
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
   * @see org.cp.elements.data.struct.Column
   * @see #getRow(int)
   * @see org.cp.elements.data.struct.Row#setValue(Column, Object)
   */
  @Override
  public void setValue(final int rowIndex, final Column column, final Object value) {
    getRow(rowIndex).setValue(column, value);
  }

}
