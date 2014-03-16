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

import java.util.Comparator;

import org.cp.elements.lang.Filter;

/**
 * The Table interface defines a tabular data structure.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.data.struct.Column
 * @see org.cp.elements.data.struct.Row
 * @see org.cp.elements.data.struct.View
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Table extends View {

  /**
   * Adds the specified Column to the Table.
   * <p/>
   * @param column the Column to add to this Table.
   * @return a boolean value indicating whether the added Column successfully modified the structure of this Table.
   * @see org.cp.elements.data.struct.Column
   */
  public boolean add(Column column);

  /**
   * Adds the array of Object values as a new row in this Table.
   * <p/>
   * @param row an array of Objects containing the values (contents) for the new row of this Table.
   * @return a boolean value indicating whether the row added successfully modifies the structure of this Table.
   */
  public boolean add(Object... row);

  /**
   * Adds the specified Row to this Table.
   * <p/>
   * @param row the Row to add to this Table.
   * @return a boolean value whether the added Row successfully modified the structure of this Table.
   * @see org.cp.elements.data.struct.Row
   */
  public boolean add(Row row);

  /**
   * Queries this Table, filtering the data with the specified Filter returning a View ordered by the given Comparator
   * and a projection specified by the array of Columns.
   * <p/>
   * @param rowFilter a Filter defining the criteria for matching rows.
   * @param orderBy a Comparator defining the order of the data (rows) in the View.
   * @param columns an array of Columns projecting the data to view.
   * @return a View based on the query of this Table.
   * @see java.util.Comparator
   * @see org.cp.elements.data.struct.Column
   * @see org.cp.elements.data.struct.Row
   * @see org.cp.elements.data.struct.View
   */
  public View query(Filter<Row> rowFilter, Comparator<Row> orderBy, Column... columns);

  /**
   * Removes the column at specified index from this Table.
   * <p/>
   * @param index an integer value specifying the index of the column to remove from this Table.
   * @return a boolean value indicating whether the specified column at index was successfully removed.
   */
  public boolean removeColumn(int index);

  /**
   * Removes the column with specified name from this Table.
   * <p/>
   * @param name a String value specifying the name of the column to remove from this Table.
   * @return a boolean value indicating whether the specified column with name was successfully removed.
   */
  public boolean removeColumn(String name);

  /**
   * Removes the specified Column from this Table.
   * <p/>
   * @param column the Column to remove from this Table.
   * @return a boolean value indicating whether the specified Column was successfully removed from this Table modifying
   * it's structure.
   * @see org.cp.elements.data.struct.Column
   */
  public boolean remove(Column column);

  /**
   * Removes the row at specified index from this Table.
   * <p/>
   * @param index an integer value specifying the index of the row to remove from this Table.
   * @return a boolean value indicating whether the specified row at index was successfully removed.
   */
  public boolean removeRow(int index);

  /**
   * Removes the specified Row from this Table.
   * <p/>
   * @param row the Row to remove from this Table.
   * @return a boolean value indicating whether the specified Row was successfully removed from this Table modifying
   * it's structure.
   * @see org.cp.elements.data.struct.Row
   */
  public boolean remove(Row row);

  /**
   * Sets the value at given row and column in this Table specified by index.
   * <p/>
   * @param rowIndex an integer value indicating the row index.
   * @param columnIndex an integer value indicating the column index.
   * @param value the Object value to add at the given row and column in this Table.
   */
  public void setValue(int rowIndex, int columnIndex, Object value);

  /**
   * Sets the value at the given row index and named column in this Table.
   * <p/>
   * @param rowIndex an integer value indicating the row index.
   * @param columnName a String value indicating the column name.
   * @param value the Object value to add at the given row and column in this Table.
   */
  public void setValue(int rowIndex, String columnName, Object value);

  /**
   * Sets the value at the given row index and Column in this Table.
   * <p/>
   * @param rowIndex an integer value indicating the row index.
   * @param column a Column in this Table.
   * @param value the Object value to add at the given row and column in this Table.
   * @see org.cp.elements.data.struct.Column
   */
  public void setValue(int rowIndex, Column column, Object value);

}
