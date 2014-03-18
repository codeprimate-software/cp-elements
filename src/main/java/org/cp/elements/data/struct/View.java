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

/**
 * The View interface defines a limited view (projection) of a tabular data structure.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.data.struct.Column
 * @see org.cp.elements.data.struct.Row
 * @see org.cp.elements.data.struct.Table
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface View extends Iterable<Row> {

  /**
   * Gets an Iterable object iterating over the Columns in this View.
   * <p/>
   * @return an Iterable object to iterate over the Columns in this View.
   * @see java.lang.Iterable
   * @see org.cp.elements.data.struct.Column
   */
  public Iterable<Column> columns();

  /**
   * Determines whether this View contains a Column with name.
   * <p/>
   * @param columnName a String value indicating the name of the Column in question.
   * @return a boolean value indicating whether this View contains a Column with the specified name.
   */
  public boolean contains(String columnName);

  /**
   * Determines whether this View contains the specified Column.
   * <p/>
   * @param column the Column being determined for containment by this View.
   * @return a boolean value indicating whether this View contains the specified Column.
   * @see org.cp.elements.data.struct.Column
   */
  public boolean contains(Column column);

  /**
   * Gets the Column in this View at the specified index.
   * <p/>
   * @param index an integer value specifying the index of the Column in this View to return.
   * @return the Column in this View at the specified index.
   * @see org.cp.elements.data.struct.Column
   */
  public Column<?> getColumn(int index);

  /**
   * Gets the Column in this View with the specified name.
   * <p/>
   * @param name a String value specifying the name of the Column in this View to return.
   * @return the Column in this View with the specified name or null if no Column with name exists in this View.
   * @see org.cp.elements.data.struct.Column
   */
  public Column<?> getColumn(String name);

  /**
   * Gets the Row in this View at the specified index.
   * <p/>
   * @param index an integer value specifying the index of the Row in this View to return.
   * @return the Row in this View at the specified index.
   * @see org.cp.elements.data.struct.Row
   */
  public Row getRow(int index);

  /**
   * Gets the value at the specified row and column index in this View.
   * <p/>
   * @param <T> the Class type of the value.
   * @param rowIndex an integer value indicating the row index of the value to get.
   * @param columnIndex an integer value indicating the column index of the value to get.
   * @return the value at the specified row and column index in this View.
   */
  public <T> T getValue(int rowIndex, int columnIndex);

  /**
   * Gets the value at the specified row index and named column in this View.
   * <p/>
   * @param <T> the Class type of the value.
   * @param rowIndex an integer value indicating the row index of the value to get.
   * @param columnName a String value indicating the name of the column from which to get the value.
   * @return the value at the specified row index and named column in this View.
   */
  public <T> T getValue(int rowIndex, String columnName);

  /**
   * Gets the value at the specified row index and Column in this View.
   * <p/>
   * @param <T> the Class type of the value.
   * @param rowIndex an integer value indicating the row index of the value to get.
   * @param column the Column from which to get the value.
   * @return the value at the specified row index and Column in this View.
   * @see org.cp.elements.data.struct.Column
   */
  public <T> T getValue(int rowIndex, Column column);

  /**
   * Determines whether a value exists at the specified row and column index in this View.
   * <p/>
   * @param rowIndex an integer value indicating the row index.
   * @param columnIndex an integer value indicating the column index.
   * @return a boolean value indicating whether a value exists at the specified row and column index in this View.
   */
  public boolean hasValue(int rowIndex, int columnIndex);

  /**
   * Determines whether a value exists at the specified row index and named column in this View.
   * <p/>
   * @param rowIndex an integer value indicating the row index.
   * @param columnName a String value indicating the name of the column.
   * @return a boolean value indicating whether a value exists at the specified row index and named column in this View.
   */
  public boolean hasValue(int rowIndex, String columnName);

  /**
   * Determines whether a value exists at the specified row index and Column in this View.
   * <p/>
   * @param rowIndex an integer value indicating the row index.
   * @param column a Column in this View.
   * @return a boolean value indicating whether a value exists at the specified row index and Column in this View.
   * @see org.cp.elements.data.struct.Column
   */
  public boolean hasValue(int rowIndex, Column column);

  /**
   * Determines the index of the named column in this View.
   * <p/>
   * @param columnName a String value indicating the name of the column.
   * @return an integer value indicating the index of the named column in this View, or -1 if the named column is not
   * contained in this View.
   */
  public int indexOf(String columnName);

  /**
   * Determines the index of the Column in this View.
   * <p/>
   * @param column a Column in this View.
   * @return an integer value indicating the index of the Column in this View, or -1 if the Column is not contained
   * in this View.
   * @see org.cp.elements.data.struct.Column
   */
  public int indexOf(Column column);

  /**
   * Determines the index of the Row in this View.
   * <p/>
   * @param row a Row in this View.
   * @return an integer value indicating the index of the Row in this View, or -1 if the Row is not contained
   * in this View.
   * @see org.cp.elements.data.struct.Row
   */
  public int indexOf(Row row);

  /**
   * Gets an Iterable object to iterate over the Rows in this View.
   * <p/>
   * @return an Iterable object to iterate over the Rows in this View.
   * @see java.lang.Iterable
   * @see org.cp.elements.data.struct.Row
   */
  public Iterable<Row> rows();

  /**
   * Gets the number of Rows in this View.
   * <p/>
   * @return an integer value indicating the number of Rows in this View.
   */
  public int size();

}
