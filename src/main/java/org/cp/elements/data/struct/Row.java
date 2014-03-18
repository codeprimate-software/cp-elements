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
 * The Row interface defines a row in a Table data structure.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.data.struct.Column
 * @see org.cp.elements.data.struct.Table
 * @see org.cp.elements.data.struct.View
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Row extends Iterable<Object> {

  /**
   * Gets the View to which this Row belongs.
   * <p/>
   * @return the View to which this Row belongs or null if this Row is not part of any View.
   * @see org.cp.elements.data.struct.View
   */
  public View getView();

  /**
   * Determines whether this Row has a value at the given column index.
   * <p/>
   * @param columnIndex an integer value indicating the column index.
   * @return a boolean value indicating whether this Row has a value at the given column index.
   */
  public boolean hasValue(int columnIndex);

  /**
   * Determines whether this Row has a value in the given named column.
   * <p/>
   * @param columnName a String value indicating the name of the column.
   * @return a boolean value indicating whether this Row has a value in the given named column.
   */
  public boolean hasValue(String columnName);

  /**
   * Determines whether this Row has a value in the given Column.
   * <p/>
   * @param column the Column in this Row of the Table.
   * @return a boolean value indicating whether this Row has a value in the given Column.
   * @see org.cp.elements.data.struct.Column
   */
  public boolean hasValue(Column column);

  /**
   * Gets the value at the given column index in this Row.
   * <p/>
   * @param columnIndex an integer value indicating the index of the column.
   * @return an Object value at the given column index in this Row.
   */
  public Object getValue(int columnIndex);

  /**
   * Gets the value at the given named column in this Row.
   * <p/>
   * @param columnName a String value indicating the name of the column.
   * @return an Object value at the given named column in this Row.
   */
  public Object getValue(String columnName);

  /**
   * Gets the value at the given Column in this Row.
   * <p/>
   * @param column a Column in this Row of the Table.
   * @return an Object value at the given Column in this Row.
   * @see org.cp.elements.data.struct.Column
   */
  public Object getValue(Column column);

  /**
   * Sets the value at the given column index in this Row.
   * <p/>
   * @param columnIndex an integer value indicating the index of the column.
   * @param value the Object value to set at the given column index in this Row.
   * @return the original Object value at the given column index in this Row.
   */
  public Object setValue(int columnIndex, Object value);

  /**
   * Sets the value in this Row in the given named column.
   * <p/>
   * @param columnName a String value indicating the name of the column.
   * @param value the Object value to set at the given named column in this Row.
   * @return the original Object value at the given named column in this Row.
   */
  public Object setValue(String columnName, Object value);

  /**
   * Sets the value at the given Column in this Row.
   * <p/>
   * @param column the Column in this Row of the Table.
   * @param value the Object value to set at the given Column in this Row.
   * @return the original Object value at the given Column in this Row.
   * @see org.cp.elements.data.struct.Column
   */
  public Object setValue(Column column, Object value);

  /**
   * Gets the index of this Row in the View.
   * <p/>
   * @return an integer value indicating the index of this Row in the View.  Returns a -1 if this Row has not been
   * added to a View.
   */
  public int index();

  /**
   * Determines the number of value in this Row (the same as the number of columns in this Row).
   * <p/>
   * @return an integer value indicating the number of values (columns) in this Row.
   */
  public int size();

  /**
   * Returns this Row as an array of Object values.
   * <p/>
   * @return an array of Object values contained this Row.
   */
  public Object[] values();

}
