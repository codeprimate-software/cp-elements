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

import org.cp.elements.lang.Assert;

/**
 * The AbstractRow class is an abstract base class implementing the Row interface encapsulating functionality common
 * to all Row implementations.
 *
 * @author John J. Blum
 * @see org.cp.elements.data.struct.Column
 * @see org.cp.elements.data.struct.Row
 * @see org.cp.elements.data.struct.Table
 * @see org.cp.elements.data.struct.View
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractRow implements Row {

  private View view;

  /**
   * Gets the View to which this Row belongs.
   *
   * @return the View to which this Row belongs or null if this Row is not part of any View.
   * @throws IllegalStateException if a reference to the containing View was not properly configured.
   * @see #setView(View)
   * @see org.cp.elements.data.struct.View
   */
  @Override
  public View getView() {
    Assert.state(view != null, "A reference to the containing View for this Row was not properly configured!");
    return view;
  }

  /**
   * Sets the View to which this Row belongs.
   *
   * @param view the View to which this Row belongs or null if this Row is not part of any View.
   * @see #getView()
   * @see org.cp.elements.data.struct.View
   */
  public void setView(final View view) {
    this.view = view;
  }

  /**
   * Determines whether this Row has a value at the given column index.
   *
   * @param columnIndex an integer value indicating the column index.
   * @return a boolean value indicating whether this Row has a value at the given column index.
   * @see #getValue(int)
   */
  @Override
  public boolean hasValue(final int columnIndex) {
    return (getValue(columnIndex) != null);
  }

  /**
   * Determines whether this Row has a value in the given named column.
   *
   * @param columnName a String value indicating the name of the column.
   * @return a boolean value indicating whether this Row has a value in the given named column.
   * @see #getValue(String)
   */
  @Override
  public boolean hasValue(final String columnName) {
    return (getValue(columnName) != null);
  }

  /**
   * Determines whether this Row has a value in the given Column.
   *
   * @param column the Column in this Row of the Table.
   * @return a boolean value indicating whether this Row has a value in the given Column.
   * @see org.cp.elements.data.struct.Column
   * @see #getValue(Column)
   */
  @Override
  public boolean hasValue(final Column column) {
    return (getValue(column) != null);
  }

  /**
   * Gets the value at the given named column in this Row.
   *
   * @param columnName a String value indicating the name of the column.
   * @return an Object value at the given named column in this Row.
   * @see org.cp.elements.data.struct.View#indexOf(String)
   * @see #getValue(int)
   * @see #getView()
   */
  @Override
  public Object getValue(final String columnName) {
    return getValue(getView().indexOf(columnName));
  }

  /**
   * Gets the value at the given Column in this Row.
   *
   * @param column a Column in this Row of the Table.
   * @return an Object value at the given Column in this Row.
   * @see org.cp.elements.data.struct.Column
   * @see org.cp.elements.data.struct.View#indexOf(Column)
   * @see #getValue(int)
   * @see #getView()
   */
  @Override
  public Object getValue(final Column column) {
    return getValue(getView().indexOf(column));
  }

  /**
   * Sets the value in this Row in the given named column.
   *
   * @param columnName a String value indicating the name of the column.
   * @param value the Object value to set at the given named column in this Row.
   * @return the original Object value at the given named column in this Row.
   * @see org.cp.elements.data.struct.View#indexOf(String)
   * @see #setValue(int, Object)
   * @see #getView()
   */
  @Override
  public Object setValue(final String columnName, final Object value) {
    return setValue(getView().indexOf(columnName), value);
  }

  /**
   * Sets the value in this Row at the given Column.
   *
   * @param column the Column in this Row of the Table.
   * @param value the Object value to set at the given Column in this Row.
   * @return the original Object value at the given Column in this Row.
   * @see org.cp.elements.data.struct.Column
   * @see org.cp.elements.data.struct.View#indexOf(Column)
   * @see #setValue(int, Object)
   * @see #getView()
   */
  @Override
  public Object setValue(final Column column, final Object value) {
    return setValue(getView().indexOf(column), value);
  }

  /**
   * Gets the index of this Row in the View.
   *
   * @return an integer value indicating the index of this Row in the View.  Returns a -1 if this Row has not been
   * added to a View.
   * @see org.cp.elements.data.struct.View#indexOf(Row)
   * @see #getView()
   */
  @Override
  public int index() {
    return getView().indexOf(this);
  }

  /**
   * Determines the number of values in this Row (the same as the number of columns in this Row).
   *
   * @return an integer value indicating the number of values (columns) in this Row.
   * @see java.lang.Iterable#iterator()
   */
  @Override
  public int size() {
    int count = 0;

    for (Object value : this) {
      count++;
    }

    return count;
  }

  /**
   * Returns this Row as an array of Object values.
   *
   * @return an array of Object values contained this Row.
   * @see java.lang.Iterable#iterator()
   */
  @Override
  public Object[] values() {
    Object[] values = new Object[size()];
    int index = 0;

    for (Object value : this) {
      values[index++] = value;
    }

    return values;
  }

}
