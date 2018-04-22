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

import org.cp.elements.lang.Assert;

/**
 * The AbstractRow class is an abstract base class implementing the Row interface encapsulating functionality common
 * to all Row implementations.
 *
 * @author John J. Blum
 * @see Column
 * @see Row
 * @see Table
 * @see View
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
   * @see View
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
   * @see View
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
   * @see Column
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
   * @see View#indexOf(String)
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
   * @see Column
   * @see View#indexOf(Column)
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
   * @see View#indexOf(String)
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
   * @see Column
   * @see View#indexOf(Column)
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
   * @see View#indexOf(Row)
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
