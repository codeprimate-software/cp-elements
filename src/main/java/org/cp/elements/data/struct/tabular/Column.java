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
 * The Column interface defines a column in a Table data structure.
 *
 * @author John J. Blum
 * @param <T> the Class type of values store in this column of the Table.
 * @see Row
 * @see Table
 * @see View
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Column<T> {

  /**
   * Gets the alias, or alternate name of this Column.
   *
   * @return a String value for the alias (alternate name) of this Column.
   */
  public String getAlias();

  /**
   * Sets the alias, or alternate name of this Column.
   *
   * @param alias a String value indicating the alias for this Column.
   */
  public void setAlias(String alias);

  /**
   * Gets the default value to use when a value is not specified for this Column.
   *
   * @return the default value of this Column when a value is not specified.
   */
  public T getDefaultValue();

  /**
   * Sets the default value to use when a value is not specified for this Column.
   *
   * @param defaultValue the default value to use for this Column when a value is not specified.
   */
  public void setDefaultValue(T defaultValue);

  /**
   * Gets a description of this Column.
   *
   * @return a String value describing this Column.
   */
  public String getDescription();

  /**
   * Sets the description of this Column.
   *
   * @param description a String value describing this Column.
   */
  public void setDescription(String description);

  /**
   * Gets the index of this Column in the Table.
   *
   * @return an integer value specifying the index of this Column in the View.  Returns a -1 if this Column has not
   * been added to a Table or is not part of any View.
   */
  public int getIndex();

  /**
   * Gets the name of this Column, which must be unique when adding this Column to a Table.
   *
   * @return a String value indicating the immutable name of this Column.
   */
  public String getName();

  /**
   * Gets the Class type for values stored in this Column of the Table.
   *
   * @return the Class type of values in this Column of the Table.
   * @see java.lang.Class
   */
  public Class<T> getType();

  /**
   * Gets the View to which this Column belongs.
   *
   * @return the View to which this Column belongs or null if this Column is not part of any View.
   * @see View
   */
  public View getView();

}
