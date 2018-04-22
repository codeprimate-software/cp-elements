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
 * The AbstractColumn class is an abstract base class implementing the Column interface, defining a column
 * in a table data structure.
 *
 * @author John J. Blum
 * @param <T> the Class type of the values in this Column of the Table.
 * @see Column
 * @see Row
 * @see Table
 * @see View
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractColumn<T> implements Column<T> {

  private T defaultValue;

  private final Class<T> type;

  private String alias;
  private String description;

  private final String name;

  private View view;

  /**
   * Constructs an instance of the AbstractColumn class with the given name and Class type.
   *
   * @param name a String value indicating the name of this Column.
   * @param type a Class object indicating the type of values stored in this Column.
   * @throws NullPointerException if the name or type are null.
   */
  public AbstractColumn(final String name, final Class<T> type) {
    Assert.notBlank(name, "The name of this Column must be specified!");
    Assert.notNull(type, "The Class type of values stored in this Column must be specified!");
    this.name = name;
    this.type = type;
  }

  /**
   * Constructs an instance of the AbstractColumn class by copying the contents of the specified Column.
   *
   * @param column the Column to copy.
   * @see Column
   * @throws NullPointerException if the specified Column to copy is null.
   * @see Column
   */
  public AbstractColumn(final Column<T> column) {
    Assert.notNull(column, "The Column to copy cannot be null!");
    this.name = column.getName();
    this.type = column.getType();
    setAlias(column.getAlias());
    setDefaultValue(column.getDefaultValue());
    setDescription(column.getDescription());
  }

  /**
   * Gets the alias, or alternate name of this Column.
   *
   * @return a String value for the alias (alternate name) of this Column.
   */
  public String getAlias() {
    return alias;
  }

  /**
   * Sets the alias, or alternate name of this Column.
   *
   * @param alias a String value indicating the alias for this Column.
   */
  public void setAlias(final String alias) {
    this.alias = alias;
  }

  /**
   * Gets the default value to use when a value is not specified for this Column.
   *
   * @return the default value of this Column when a value is not specified.
   */
  public T getDefaultValue() {
    return defaultValue;
  }

  /**
   * Sets the default value to use when a value is not specified for this Column.
   *
   * @param defaultValue the default value to use for this Column when a value is not specified.
   */
  public void setDefaultValue(final T defaultValue) {
    this.defaultValue = defaultValue;
  }

  /**
   * Gets a description of this Column.
   *
   * @return a String value describing this Column.
   */
  public String getDescription() {
    return description;
  }

  /**
   * Sets the description of this Column.
   *
   * @param description a String value describing this Column.
   */
  public void setDescription(final String description) {
    this.description = description;
  }

  /**
   * Gets the index of this Column in the Table.
   *
   * @return an integer value specifying the index of this Column in the View.  Returns a -1 if this Column has not
   * been added to a Table or is not part of any View.
   * @see View#indexOf(Column)
   * @see #getView()
   */
  @Override
  public int getIndex() {
    return getView().indexOf(this);
  }

  /**
   * Gets the name of this Column, which must be unique when adding this Column to a Table.
   *
   * @return a String value indicating the immutable name of this Column.
   */
  public String getName() {
    return name;
  }

  /**
   * Gets the Class type for values stored in this Column of the Table.
   *
   * @return the Class type of values in this Column of the Table.
   * @see java.lang.Class
   */
  public Class<T> getType() {
    return type;
  }

  /**
   * Gets the View to which this Column belongs.
   *
   * @return the View to which this Column belongs or null if this Column is not part of any View.
   * @throws IllegalStateException if a reference to the containing View was not properly configured.
   * @see View
   * @see #setView(View)
   */
  @Override
  public View getView() {
    Assert.state(view != null, "A reference to the View containing this Column has not been properly configured!");
    return view;
  }

  /**
   * Sets the View to which this Column belongs.
   *
   * @param view the View to which this Column belongs or null if this Column is not part of any View.
   * @see #getView()
   * @see View
   */
  public void setView(final View view) {
    this.view = view;
  }

  /**
   * Determines whether the specified Object is equal to this Column.
   *
   * @param obj the Object compared for equality with this Column.
   * @return a boolean value if the specified Object is equal to this Column.
   * @see java.lang.Object#equals(Object)
   */
  @Override
  public boolean equals(final Object obj) {
    if (obj == this) {
      return true;
    }

    if (!(obj instanceof Column)) {
      return false;
    }

    Column that = (Column) obj;

    return this.getName().equals(that.getName());
  }

  /**
   * Gets the hash value of this Column.
   *
   * @return an integer value indicating the hash value of this Column.
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode() {
    int hashValue = 17;
    hashValue = 37 * hashValue + getName().hashCode();
    return hashValue;
  }

  /**
   * Gets a String representation of this Column.
   *
   * @return a String representing this Column.
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    return String.format("{ @type = %1$s, name = %2$s, alias = %3$s, description = %4$s, type = %5$s, defaultValue = %6$s }",
      getClass().getName(), getName(), getAlias(), getDescription(), getType(), getDefaultValue());
  }

}
