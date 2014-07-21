/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * 
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * 
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * 
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * 
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.data.struct;

/**
 * The Column interface defines a column in a Table data structure.
 *
 * @author John J. Blum
 * @param <T> the Class type of values store in this column of the Table.
 * @see org.cp.elements.data.struct.Row
 * @see org.cp.elements.data.struct.Table
 * @see org.cp.elements.data.struct.View
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
   * @see org.cp.elements.data.struct.View
   */
  public View getView();

}
