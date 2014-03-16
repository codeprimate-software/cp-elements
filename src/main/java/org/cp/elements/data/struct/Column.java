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
 * The Column interface defines a column in a Table data structure.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.data.struct.Table
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Column<T> {

  /**
   * Gets the alias, or alternate name of this Column.
   * <p/>
   * @return a String value for the alias (alternate name) of this Column.
   */
  public String getAlias();

  /**
   * Sets the alias, or alternate name of this Column.
   * <p/>
   * @param alias a String value indicating the alias for this Column.
   */
  public void setAlias(String alias);

  /**
   * Gets the default value to use when a value is not specified for this Column.
   * <p/>
   * @return the default value of this Column when a value is not specified.
   */
  public T getDefaultValue();

  /**
   * Sets the default value to use when a value is not specified for this Column.
   * <p/>
   * @param defaultValue the default value to use for this Column when a value is not specified.
   */
  public void setDefaultValue(T defaultValue);

  /**
   * Gets a description of this Column.
   * <p/>
   * @return a String value describing this Column.
   */
  public String getDescription();

  /**
   * Sets the description of this Column.
   * <p/>
   * @param description a String value describing this Column.
   */
  public void setDescription(String description);

  /**
   * Gets the index of this Column in the Table.
   * <p/>
   * @return an integer value specifying the index of this Column in the Table.  Returns a -1 if this Column has not
   * been added to a Table.
   */
  public int getIndex();

  /**
   * Gets the name of this Column, which must be unique when adding this Column to a Table.
   * <p/>
   * @return a String value indicating the immutable name of this Column.
   */
  public String getName();

  /**
   * Gets the Class type for values stored in this Column of the Table.
   * <p/>
   * @return the Class type of values in this Column of the Table.
   * @see java.lang.Class
   */
  public Class<T> getType();

}
