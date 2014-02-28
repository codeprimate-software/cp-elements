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

  public boolean contains(String columnName);

  public boolean contains(Column column);

  public Column<?> getColumn(int index);

  public Column<?> getColumn(String name);

  public Iterable<Column> getColumns();

  public Row getRow(int index);

  public Iterable<Row> getRows();

  public <T> T getValue(int rowIndex, int columnIndex);

  public <T> T getValue(int rowIndex, String columnName);

  public <T> T getValue(int rowIndex, Column column);

  public boolean hasValue(int rowIndex, int columnIndex);

  public boolean hasValue(int rowIndex, String columnName);

  public boolean hasValue(int rowIndex, Column column);

  public int size();

}
