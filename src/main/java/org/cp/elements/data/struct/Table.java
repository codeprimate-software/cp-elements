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

  public boolean add(Column column);

  public boolean add(Object... row);

  public boolean add(Row row);

  public View query(Filter<Row> rowFilter, Comparator<Row> orderBy, Column... columns);

  public boolean remove(Column column);

  public boolean remove(Row row);

  public boolean removeColumn(int index);

  public boolean removeColumn(String name);

  public boolean removeRow(int index);

  public void setValue(int rowIndex, int columnIndex, Object value);

  public void setValue(int rowIndex, String columnName, Object value);

  public void setValue(int rowIndex, Column column, Object value);

}
