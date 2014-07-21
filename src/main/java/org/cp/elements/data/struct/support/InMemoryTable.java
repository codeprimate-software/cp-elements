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

package org.cp.elements.data.struct.support;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import org.cp.elements.data.struct.AbstractColumn;
import org.cp.elements.data.struct.AbstractRow;
import org.cp.elements.data.struct.AbstractTable;
import org.cp.elements.data.struct.Column;
import org.cp.elements.data.struct.Row;
import org.cp.elements.data.struct.View;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Filter;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.util.ArrayUtils;

/**
 * The InMemoryTable class is an implementation of the Table interface implementing a table (tabular) data structure
 * in the Java VM Heap.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.data.struct.AbstractTable
 * @see org.cp.elements.data.struct.Column
 * @see org.cp.elements.data.struct.Row
 * @see org.cp.elements.data.struct.Table
 * @see org.cp.elements.data.struct.View
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class InMemoryTable extends AbstractTable {

  protected static final int DEFAULT_INITIAL_CAPACITY = 51;

  private final List<Column> columns;
  private final List<Row> rows;

  public InMemoryTable() {
    this(DEFAULT_INITIAL_CAPACITY);
  }

  public InMemoryTable(final Column... columns) {
    this(DEFAULT_INITIAL_CAPACITY, columns);
  }

  @SuppressWarnings("unchecked")
  public InMemoryTable(final int initialCapacity, final Column... columns) {
    List<Column> inMemoryColumns = new ArrayList<Column>(columns.length);

    for (Column column : columns) {
      inMemoryColumns.add(new InMemoryColumn(column));
    }

    this.columns = new CopyOnWriteArrayList<Column>(inMemoryColumns);
    this.rows = Collections.synchronizedList(new ArrayList<Row>(initialCapacity));
  }

  @Override
  public synchronized boolean add(final Column column) {
    return false;  //To change body of implemented methods use File | Settings | File Templates.
  }

  @Override
  public synchronized boolean add(final Row row) {
    return rows.add(new InMemoryRow(validateRow(row)));
  }

  @Override
  public Iterable<Column> columns() {
    return Collections.unmodifiableList(columns);
  }

  @Override
  public View query(final Filter<Row> predicate, final Comparator<Row> orderBy, final Column... projection) {
    return null;
  }

  @Override
  public Iterable<Row> rows() {
    return Collections.unmodifiableList(rows);
  }

  private Row validateRow(final Row row) {
    Assert.notNull(row, "Cannot add a null row to this table!");
    Assert.isTrue(row.size() == columns.size(), "The size of the Row ({0}) must must be equal to the number of Columns ({1}) in this Table!",
      row.size(), columns.size());
    return row;
  }

  private Object validateValue(final Object value) {
    return value;
  }

  protected class InMemoryColumn<T> extends AbstractColumn<T> {

    public InMemoryColumn(final Column<T> column) {
      super(column);
    }

    @Override
    public final View getView() {
      return InMemoryTable.this;
    }

    @Override
    public final void setView(final View view) {
      throw new UnsupportedOperationException(StringUtils.NOT_IMPLEMENTED);
    }
  }

  protected class InMemoryRow extends AbstractRow {

    private Object[] values;

    public InMemoryRow(final Row row) {
      values = new Object[columns.size()];
      for (int index = 0; index < values.length; index++) {
        values[index] = row.getValue(columns.get(index));
      }
    }

    @Override
    public Object getValue(final int columnIndex) {
      return values[columnIndex];
    }

    @Override
    public Object setValue(final int columnIndex, final Object value) {
      Object currentValue = values[columnIndex];
      values[columnIndex] = validateValue(value);
      return currentValue;
    }

    @Override
    public final View getView() {
      return InMemoryTable.this;
    }

    @Override
    public final void setView(final View view) {
      throw new UnsupportedOperationException(StringUtils.NOT_IMPLEMENTED);
    }

    @Override
    public Iterator<Object> iterator() {
      return ArrayUtils.iterator(values);
    }
  }

}
