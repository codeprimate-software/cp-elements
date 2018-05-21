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

package org.cp.elements.data.struct.tabular.provider;

import static org.cp.elements.lang.RuntimeExceptionsFactory.newUnsupportedOperationException;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.stream.Collectors;

import org.cp.elements.data.struct.tabular.AbstractColumn;
import org.cp.elements.data.struct.tabular.AbstractRow;
import org.cp.elements.data.struct.tabular.AbstractTable;
import org.cp.elements.data.struct.tabular.Column;
import org.cp.elements.data.struct.tabular.Row;
import org.cp.elements.data.struct.tabular.Table;
import org.cp.elements.data.struct.tabular.View;
import org.cp.elements.lang.Assert;

/**
 * The {@link InMemoryTable} class is an implementation of the {@link Table} interface
 * implementing a tabular data structure and storing data in the JVM Heap.
 *
 * @author John J. Blum
 * @see org.cp.elements.data.struct.tabular.AbstractTable
 * @see org.cp.elements.data.struct.tabular.Column
 * @see org.cp.elements.data.struct.tabular.Row
 * @see org.cp.elements.data.struct.tabular.Table
 * @see org.cp.elements.data.struct.tabular.View
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class InMemoryTable extends AbstractTable {

  private final List<Column> columns;
  private final List<Row> rows;

  /**
   * Constructs a new instance of {@link InMemoryTable} initialized with the given array of {@link Column Columns}
   * defining the {@link Table} structure.
   *
   * @param columns array of {@link Column Columns} defining the structure of this {@link Table}.
   * @throws IllegalArgumentException if the array of {@link Column Columns} are {@literal null} or empty.
   * @see org.cp.elements.data.struct.tabular.Column
   */
  @SuppressWarnings("unchecked")
  public InMemoryTable(Column... columns) {

    Assert.notEmpty(columns, "Columns are required");

    List<Column> inMemoryColumns = (List<Column>) Arrays.stream(columns)
      .map(InMemoryColumn::new)
      .collect(Collectors.toList());

    this.columns = new CopyOnWriteArrayList<>(inMemoryColumns);
    this.rows = Collections.synchronizedList(new ArrayList<>());
  }

  /**
   * Adds a new {@link Column} to this {@link Table}.
   *
   * @param column {@link Column} to add.
   * @return a boolean value indicating whether the {@link Column} addition
   * successfully modified the structure of this {@link Table}.
   * @throws IllegalArgumentException if {@link Column} is {@literal null}.
   * @see org.cp.elements.data.struct.tabular.provider.InMemoryTable.InMemoryRow#addColumn()
   * @see org.cp.elements.data.struct.tabular.Column
   */
  @Override
  public boolean add(Column column) {

    if (this.columns.add(validateColumn(column))) {
      for (Row row : this) {
        ((InMemoryRow) row).addColumn();
      }

      return true;
    }

    return false;
  }

  /**
   * Adds the given {@link Row} to the end of this {@link Table}.
   *
   * @param row {@link Row} to add.
   * @return a boolean value indicating whether the given {@link Row}
   * was successfully added to this {@link Table}.
   * @throws IllegalArgumentException if {@link Row} is {@literal null}.
   * @see org.cp.elements.data.struct.tabular.Row
   */
  @Override
  public boolean add(Row row) {
    return this.rows.add(new InMemoryRow(validateRow(row)));
  }

  /**
   * Iterates over the {@link Column Columns} in this {@link Table}.
   *
   * @return an unmodifiable, {@link Iterable} object over the {@link Column Columns} in this {@link Table}.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see java.lang.Iterable
   * @see #rows()
   */
  @Override
  public Iterable<Column> columns() {
    return Collections.unmodifiableList(this.columns);
  }

  /**
   * Removes the {@link Column} at the given {@link Integer index} from this {@link Table}.
   *
   * @param index {@link Integer} value indicating the index of the {@link Column} to remove.
   * @throws IndexOutOfBoundsException if the {@link Integer index} is not a valid {@link Column} index
   * in this {@link Table}
   * @return a boolean value if the {@link Column} was successfully removed.
   */
  @Override
  public boolean removeColumn(int index) {

    if (this.columns.remove(index) != null) {
      for (Row row : this) {
        ((InMemoryRow) row).removeColumn(index);
      }

      return true;
    }

    return false;
  }

  /**
   * Removes the {@link Row} at the given {@link Integer index} from this {@link Table}.
   *
   * @param index {@link Integer} value indicating the index of the {@link Row} to remove.
   * @throws IndexOutOfBoundsException if the {@link Integer index} is not a valid {@link Row} index
   * in this {@link Table}
   * @return a boolean value if the {@link Row} was successfully removed.
   */
  @Override
  public boolean removeRow(int index) {
    return this.rows.remove(index) != null;
  }

  /**
   * Iterates over the {@link Row Rows} in this {@link Table}.
   *
   * @return an unmodifiable, {@link Iterable} object over the {@link Row Rows} in this {@link Table}.
   * @see org.cp.elements.data.struct.tabular.Row
   * @see java.lang.Iterable
   * @see #columns()
   */
  @Override
  public Iterable<Row> rows() {
    return Collections.unmodifiableList(this.rows);
  }

  /**
   * Validates the given {@link Column}.
   *
   * @param column {@link Column} to validate.
   * @return the given {@link Column}.
   * @throws IllegalArgumentException if the {@link Column} is {@literal null}.
   * @see org.cp.elements.data.struct.tabular.Column
   */
  protected Column validateColumn(Column column) {

    Assert.notNull(column, "Column is required");

    return column;
  }

  /**
   * Validates the given {@link Row}.
   *
   * @param row {@link Row} to validate.
   * @return the given {@link Row}.
   * @throws IllegalArgumentException if the {@link Row} is {@literal null}.
   * @see org.cp.elements.data.struct.tabular.Row
   */
  protected Row validateRow(Row row) {

    Assert.notNull(row, "Row is required");

    return row;
  }

  /**
   * Validates the given {@link Object value} to insert into this {@link Table}.
   *
   * @param value {@link Object} to evaluate.
   * @return the given {@link Object value}.
   * @see java.lang.Object
   */
  protected Object validateValue(Object value) {
    return value;
  }

  protected class InMemoryColumn<T> extends AbstractColumn<T> {

    public InMemoryColumn(Column<T> column) {
      super(column);
    }

    @Override
    public final Optional<View> getView() {
      return Optional.of(InMemoryTable.this);
    }

    @Override
    public final void setView(View view) {
      throw newUnsupportedOperationException("The View for this Column [%s] cannot be changed", getName());
    }
  }

  protected class InMemoryRow extends AbstractRow {

    private volatile Object[] values;

    public InMemoryRow(Row row) {

      this.values = new Object[InMemoryTable.this.columns.size()];

      for (int index = 0; index < this.values.length; index++) {
        this.values[index] = validateValue(row.getValue(index));
      }
    }

    @SuppressWarnings("unchecked")
    public <T> T getValue(int columnIndex) {
      return (T) this.values[columnIndex];
    }

    @SuppressWarnings("unchecked")
    public <T> T setValue(int columnIndex, T value) {

      Object currentValue = this.values[columnIndex];

      this.values[columnIndex] = validateValue(value);

      return (T) currentValue;
    }

    @Override
    public final Optional<View> getView() {
      return Optional.of(InMemoryTable.this);
    }

    @Override
    public final void setView(View view) {
      throw newUnsupportedOperationException("The View for this Row [%d] cannot be changed", index());
    }

    void addColumn() {

      int columnsSize = InMemoryTable.this.columns.size();
      int valuesLength = this.values.length;

      if (valuesLength < columnsSize) {

        Object[] localValues = new Object[columnsSize];

        System.arraycopy(this.values, 0, localValues, 0, valuesLength);

        this.values = localValues;
      }
    }

    void removeColumn(int index) {

      int valuesLength = this.values.length;

      Object[] localValues = new Object[valuesLength - 1];

      if (index > 0) {
        System.arraycopy(this.values, 0, localValues, 0, index);
      }

      int adjustedIndex = index + 1;
      int remainingLength = valuesLength - adjustedIndex;

      if (remainingLength > 0) {
        System.arraycopy(this.values, adjustedIndex, localValues, index, remainingLength);
      }

      this.values = localValues;
    }
  }
}
