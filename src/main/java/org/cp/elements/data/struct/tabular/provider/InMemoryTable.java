/*
 * Copyright 2011-Present Author or Authors.
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
package org.cp.elements.data.struct.tabular.provider;

import static org.cp.elements.lang.RuntimeExceptionsFactory.newUnsupportedOperationException;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
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
import org.cp.elements.util.ArrayUtils;

/**
 * The {@link InMemoryTable} class is an implementation of the {@link Table} interface
 * implementing a tabular data structure and storing data in the JVM Heap.
 *
 * @author John J. Blum
 * @see java.util.List
 * @see org.cp.elements.data.struct.tabular.AbstractColumn
 * @see org.cp.elements.data.struct.tabular.AbstractRow
 * @see org.cp.elements.data.struct.tabular.AbstractTable
 * @see org.cp.elements.data.struct.tabular.Column
 * @see org.cp.elements.data.struct.tabular.Row
 * @see org.cp.elements.data.struct.tabular.Table
 * @see org.cp.elements.data.struct.tabular.View
 * @since 1.0.0
 */
@SuppressWarnings({ "rawtypes", "unused" })
public class InMemoryTable extends AbstractTable {

  private final List<Column> columns;
  private final List<Row> rows;

  /**
   * Factory method used to construct a new instance of {@link InMemoryTable} initialized with
   * the given array of {@link Column columns}.
   *
   * @param columns {@link Column Columns} defining the structure of the {@link Table}.
   * @return a new instance of {@link InMemoryTable} initialized with {@link Column Columns}.
   * @throws IllegalArgumentException if {@link Column Columns} are {@literal null} or empty.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see #InMemoryTable(Column[])
   */
  public static InMemoryTable of(Column... columns) {
    return new InMemoryTable(columns);
  }

  /**
   * Factory method used to construct a new instance of {@link InMemoryTable} initialized with
   * the given {@literal Iterable} of {@link Column columns}.
   *
   * @param columns {@link Column Columns} defining the structure of the {@link Table}.
   * @return a new instance of {@link InMemoryTable} initialized with {@link Column Columns}.
   * @throws IllegalArgumentException if {@link Column Columns} are {@literal null} or empty.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see #InMemoryTable(Column[])
   * @see java.lang.Iterable
   */
  public static InMemoryTable of(Iterable<Column> columns) {

    Assert.notNull(columns, "Columns are required");

    Column[] columnArray = ArrayUtils.asArray(columns, Column.class);

    return new InMemoryTable(columnArray);
  }

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

    List<? extends Column> inMemoryColumns = (List<? extends Column>) Arrays.stream(columns)
      .map(InMemoryColumn::new)
      .collect(Collectors.toList());

    this.columns = new CopyOnWriteArrayList<>(inMemoryColumns);
    this.rows = Collections.synchronizedList(new ArrayList<>());
  }

  /**
   * Returns the {@link Column Columns} in this {@link Table}.
   *
   * @return a {@link List} of {@link Column Columns} in this {@link Table}.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see java.util.List
   */
  protected List<Column> getColumns() {
    return this.columns;
  }

  /**
   * Returns the {@link Row Rows} in this {@link Table}.
   *
   * @return a {@link List} of {@link Row Rows} in this {@link Table}.
   * @see org.cp.elements.data.struct.tabular.Row
   * @see java.util.List
   */
  protected List<Row> getRows() {
    return this.rows;
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
  @SuppressWarnings("all")
  public boolean add(Column column) {

    if (getColumns().add(validateColumn(column))) {
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
    return getRows().add(newRow(validateRow(row)));
  }

  /**
   * Constructs a new instance of {@link InMemoryTable.InMemoryRow} initialized with
   * a copy of the existing {@link Row}.
   *
   * @param <T> {@link Class sub-type} of the {@link Row}.
   * @param row {@link Row} to copy.
   * @return a new instance of {@link InMemoryRow}.
   * @see org.cp.elements.data.struct.tabular.provider.InMemoryTable.InMemoryRow
   * @see org.cp.elements.data.struct.tabular.Row
   */
  @SuppressWarnings("unchecked")
  protected <T extends Row> T newRow(Row row) {
    return (T) new InMemoryRow(row);
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
    return Collections.unmodifiableList(getColumns());
  }

  /**
   * Returns an {@literal Iterator} over the {@link Row Rows} in this {@link Table}.
   *
   * @return an {@literal Iterator} over the {@link Row Rows} in this {@link Table}.
   * @see #rows()
   */
  @Override
  public Iterator<Row> iterator() {
    return rows().iterator();
  }

  /**
   * Removes the {@link Column} at the given {@link Integer index} from this {@link Table}.
   *
   * @param index {@link Integer} value indicating the index of the {@link Column} to remove.
   * @return a boolean value if the {@link Column} was successfully removed.
   * @throws IndexOutOfBoundsException if the {@link Integer index} is not a valid {@link Column} index
   * in this {@link Table}
   */
  @Override
  public boolean removeColumn(int index) {

    if (getColumns().remove(index) != null) {
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
   * @return a boolean value if the {@link Row} was successfully removed.
   * @throws IndexOutOfBoundsException if the {@link Integer index} is not a valid {@link Row} index
   * in this {@link Table}
   */
  @Override
  public boolean removeRow(int index) {
    return getRows().remove(index) != null;
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
    return Collections.unmodifiableList(getRows());
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
   * @param value {@link Object} value to evaluate.
   * @return the given {@link Object value}.
   * @see java.lang.Object
   */
  protected Object validateValue(Object value) {
    return value;
  }

  protected class InMemoryColumn<T> extends AbstractColumn<T> {

    protected InMemoryColumn(Column<T> column) {
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

    protected InMemoryRow(Row row) {

      int columnSize = InMemoryTable.this.getColumns().size();

      this.values = new Object[columnSize];

      for (int index = 0; index < columnSize; index++) {
        this.values[index] = validateValue(row.getValue(index));
      }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getValue(int columnIndex) {
      return (T) this.values[columnIndex];
    }

    @Override
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

    synchronized void addColumn() {

      int columnsSize = InMemoryTable.this.getColumns().size();
      int valuesLength = this.values.length;

      if (valuesLength < columnsSize) {

        Object[] localValues = new Object[columnsSize];

        System.arraycopy(this.values, 0, localValues, 0, valuesLength);

        this.values = localValues;
      }
    }

    synchronized void removeColumn(int index) {

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

    @Override
    public synchronized Object[] values() {
      return ArrayUtils.shallowCopy(this.values);
    }
  }
}
