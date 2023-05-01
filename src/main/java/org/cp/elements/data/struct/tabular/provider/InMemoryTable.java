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
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.util.ArrayUtils;

/**
 * Implementation of the {@link Table} interface implementing a tabular data structure and storing data in the JVM Heap.
 *
 * The {@link InMemoryTable} is an ordered collection of {@link Column Columns} and {@link Row Rows} along with
 * a data set populating the {@literal table}.
 *
 * @author John Blum
 * @see java.util.List
 * @see org.cp.elements.data.struct.tabular.AbstractColumn
 * @see org.cp.elements.data.struct.tabular.AbstractRow
 * @see org.cp.elements.data.struct.tabular.AbstractTable
 * @see org.cp.elements.data.struct.tabular.AbstractView
 * @see org.cp.elements.data.struct.tabular.Column
 * @see org.cp.elements.data.struct.tabular.Row
 * @see org.cp.elements.data.struct.tabular.Table
 * @see org.cp.elements.data.struct.tabular.View
 * @since 1.0.0
 */
@SuppressWarnings({ "rawtypes", "unused" })
public class InMemoryTable extends AbstractTable {

  protected static final int DEFAULT_ROW_COUNT = 100;

  private final List<Column<?>> columns;
  private final List<Row> rows;

  /**
   * Factory method used to construct a new instance of {@link InMemoryTable} initialized with
   * the given, required array of {@link Column Columns}.
   *
   * @param columns array of {@link Column Columns} defining the structure of the {@link Table}.
   * @return a new {@link InMemoryTable} initialized with the given, required {@link Column Columns}
   * used to define the structure of the {@link Table}.
   * @throws IllegalArgumentException if the given array of {@link Column Columns} is {@literal null}
   * or {@literal empty}.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see #InMemoryTable(Column[])
   */
  public static @NotNull InMemoryTable of(@NotNull Column... columns) {
    return new InMemoryTable(columns);
  }

  /**
   * Factory method used to construct a new instance of {@link InMemoryTable} initialized with
   * the given, required {@literal Iterable} of {@link Column Columns}.
   *
   * @param columns {@link Iterable} of {@link Column Columns} defining the structure of the {@link Table}.
   * @return a new {@link InMemoryTable} initialized with the given, required {@link Column Columns}
   * used to define the structure of the {@link Table}.
   * @throws IllegalArgumentException if given {@link Iterable} of {@link Column Columns} is {@literal null}
   * or {@literal empty}.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see #InMemoryTable(Column[])
   * @see java.lang.Iterable
   */
  public static @NotNull InMemoryTable of(@NotNull Iterable<Column> columns) {

    Assert.notNull(columns, "Columns are required");

    Column[] columnArray = ArrayUtils.asArray(columns, Column.class);

    return new InMemoryTable(columnArray);
  }

  /**
   * Constructs a new instance of {@link InMemoryTable} initialized with the given, required
   * array of {@link Column Columns} defining the structure for this {@link Table}.
   *
   * @param columns array of {@link Column Columns} defining the structure for this {@link Table}.
   * @throws IllegalArgumentException if the given array of {@link Column Columns} is {@literal null}
   * or {@literal empty}.
   * @see org.cp.elements.data.struct.tabular.Column
   */
  @SuppressWarnings("unchecked")
  public InMemoryTable(@NotNull Column... columns) {

    Assert.notEmpty(columns, "Columns are required");

    List<Column> inMemoryColumns = (List<Column>) Arrays.stream(columns)
      .map(this::newColumn)
      .collect(Collectors.toList());

    this.columns = new CopyOnWriteArrayList(inMemoryColumns);
    this.rows = Collections.synchronizedList(new ArrayList<>(DEFAULT_ROW_COUNT));
  }

  /**
   * Returns the {@link Column Columns} in this {@link Table}.
   *
   * @return a {@link List} of {@link Column Columns} in this {@link Table}.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see java.util.List
   */
  protected List<Column<?>> getColumns() {
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
   * @param column {@link Column} to add; must not able {@literal null}.
   * @return a boolean value indicating whether the given {@link Column}
   * was successfully added and modified the structure of this {@link Table}.
   * @throws IllegalArgumentException if the given {@link Column} is {@literal null}.
   * @see org.cp.elements.data.struct.tabular.provider.InMemoryTable.InMemoryRow#addColumn()
   * @see org.cp.elements.data.struct.tabular.Column
   * @see #validateColumn(Column)
   * @see #newColumn(Column)
   * @see #getColumns()
   */
  @Override
  @SuppressWarnings({ "unchecked", "all" })
  public boolean add(@NotNull Column column) {

    if (getColumns().add(newColumn(validateColumn(column)))) {
      for (Row row : this) {
        ((InMemoryRow) row).addColumn();
      }

      return true;
    }

    return false;
  }

  /**
   * Constructs a new instance of {@link InMemoryTable.InMemoryColumn} initialized with
   * a copy of the existing, required {@link Column}.
   *
   * @param <T> concrete {@link Class type} of the {@link Column}; returns {@link InMemoryColumn}.
   * @param <TYPE> {@link Class type} of {@link Object values} stored in the {@link Column}.
   * @param column {@link Column} to copy; must not be {@literal null}.
   * @return a new {@link InMemoryColumn}.
   * @throws IllegalArgumentException if the given {@link Column} is {@literal null}.
   * @see org.cp.elements.data.struct.tabular.provider.InMemoryTable.InMemoryColumn
   * @see org.cp.elements.data.struct.tabular.Column
   */
  @SuppressWarnings("unchecked")
  protected <TYPE, T extends Column<TYPE>> T newColumn(@NotNull Column<TYPE> column) {
    return (T) new InMemoryColumn<>(column);
  }

  /**
   * Adds the given, required {@link Row} to the end of this {@link Table}.
   *
   * @param row {@link Row} to add; must not be {@literal null}.
   * @return a boolean value indicating whether the given {@link Row}
   * was successfully added to this {@link Table}.
   * @throws IllegalArgumentException if the given {@link Row} is {@literal null}.
   * @see org.cp.elements.data.struct.tabular.Row
   * @see #validateRow(Row)
   * @see #newRow(Row)
   * @see #getRows()
   */
  @Override
  public boolean add(@NotNull Row row) {
    return getRows().add(newRow(validateRow(row)));
  }

  /**
   * Constructs a new instance of {@link InMemoryTable.InMemoryRow} initialized with
   * a copy of the existing, required {@link Row}.
   *
   * @param <T> concrete {@link Class type} of the {@link Row}; returns {@link InMemoryRow}.
   * @param row {@link Row} to copy; must not be {@literal null}.
   * @return a new {@link InMemoryRow}.
   * @throws IllegalArgumentException if the given {@link Row} is {@literal null}.
   * @see org.cp.elements.data.struct.tabular.provider.InMemoryTable.InMemoryRow
   * @see org.cp.elements.data.struct.tabular.Row
   */
  @SuppressWarnings("unchecked")
  protected @NotNull <T extends Row> T newRow(@NotNull Row row) {
    return (T) new InMemoryRow(row);
  }

  /**
   * Returns an {@link Iterable} over the {@link Column Columns} in this {@link Table}.
   *
   * @return an unmodifiable {@link Iterable} over the {@link Column Columns} in this {@link Table}.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see java.lang.Iterable
   * @see #getColumns()
   */
  @Override
  public Iterable<Column<?>> columns() {
    return Collections.unmodifiableList(getColumns());
  }

  /**
   * Returns an {@literal Iterator} over the {@link Row Rows} in this {@link Table}.
   *
   * @return an unmodifiable {@literal Iterator} over the {@link Row Rows} in this {@link Table}.
   * @see org.cp.elements.data.struct.tabular.Row
   * @see java.util.Iterator
   * @see #rows()
   */
  @Override
  public Iterator<Row> iterator() {
    return rows().iterator();
  }

  /**
   * Removes the {@link Column} at the given {@link Integer index} from this {@link Table}.
   *
   * @param index {@link Integer value} specifying the {@literal index} of the {@link Column} to remove.
   * @return a boolean value if the {@link Column} at {@link Integer index} was successfully removed.
   * @throws IndexOutOfBoundsException if the {@link Integer index} is not a valid {@link Column} {@literal index}
   * in this {@link Table}
   * @see org.cp.elements.data.struct.tabular.provider.InMemoryTable.InMemoryRow#removeColumn(int)
   * @see #getColumns()
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
   * @param index {@link Integer value} specifying the {@literal index} of the {@link Row} to remove.
   * @return a boolean value if the {@link Row} was successfully removed.
   * @throws IndexOutOfBoundsException if the {@link Integer index} is not a valid {@link Row} {@literal index}
   * in this {@link Table}
   * @see #getRows()
   */
  @Override
  public boolean removeRow(int index) {
    return getRows().remove(index) != null;
  }

  /**
   * Returns an {@link Iterable} over the {@link Row Rows} in this {@link Table}.
   *
   * @return an unmodifiable {@link Iterable} over the {@link Row Rows} in this {@link Table}.
   * @see org.cp.elements.data.struct.tabular.Row
   * @see java.lang.Iterable
   * @see #getRows()
   */
  @Override
  public Iterable<Row> rows() {
    return Collections.unmodifiableList(getRows());
  }

  /**
   * Validates the given {@link Column}.
   *
   * By default, the {@link Column} must not be {@literal null}.
   *
   * @param column {@link Column} to validate.
   * @return the given {@link Column}.
   * @throws IllegalArgumentException if the given {@link Column} is {@literal null}.
   * @see org.cp.elements.data.struct.tabular.Column
   */
  protected @NotNull Column validateColumn(@NotNull Column column) {
    return ObjectUtils.requireObject(column, "Column is required");
  }

  /**
   * Validates the given {@link Row}.
   *
   * By default, the {@link Row} must not be {@literal null}.
   *
   * @param row {@link Row} to validate.
   * @return the given {@link Row}.
   * @throws IllegalArgumentException if the given {@link Row} is {@literal null}.
   * @see org.cp.elements.data.struct.tabular.Row
   */
  protected @NotNull Row validateRow(@NotNull Row row) {
    return ObjectUtils.requireObject(row, "Row is required");
  }

  /**
   * Validates the given {@link Object value} to be inserted into this {@link Table}.
   *
   * @param value {@link Object value} to evaluate.
   * @return the given {@link Object value}.
   * @see java.lang.Object
   */
  protected @Nullable Object validateValue(@Nullable Object value) {
    return value;
  }

  /**
   * Implementation of the {@link Column} interface modeling an {@literal in-memory column}
   * for an {@link InMemoryTable}.
   *
   * @param <T> {@link Class type} of {@link Object values} stored in this {@link Column}.
   * @see org.cp.elements.data.struct.tabular.AbstractColumn
   * @see org.cp.elements.data.struct.tabular.Column
   */
  protected class InMemoryColumn<T> extends AbstractColumn<T> {

    /**
     * Constructs a new instance of {@link InMemoryColumn} copied from the existing, required {@link Column}.
     *
     * @param column {@link Column} to copy; must not be {@literal null}.
     * @throws IllegalArgumentException if the given {@link Column} to copy is {@literal null}.
     * @see org.cp.elements.data.struct.tabular.Column
     */
    protected InMemoryColumn(@NotNull Column<T> column) {
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

    @Override
    public boolean equals(Object obj) {

      if (obj == this) {
        return true;
      }

      if (!(obj instanceof Column)) {
        return false;
      }

      Column<?> that = (Column<?>) obj;

      return ObjectUtils.equals(this.getName(), that.getName())
        && ObjectUtils.isNullOrEqualTo(that.getView().orElse(null), this.getView().orElse(null));
    }

    @Override
    public int hashCode() {
      return ObjectUtils.hashCodeOf(this.getName(), this.getView().orElse(null));
    }

    @Override
    public String toString() {
      return getName();
    }
  }

  /**
   * Implementation of the {@link Row} interface modeling an {@literal in-memory row} for an {@link InMemoryTable}.
   *
   * @see org.cp.elements.data.struct.tabular.AbstractRow
   * @see org.cp.elements.data.struct.tabular.Row
   */
  protected class InMemoryRow extends AbstractRow {

    private volatile Object[] values;

    /**
     * Constructs a new instance of {@link InMemoryRow} copied from the existing, required {@link Row}.
     *
     * @param row {@link Row} to copy; must not be {@literal null}.
     * @throws IllegalArgumentException if the given {@link Row} to copy is {@literal null}.
     * @see org.cp.elements.data.struct.tabular.Row
     */
    protected InMemoryRow(@NotNull Row row) {

      Assert.notNull(row, "Row is required");

      int columnSize = getColumnSize(InMemoryTable.this);

      this.values = new Object[columnSize];

      for (int index = 0; index < columnSize; index++) {
        this.values[index] = validateValue(row.getValue(index));
      }
    }

    int getColumnSize(@NotNull InMemoryTable table) {
      return table.getColumns().size();
    }

    @Override
    @SuppressWarnings("unchecked")
    public synchronized @Nullable <T> T getValue(int columnIndex) {
      return (T) this.values[columnIndex];
    }

    @Override
    @SuppressWarnings("unchecked")
    public synchronized @Nullable <T> T setValue(int columnIndex, @Nullable T value) {

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

      int columnsSize = getColumnSize(InMemoryTable.this);
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
