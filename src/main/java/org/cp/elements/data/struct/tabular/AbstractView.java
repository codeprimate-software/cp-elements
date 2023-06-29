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
package org.cp.elements.data.struct.tabular;

import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalStateException;

import java.util.Collections;
import java.util.Iterator;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.util.ArrayUtils;
import org.cp.elements.util.CollectionUtils;

/**
 * Abstract base class implementing the {@link View} interface to provide functionality
 * common to all {@link View} implementations.
 *
 * @author John Blum
 * @see org.cp.elements.data.struct.tabular.Column
 * @see org.cp.elements.data.struct.tabular.Row
 * @see org.cp.elements.data.struct.tabular.Table
 * @see org.cp.elements.data.struct.tabular.View
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractView implements View {

  /**
   * Factory method used to construct a new instance of {@link AbstractView} initialized with the given,
   * required {@link Iterable} of {@link Column Columns} defining the structure of the {@link View}
   * and the given array of {@link Row Rows} constituting the data for the {@link View}.
   *
   * @param columns {@link Iterable} of {@link Column Columns} defining the projection or structure
   * of the new {@link View}; must not be {@literal null} or {@literal empty}.
   * @param rows array of {@link Row Rows} supplying the contents of the new {@link View}.
   * @return a new {@link View} initialized with the given {@link Column Columns} and {@link Row Rows}.
   * @throws IllegalArgumentException if the given {@link Column Columns} are {@literal null} or {@literal empty}.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see org.cp.elements.data.struct.tabular.Row
   * @see org.cp.elements.data.struct.tabular.View
   * @see #of(Iterable, Iterable)
   * @see java.lang.Iterable
   */
  public static @NotNull View of(@NotNull Iterable<Column<?>> columns, Row... rows) {
    return of(columns, ArrayUtils.asIterable(rows));
  }

  /**
   * Factory method used to construct a new instance of {@link AbstractView} initialized with the given,
   * required {@link Column Columns} defining the structure of the {@link View} and the given {@link Row Rows}
   * constituting the data for the {@link View}.
   *
   * @param columns {@link Column Columns} defining the projection or structure of the new {@link View};
   * must not be {@literal null} or {@literal empty}.
   * @param rows {@link Row Rows} defining the contents of the new {@link View}.
   * @return a new {@link View} initialized with the given {@link Column Columns} and {@link Row Rows}.
   * @throws IllegalArgumentException if the given {@link Column Columns} are {@literal null} or {@literal empty}.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see org.cp.elements.data.struct.tabular.Row
   * @see org.cp.elements.data.struct.tabular.View
   * @see java.lang.Iterable
   */
  public static @NotNull View of(@NotNull Iterable<Column<?>> columns, @Nullable Iterable<Row> rows) {

    Assert.notEmpty(columns, "Columns are required");

    return new AbstractView() {

      @Override
      public Iterable<Column<?>> columns() {
        return columns;
      }

      @Override
      public Iterator<Row> iterator() {
        return CollectionUtils.nullSafeIterable(rows).iterator();
      }
    };
  }

  private volatile String name;

  /**
   * Sets the {@link String name} of this {@link View}.
   *
   * @param name {@link String} containing the {@literal name} for this {@link View}.
   * @see #named(String)
   * @see #getName()
   */
  public void setName(@Nullable String name) {
    this.name = name;
  }

  /**
   * Returns the {@link String name} of this {@link View}.
   *
   * @return the {@link String name} of this {@link View}.
   * @see #setName(String)
   */
  @Override
  public @Nullable String getName() {
    return this.name;
  }

  @Override
  public Iterable<Column<?>> columns() {
    throw newIllegalStateException("Columns for this View have not been defined");
  }

  @Override
  public Iterator<Row> iterator() {
    return Collections.emptyIterator();
  }

  /**
   * Builder method used to set the {@link String name} of this {@link View}.
   *
   * @param <T> concrete {@link Class type} of this {@link View}.
   * @param name {@link String} containing the {@literal name} for this {@link View}.
   * @return this {@link View}.
   * @see #setName(String)
   */
  @SuppressWarnings("unchecked")
  public @NotNull <T extends AbstractView> T named(@Nullable String name) {
    setName(name);
    return (T) this;
  }

  /**
   * Returns a {@link String} representation of this {@link View}.
   * <p>
   * By default, this method returns the {@link #getName()} of this {@link View}.
   *
   * @return a {@link String} describing this {@link View}.
   * @see java.lang.Object#toString()
   * @see #getName()
   */
  @Override
  public @NotNull String toString() {
    return getName();
  }
}
