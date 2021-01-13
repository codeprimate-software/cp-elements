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
import static org.cp.elements.util.CollectionUtils.nullSafeIterable;

import java.util.Collections;
import java.util.Iterator;

import org.cp.elements.lang.Assert;

/**
 * {@link AbstractView} is an abstract base class implementing the {@link View} interface to provide functionality
 * common to all {@link View} implementations.
 *
 * @author John J. Blum
 * @see org.cp.elements.data.struct.tabular.Column
 * @see org.cp.elements.data.struct.tabular.Row
 * @see org.cp.elements.data.struct.tabular.Table
 * @see org.cp.elements.data.struct.tabular.View
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractView implements View {

  /**
   * Factory method constructing a new {@link View} initialized with the given {@link Column Columns}
   * and {@link Row Rows}.
   *
   * @param columns {@link Column Columns} defining the projection of the {@link View}.
   * @param rows {@link Row Rows} defining the contents of thew {@link View}.
   * @return a new {@link View} initialized with the given {@link Column Columns} and {@link Row Rows}.
   * @throws IllegalArgumentException if {@link Column Columns} are {@literal null} or empty.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see org.cp.elements.data.struct.tabular.Row
   * @see java.lang.Iterable
   */
  public static View of(Iterable<Column> columns, Iterable<Row> rows) {

    Assert.notNull(columns, "Columns [null] are required");
    Assert.isTrue(columns.iterator().hasNext(), "Columns [empty] are required");

    return new AbstractView() {

      @Override
      public Iterable<Column> columns() {
        return columns;
      }

      @Override
      public Iterator<Row> iterator() {
        return nullSafeIterable(rows).iterator();
      }
    };
  }

  private String name;

  /**
   * Sets the {@link String name} of this {@link View}.
   *
   * @param name {@link String} containing the name of this {@link View}.
   */
  public void setName(String name) {
    this.name = name;
  }

  /**
   * Returns the {@link String name} of this {@link View}.
   *
   * @return a {@link String} containing the name of this {@link View}.
   */
  @Override
  public String getName() {
    return this.name;
  }

  /**
   * Returns an {@link Iterable} to iterate over the {@link Column Columns} in this {@link View}.
   *
   * @return an {@link Iterable} to iterate over the {@link Column Columns} in this {@link View}.
   * @see org.cp.elements.data.struct.tabular.Column
   * @see java.lang.Iterable
   */
  @Override
  public Iterable<Column> columns() {
    throw newIllegalStateException("Columns for this View have not been defined");
  }

  /**
   * Returns an {@link Iterator} to iterate over the rows in this {@link View}.
   *
   * @return an {@link Iterator} to iterate over the rows in this {@link View}.
   * @see java.util.Iterator
   */
  @Override
  public Iterator<Row> iterator() {
    return Collections.emptyIterator();
  }

  /**
   * Builder method used to set the {@link String name} of this {@link View}.
   *
   * @param <T> {@link Class sub-type} of this {@link View}.
   * @param name {@link String} containing the name of this {@link View}.
   * @return this {@link View}.
   * @see #setName(String)
   */
  @SuppressWarnings("unchecked")
  public <T extends AbstractView> T named(String name) {
    setName(name);
    return (T) this;
  }

  /**
   * Returns a {@link String} representation of this {@link View}.
   *
   * By default, this method returns the {@link String name} of this {@link View}
   * by calling {@link #getName()}.
   *
   * @return a {@link String} describing this {@link View}.
   * @see java.lang.Object#toString()
   * @see #getName()
   */
  @Override
  public String toString() {
    return getName();
  }
}
