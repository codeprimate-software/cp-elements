/*
 * Copyright 2016 Author or Authors.
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

import org.cp.elements.lang.Constants;

/**
 * The AbstractTable class is an abstract base class implementation of the Table interface encapsulating functionality
 * common to all Table implementations.
 * <p/>
 * @author John J. Blum
 * @see AbstractView
 * @see Column
 * @see Row
 * @see Table
 * @see View
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractTable extends AbstractView implements Table {

  /**
   * Adds the given {@link Column} to this {@link Table}.
   *
   * @param column {@link Column} to add.
   * @return a boolean value indicating whether the added {@link Column}
   * successfully modified the structure of this {@link Table}.
   * @see org.cp.elements.data.struct.tabular.Column
   */
  @Override
  public boolean add(Column column) {
    throw new UnsupportedOperationException(Constants.OPERATION_NOT_SUPPORTED);
  }

  /**
   * Adds the given {@link Row} to this {@link Table}.
   *
   * @param row {@link Row} to add.
   * @return a boolean value indicating whether the added {@link Row}
   * successfully modified the structure of this {@link Table}.
   * @see org.cp.elements.data.struct.tabular.Row
   */
  @Override
  public boolean add(Row row) {
    throw new UnsupportedOperationException(Constants.OPERATION_NOT_SUPPORTED);
  }

  /**
   * Removes the {@link Column} at the given {@link Integer index} from this {@link Table}.
   *
   * @param index {@link Integer} value indicating the index of the {@link Column} to remove.
   * @return a boolean value indicating whether the {@link Column} at {@link Integer index}
   * was successfully removed.
   * @throws IndexOutOfBoundsException if the given {@link Integer index} is not a valid {@link Column} index
   * in this {@link Table}.
   */
  @Override
  public boolean removeColumn(int index) {
    throw new UnsupportedOperationException(Constants.OPERATION_NOT_SUPPORTED);
  }

  /**
   * Removes the {@link Row} at the given {@link Integer index} from this {@link Table}.
   *
   * @param index {@link Integer} value indicating the index of the {@link Row} to remove.
   * @return a boolean value indicating whether the given {@link Row} at {@link Integer index}
   * was successfully removed.
   */
  @Override
  public boolean removeRow(int index) {
    throw new UnsupportedOperationException(Constants.OPERATION_NOT_SUPPORTED);
  }
}
