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

import org.cp.elements.lang.Constants;

/**
 * Abstract base class implementing the {@link Table} interface encapsulating functionality common to
 * all {@link Table} implementations.
 *
 * @author John J. Blum
 * @see org.cp.elements.data.struct.tabular.AbstractView
 * @see org.cp.elements.data.struct.tabular.Column
 * @see org.cp.elements.data.struct.tabular.Row
 * @see org.cp.elements.data.struct.tabular.Table
 * @see org.cp.elements.data.struct.tabular.View
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractTable extends AbstractView implements Table {

  @Override
  @SuppressWarnings("rawtypes")
  public boolean add(Column column) {
    throw new UnsupportedOperationException(Constants.OPERATION_NOT_SUPPORTED);
  }

  @Override
  public boolean add(Row row) {
    throw new UnsupportedOperationException(Constants.OPERATION_NOT_SUPPORTED);
  }
}
