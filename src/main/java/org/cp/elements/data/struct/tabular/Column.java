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

import java.util.Optional;

import org.cp.elements.lang.Nameable;

/**
 * The {@link Column} interface is an Abstract Data Type (ADT) modeling a column in a tabular data structure.
 *
 * @author John J. Blum
 * @param <T> {@link Class type} of {@link Object values} stored in this {@link Column}.
 * @see org.cp.elements.data.struct.tabular.Row
 * @see org.cp.elements.data.struct.tabular.Table
 * @see org.cp.elements.data.struct.tabular.View
 * @see org.cp.elements.lang.Nameable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Column<T> extends Nameable<String> {

  /**
   * Sets the {@link String alias}, or alternate name for referring to this {@link Column}.
   *
   * @param alias {@link String} containing the alias, or alternate name for this {@link Column}.
   */
  void setAlias(String alias);

  /**
   * Returns an {@link Optional} {@link String alias}, or alternate name for referring to this {@link Column}.
   *
   * @return an {@link Optional} {@link String alias}, or alternate name for referring to this {@link Column}.
   * @see java.util.Optional
   * @see #getName()
   */
  Optional<String> getAlias();

  /**
   * Sets the {@link Object default value} used when a {@link Object value} is not specified for this {@link Column}.
   *
   * @param defaultValue {@link Object default value} used when a {@link Object value} is not specified
   * for this {@link Column}.
   */
  void setDefaultValue(T defaultValue);

  /**
   * Returns an {@link Optional} {@link Object default value} used when a {@link Object value}
   * is not specified for this {@link Column}.
   *
   * @return an {@link Optional} {@link Object default value} used when a {@link Object value}
   * is not specified for this {@link Column}.
   * @see java.util.Optional
   */
  Optional<T> getDefaultValue();

  /**
   * Sets a {@link String} to describe the data stored by this {@link Column}.
   *
   * @param description {@link String} containing the description for this {@link Column}.
   */
  void setDescription(String description);

  /**
   * Returns an {@link Optional} {@link String} to describe the data stored by this {@link Column}.
   *
   * @return an {@link Optional} {@link String} to describe the data stored by this {@link Column}.
   * @see java.util.Optional
   * @see #getAlias()
   * @see #getName()
   */
  Optional<String> getDescription();

  /**
   * Returns the {@link String name} of this {@link Column}.
   *
   * @return the {@link String name} of this {@link Column}.
   * @see org.cp.elements.lang.Nameable#getName()
   * @see #getDescription()
   * @see #getAlias()
   */
  @Override
  String getName();

  /**
   * Returns the {@link Class type} of {@link Object values} stored in this {@link Column}.
   *
   * @return {@link Class type} of {@link Object values} stored in this {@link Column}.
   * @see java.lang.Class
   */
  Class<T> getType();

  /**
   * Returns the {@link View} containing this {@link Column}.
   *
   * @return the {@link View} containing this {@link Column}.
   * @see org.cp.elements.data.struct.tabular.View
   */
  Optional<View> getView();

  /**
   * Returns the {@link Integer index} of this {@link Column} in the {@link View}.
   *
   * @return the {@link Integer index} of this {@link Column} in the {@link View}, or a {@literal -1}
   * this {@link Column} is not presently contained by a {@link View}.
   * @see org.cp.elements.data.struct.tabular.View#indexOf(Column)
   * @see #getView()
   */
  default int index() {

    return getView()
      .map(view -> view.indexOf(this))
      .orElse(-1);
  }
}
