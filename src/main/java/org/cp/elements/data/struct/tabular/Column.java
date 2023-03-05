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

import org.cp.elements.lang.Integers;
import org.cp.elements.lang.Nameable;

/**
 * Abstract Data Type (ADT) modeling a column in a tabular data structure.
 *
 * @author John Blum
 * @param <T> {@link Class type} of {@link Object values} stored in this {@link Column}.
 * @see org.cp.elements.data.struct.tabular.Row
 * @see org.cp.elements.data.struct.tabular.Table
 * @see org.cp.elements.data.struct.tabular.View
 * @see org.cp.elements.lang.Nameable
 * @since 1.0.0
 */
public interface Column<T> extends Nameable<String> {

  /**
   * Returns an {@link Optional} {@link String alias} used as an {@link String alternate name}
   * when referring to this {@link Column}.
   *
   * Returns {@link Optional#empty()} by default.
   *
   * @return an {@link Optional} {@link String alias} used as an {@link String alternate name}
   * when referring to this {@link Column}.
   * @see java.util.Optional
   * @see #getName()
   */
  default Optional<String> getAlias() {
    return Optional.empty();
  }

  /**
   * Sets the {@link String alias} used as an {@link String alternate name} when referring to this {@link Column}.
   *
   * @param alias {@link String} containing the {@literal alias}, or {@link String alternate name},
   * used when referring to this {@link Column}.
   */
  void setAlias(String alias);

  /**
   * Returns an {@link Optional} {@link Object default value} used as the {@link Object value} for this {@link Column}
   * when a {@link Object value} is not provided.
   *
   * Returns {@link Optional#empty()} by default.
   *
   * @return an {@link Optional} {@link Object default value} used as the {@link Object value} for this {@link Column}
   * when a {@link Object value} is not provided.
   * @see java.util.Optional
   */
  default Optional<T> getDefaultValue() {
    return Optional.empty();
  }

  /**
   * Sets the {@link Object default value} used as the {@link Object value} for this {@link Column}
   * when a {@link Object value} is not provided.
   *
   * @param defaultValue {@link Object default value} used as the {@link Object value} for this {@link Column}
   * when a {@link Object value} is not provided.
   */
  void setDefaultValue(T defaultValue);

  /**
   * Returns an {@link Optional} {@link String description} of the data stored in this {@link Column}.
   *
   * Returns {@link Optional#empty()} by default.
   *
   * @return an {@link Optional} {@link String description} of the data stored in this {@link Column}.
   * @see java.util.Optional
   * @see #getAlias()
   * @see #getName()
   */
  default Optional<String> getDescription() {
    return Optional.empty();
  }

  /**
   * Sets a {@link String description} to describe the data stored in this {@link Column}.
   *
   * @param description {@link String} containing a {@literal description} for this {@link Column}.
   */
  void setDescription(String description);

  /**
   * Returns the {@link String name} of this {@link Column}.
   *
   * @return the {@link String name} of this {@link Column}.
   * @see org.cp.elements.lang.Nameable#getName()
   * @see #getDescription()
   * @see java.lang.String
   * @see #getAlias()
   */
  @Override
  String getName();

  /**
   * Returns the {@link Class type} of {@link Object values} stored in this {@link Column}.
   *
   * @return the {@link Class type} of {@link Object values} stored in this {@link Column}.
   * @see java.lang.Class
   */
  Class<T> getType();

  /**
   * Returns an {@link Optional} {@link View} containing this {@link Column}.
   *
   * This {@link Column} may not be necessarily defined inside the context of a {@link View}.
   *
   * Returns {@link Optional#empty()} by default.
   *
   * @return an {@link Optional} {@link View} containing this {@link Column}.
   * @see org.cp.elements.data.struct.tabular.View
   * @see java.util.Optional
   */
  default Optional<View> getView() {
    return Optional.empty();
  }

  /**
   * Returns the {@link Integer index} of this {@link Column} in the {@link View}.
   *
   * @return the {@link Integer index} of this {@link Column} in the {@link View}.
   * Returns a {@literal -1} if this {@link Column} is not present in the {@link View}.
   * @see org.cp.elements.data.struct.tabular.View#indexOf(Column)
   * @see #getView()
   */
  default int index() {

    return getView()
      .map(view -> view.indexOf(this))
      .orElse(Integers.MINUS_ONE);
  }
}
