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

import java.util.Optional;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.StringUtils;

/**
 * The {@link AbstractColumn} class is an abstract base class implementing the {@link Column} interface,
 * encapsulating common state and functionality for implementing {@link Column} types.
 *
 * @author John J. Blum
 * @param <T> {@link Class type} of {@link Object values} stored in this {@link Column}.
 * @see org.cp.elements.data.struct.tabular.Column
 * @see org.cp.elements.data.struct.tabular.Row
 * @see org.cp.elements.data.struct.tabular.Table
 * @see org.cp.elements.data.struct.tabular.View
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractColumn<T> implements Column<T> {

  protected static final String COLUMN_TO_STRING =
    "{ @type = %1$s, name = %2$s, alias = %3$s, description = %4$s, type = %5$s, defaultValue = %6$s }";

  private T defaultValue;

  private final Class<T> type;

  private String alias;
  private String description;

  private final String name;

  private View view;

  /**
   * Constructs a new instance of {@link AbstractColumn} initialized with
   * the given {@link String name} and {@link Class type}.
   *
   * @param name {@link String} containing the name for this {@link Column}; must not be {@literal null}.
   * @param type {@link Class} specifying the type of {@link Object values} stored by this {@link Column};
   * must not be {@literal null}.
   * @throws IllegalArgumentException if either {@link String name} or {@link Class type} are {@literal null}.
   */
  public AbstractColumn(String name, Class<T> type) {

    Assert.hasText(name, "Column name is required");
    Assert.notNull(type, "Column type is required");

    this.name = name;
    this.type = type;
  }

  /**
   * Constructs a new instance of {@link AbstractColumn} initialized with the given {@link Column}.
   *
   * @param column {@link Column} to copy; must not be {@literal null}.
   * @throws IllegalArgumentException if the given {@link Column} is {@literal null}.
   * @see org.cp.elements.data.struct.tabular.Column
   */
  public AbstractColumn(Column<T> column) {

    Assert.notNull(column, "The Column to copy is required");

    this.name = column.getName();
    this.type = column.getType();

    column.getAlias().ifPresent(this::setAlias);
    column.getDefaultValue().ifPresent(this::setDefaultValue);
    column.getDescription().ifPresent(this::setDescription);
  }

  /**
   * Sets the {@link String alias}, or alternate name for referring to this {@link Column}.
   *
   * @param alias {@link String} containing the alias, or alternate name for this {@link Column}.
   */
  public void setAlias(String alias) {
    this.alias = alias;
  }

  /**
   * Returns an {@link Optional} {@link String alias}, or alternate name for referring to this {@link Column}.
   *
   * @return an {@link Optional} {@link String alias}, or alternate name for referring to this {@link Column}.
   * @see java.util.Optional
   * @see #getName()
   */
  public Optional<String> getAlias() {
    return Optional.ofNullable(this.alias).filter(StringUtils::hasText);
  }

  /**
   * Sets the {@link Object default value} used when a {@link Object value} is not specified for this {@link Column}.
   *
   * @param defaultValue {@link Object default value} used when a {@link Object value} is not specified
   * for this {@link Column}.
   */
  public void setDefaultValue(T defaultValue) {
    this.defaultValue = defaultValue;
  }

  /**
   * Returns an {@link Optional} {@link Object default value} used when a {@link Object value}
   * is not specified for this {@link Column}.
   *
   * @return an {@link Optional} {@link Object default value} used when a {@link Object value}
   * is not specified for this {@link Column}.
   * @see java.util.Optional
   */
  public Optional<T> getDefaultValue() {
    return Optional.ofNullable(this.defaultValue);
  }

  /**
   * Sets a {@link String} to describe the data stored by this {@link Column}.
   *
   * @param description {@link String} containing the description for this {@link Column}.
   */
  public void setDescription(String description) {
    this.description = description;
  }

  /**
   * Returns an {@link Optional} {@link String} to describe the data stored by this {@link Column}.
   *
   * @return an {@link Optional} {@link String} to describe the data stored by this {@link Column}.
   * @see java.util.Optional
   * @see #getAlias()
   * @see #getName()
   */
  public Optional<String> getDescription() {
    return Optional.ofNullable(this.description).filter(StringUtils::hasText);
  }

  /**
   * Returns the {@link String name} of this {@link Column}.
   *
   * @return the {@link String name} of this {@link Column}.
   * @see org.cp.elements.lang.Nameable#getName()
   */
  public String getName() {
    return this.name;
  }

  /**
   * Returns the {@link Class type} of {@link Object values} stored in this {@link Column}.
   *
   * @return {@link Class type} of {@link Object values} stored in this {@link Column}.
   * @see java.lang.Class
   */
  public Class<T> getType() {
    return this.type;
  }

  /**
   * Set the {@link View} containing this {@link Column}.
   *
   * @param view {@link View} containing this {@link Column}.
   * @see org.cp.elements.data.struct.tabular.View
   */
  public void setView(View view) {
    this.view = view;
  }

  /**
   * Returns an {@link Optional} {@link View} containing this {@link Column}.
   *
   * @return an {@link Optional} {@link View} containing this {@link Column}.
   * @see org.cp.elements.data.struct.tabular.View
   * @see java.util.Optional
   */
  public Optional<View> getView() {
    return Optional.ofNullable(this.view);
  }

  /**
   * Builder method used to set a {@link String description} of this {@link Column}.
   *
   * @param <S> {@link Class Sub-type} of this {@link Column}.
   * @param description {@link String} describing this {@link Column}.
   * @return this {@link Column}.
   * @see #setDescription(String)
   */
  @SuppressWarnings("unchecked")
  public <S extends AbstractColumn<T>> S describedAs(String description) {
    setDescription(description);
    return (S) this;
  }

  /**
   * Builder methods used to set the {@link Object default value} used when a {@link Object value}
   * is not specified for this {@link Column}.
   *
   * @param <S> {@link Class Sub-type} of this {@link Column}.
   * @param defaultValue {@link Object default value} used when a {@link Object value}
   * is not specified for this {@link Column}.
   * @return this {@link Column}.
   * @see #setDefaultValue(Object)
   */
  @SuppressWarnings("unchecked")
  public <S extends AbstractColumn<T>> S usingDefaultValue(T defaultValue) {
    setDefaultValue(defaultValue);
    return (S) this;
  }

  /**
   * Builder method used to set the {@link String alias}, or alternate name for this {@link Column}.
   *
   * @param <S> {@link Class Sub-type} of this {@link Column}.
   * @param alias {@link String} containing the alias, or alternate name for this {@link Column}.
   * @return this {@link Column}.
   * @see #setAlias(String)
   */
  @SuppressWarnings("unchecked")
  public <S extends AbstractColumn<T>> S withAlias(String alias) {
    setAlias(alias);
    return (S) this;
  }

  /**
   * Determines whether this {@link Column} is equal to the given {@link Object}.
   *
   * @param obj {@link Object} to compare for equality with this {@link Column}.
   * @return a boolean value indicating whether this {@link Column} is equal to
   * the given {@link Object}
   * @see java.lang.Object#equals(Object)
   */
  @Override
  public boolean equals(Object obj) {

    if (this == obj) {
      return true;
    }

    if (!(obj instanceof Column)) {
      return false;
    }

    Column that = (Column) obj;

    return this.getName().equals(that.getName());
  }

  /**
   * Computes the {@link Integer hash value} for this {@link Column}.
   *
   * @return the computed {@link Integer hash value} for this {@link Column}.
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode() {

    int hashValue = 17;

    hashValue = 37 * hashValue + getName().hashCode();

    return hashValue;
  }

  /**
   * Returns a {@link String} representation of this {@link Column}.
   *
   * @return a {@link String} describing this {@link Column}.
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {

    return String.format(COLUMN_TO_STRING,
      getClass().getName(), getName(), getAlias().orElse(null), getDescription().orElse(null), getType(),
        getDefaultValue().orElse(null));
  }
}
