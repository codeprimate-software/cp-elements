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

import java.util.Objects;
import java.util.Optional;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Abstract base class implementing the {@link Column} interface, encapsulating state and functionality common
 * to all {@link Column} implementations.
 *
 * @author John Blum
 * @param <T> {@link Class type} of {@link Object values} stored in this {@link Column}.
 * @see java.lang.Comparable
 * @see org.cp.elements.data.struct.tabular.Column
 * @see org.cp.elements.data.struct.tabular.Row
 * @see org.cp.elements.data.struct.tabular.Table
 * @see org.cp.elements.data.struct.tabular.View
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractColumn<T> implements Column<T>, Comparable<Column<T>> {

  protected static final String COLUMN_TO_STRING =
    "{ @type = %1$s, name = %2$s, alias = %3$s, description = %4$s, type = %5$s, defaultValue = %6$s, view = %7$s }";

  private T defaultValue;

  private final Class<T> type;

  private String alias;
  private String description;

  private final String name;

  private View view;

  /**
   * Constructs a new instance of {@link AbstractColumn} initialized with the given, required {@link String name}
   * and {@link Class type}.
   *
   * @param name {@link String} containing the {@literal name} of this {@link Column};
   * must not be {@literal null} or {@literal empty}.
   * @param type {@link Class} declaring the {@literal type} of {@link Object values}
   * stored in this {@link Column}; must not be {@literal null}.
   * @throws IllegalArgumentException if either {@link String name} or {@link Class type} are {@literal null},
   * or the {@link String name} is {@literal empty}.
   */
  public AbstractColumn(@NotNull String name, @NotNull Class<T> type) {

    this.name = StringUtils.requireText(name, "Name [%s] is required");
    this.type = ObjectUtils.requireObject(type, "Type is required");
  }

  /**
   * Constructs a new instance of {@link AbstractColumn} copied from the given, required {@link Column}.
   *
   * @param column {@link Column} to copy; must not be {@literal null}.
   * @throws IllegalArgumentException if the given {@link Column} is {@literal null}.
   * @see org.cp.elements.data.struct.tabular.Column
   */
  public AbstractColumn(@NotNull Column<T> column) {

    Assert.notNull(column, "The Column to copy is required");

    this.name = column.getName();
    this.type = column.getType();

    column.getAlias().ifPresent(this::setAlias);
    column.getDefaultValue().ifPresent(this::setDefaultValue);
    column.getDescription().ifPresent(this::setDescription);
  }

  @Override
  public Optional<String> getAlias() {

    return Optional.ofNullable(this.alias)
      .filter(StringUtils::hasText);
  }

  @Override
  public void setAlias(@Nullable String alias) {
    this.alias = alias;
  }

  @Override
  public Optional<T> getDefaultValue() {
    return Optional.ofNullable(this.defaultValue);
  }

  @Override
  public void setDefaultValue(@Nullable T defaultValue) {
    this.defaultValue = defaultValue;
  }

  @Override
  public Optional<String> getDescription() {

    return Optional.ofNullable(this.description)
      .filter(StringUtils::hasText);
  }

  @Override
  public void setDescription(@Nullable String description) {
    this.description = description;
  }

  @Override
  public @NotNull String getName() {
    return this.name;
  }

  @Override
  public @NotNull Class<T> getType() {
    return this.type;
  }

  @Override
  public Optional<View> getView() {
    return Optional.ofNullable(this.view);
  }

  protected @Nullable View getResolvedView() {
    return getView().orElse(null);
  }

  /**
   * Set the {@link View} containing this {@link Column}.
   *
   * @param view {@link View} containing this {@link Column}.
   * @see org.cp.elements.data.struct.tabular.View
   * @see #getView()
   */
  public void setView(@Nullable View view) {
    this.view = view;
  }

  /**
   * Builder method used to set the {@link String alias}, or {@link String alternate name},
   * when referring to this {@link Column}.
   *
   * @param <S> concrete {@link Class type} of this {@link Column}.
   * @param alias {@link String} containing the {@literal alias}, or {@literal alternate name},
   * used when referring to this {@link Column}.
   * @return this {@link Column}.
   * @see #setAlias(String)
   */
  @SuppressWarnings("unchecked")
  public @NotNull <S extends AbstractColumn<T>> S aliasedWith(@Nullable String alias) {
    setAlias(alias);
    return (S) this;
  }

  /**
   * Builder method used to set the {@link String description} of this {@link Column}.
   *
   * @param <S> concrete {@link Class type} of this {@link Column}.
   * @param description {@link String} describing this {@link Column}.
   * @return this {@link Column}.
   * @see #setDescription(String)
   */
  @SuppressWarnings("unchecked")
  public @NotNull <S extends AbstractColumn<T>> S describedAs(@Nullable String description) {
    setDescription(description);
    return (S) this;
  }

  /**
   * Builder method used to declare the {@link View} containing this {@link Column}.
   *
   * @param <S> concrete {@link Class type} of this {@link Column}.
   * @param view {@link View} to which this {@link Column} belongs.
   * @return this {@link Column}.
   * @see org.cp.elements.data.struct.tabular.View
   * @see #setView(View)
   */
  @SuppressWarnings("unchecked")
  public @NotNull <S extends AbstractColumn<T>> S in(@Nullable View view) {
    setView(view);
    return (S) this;
  }

  /**
   * Builder method used to set the {@link Object default value} used when a {@link Object value}
   * is not provided for this {@link Column}.
   *
   * @param <S> concrete {@link Class type} of this {@link Column}.
   * @param defaultValue {@link Object default value} used when a {@link Object value}
   * is not provided for this {@link Column}.
   * @return this {@link Column}.
   * @see #setDefaultValue(Object)
   */
  @SuppressWarnings("unchecked")
  public @NotNull <S extends AbstractColumn<T>> S valueDefaultingTo(@Nullable T defaultValue) {
    setDefaultValue(defaultValue);
    return (S) this;
  }

  /**
   * Compares this {@link Column} to the given, required {@link Column} by {@link #getName()} to determine order,
   * sorted in ascending order.
   *
   * @param column {@link Column} being compared to this {@link Column}; must not be {@literal null}.
   * @return {@link Integer value} less than {@literal 0} if this {@link Column} comes before the given {@link Column}.
   * Return {@link Integer value} greater than {@literal 0} if this {@link Column} comes after the given {@link Column}.
   * Return {@link Integer 0} if this {@link Column} is comparably equal to the given {@link Column}.
   * @see java.lang.Comparable#compareTo(Object)
   * @see #getName()
   */
  @Override
  public int compareTo(@NotNull Column<T> column) {
    return this.getName().compareTo(column.getName());
  }

  /**
   * Determines whether this {@link Column} is equal to the given {@link Object}.
   *
   * @param obj {@link Object} to test for equality with this {@link Column}.
   * @return {@literal true} if this {@link Column} is equal to the given {@link Object},
   * otherwise return {@literal false}.
   * @see java.lang.Object#equals(Object)
   * @see #getName()
   * @see #getView()
   */
  @Override
  public boolean equals(@Nullable Object obj) {

    if (this == obj) {
      return true;
    }

    if (!(obj instanceof Column)) {
      return false;
    }

    Column<?> that = (Column<?>) obj;

    return this.getName().equals(that.getName())
      && ObjectUtils.equalsIgnoreNull(resolveView(this), resolveView(that));
  }

  /**
   * Computes the {@link Integer hash code} for this {@link Column}.
   *
   * @return the computed {@link Integer hash code} for this {@link Column}.
   * @see java.lang.Object#hashCode()
   * @see #getName()
   * @see #getView()
   */
  @Override
  public int hashCode() {
    return ObjectUtils.hashCodeOf(getName(), resolveView(this));
  }

  @NullSafe
  private @Nullable View resolveView(Column<?> column) {

    return column instanceof AbstractColumn ? ((AbstractColumn<?>) column).getResolvedView()
      : Objects.nonNull(column) ? column.getView().orElse(null)
      : null;
  }

  /**
   * Returns a {@link String} representation of this {@link Column}.
   *
   * @return a {@link String} describing this {@link Column}.
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {

    String alias = getAlias().orElse(null);
    String description = getDescription().orElse(null);
    String viewName = getView().map(View::getName).orElse(null);

    return String.format(COLUMN_TO_STRING, getClass().getName(), getName(), alias, description,
      getType().getName(), getDefaultValue().orElse(null), viewName);
  }
}
