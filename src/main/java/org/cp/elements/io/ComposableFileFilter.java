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
package org.cp.elements.io;

import java.io.File;
import java.io.FileFilter;

import org.cp.elements.lang.Composite;
import org.cp.elements.lang.Filter;
import org.cp.elements.lang.LogicalOperator;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.util.ArrayUtils;

/**
 * Java {@link FileFilter} and Elements {@link Filter} implementation composed of multiple {@link FileFilter FileFilters}
 * joined by {@link LogicalOperator logical operators}: [ {@link LogicalOperator#AND}, {@link LogicalOperator#OR},
 * {@link LogicalOperator#XOR} ].
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.cp.elements.lang.Composite
 * @see org.cp.elements.lang.Filter
 * @see org.cp.elements.lang.LogicalOperator
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ComposableFileFilter implements Composite<FileFilter>, FileFilter, Filter<File> {

  protected static final ComposableFileFilter INSTANCE = new ComposableFileFilter();

  protected static final FileFilter DEFAULT_FILE_FILTER = FileUtils.nullSafeFileFilter(null, false);

  protected static final LogicalOperator DEFAULT_OPERATOR = LogicalOperator.AND;

  /**
   * Factory method used to return a Singleton instance of {@link ComposableFileFilter} that can be used to
   * compose two or more individual {@link FileFilter} objects into a single, {@link Composite}
   * {@link FileFilter} object.
   *
   * @return the Singleton instance of {@link ComposableFileFilter} used to compose {@link FileFilter FileFilters}.
   * @see org.cp.elements.io.ComposableFileFilter
   */
  public static @NotNull ComposableFileFilter builder() {
    return INSTANCE;
  }

  /**
   * Factory method used to construct a new instance of {@link ComposableFileFilter} composed of two {@link FileFilter}
   * operands joined by the given {@link LogicalOperator}.
   *
   * @param leftOperand {@link FileFilter} operand used in the left-hand side (lhs) of the logical expression.
   * @param rightOperand {@link FileFilter} operand used in the right-hand side (rhs) of the logical expression.
   * @param operator {@link LogicalOperator} used to join the {@link FileFilter} operands.
   * @return a {@link ComposableFileFilter} consisting of two {@link FileFilter} operands joined by
   * the {@link LogicalOperator}. Returns the {@link FileFilter left operand} if the {@link FileFilter right operand}
   * is {@literal null}. Returns the {@link FileFilter right operand} if the {@link FileFilter left operand}
   * is {@literal null}. Returns both {@link FileFilter operands} joined by the {@link LogicalOperator}
   * if neither is {@literal null}.
   * @see org.cp.elements.lang.LogicalOperator
   * @see java.io.FileFilter
   */
  @NullSafe
  protected static @Nullable FileFilter compose(@Nullable FileFilter leftOperand, @Nullable FileFilter rightOperand,
      @Nullable LogicalOperator operator) {

    return leftOperand == null ? rightOperand
      : rightOperand == null ? leftOperand
      : new ComposableFileFilter(leftOperand, rightOperand, nullSafeOperator(operator));
  }

  /**
   * Creates a single, {@literal Composite} {@link FileFilter} composed from the array of {@link FileFilter FileFilters}.
   *
   * @param fileFilters array of {@link FileFilter FileFilters} to compose.
   * @param operator {@link LogicalOperator} used to join the array of {@link FileFilter FileFilters} together
   * in a composition, as a single, {@literal Composite} {@link FileFilter}.
   * @return a single, {@literal Composite} {@link FileFilter} composed from the array of {@link FileFilter FileFilters}.
   * @see org.cp.elements.lang.LogicalOperator
   * @see java.io.FileFilter
   */
  @NullSafe
  protected static @NotNull FileFilter compositeOf(@Nullable FileFilter[] fileFilters,
      @Nullable LogicalOperator operator) {

    FileFilter composedFileFilter = null;

    for (FileFilter fileFilter : ArrayUtils.nullSafeArray(fileFilters, FileFilter.class)) {
      composedFileFilter = compose(composedFileFilter, fileFilter, operator);
    }

    return nullSafeFileFilter(composedFileFilter);
  }

  /**
   * Composes two {@link FileFilter} objects in an expression joined by {@link LogicalOperator#AND}.
   *
   * @param fileFilters array of {@link FileFilter FileFilters} to compose in an expression
   * joined by {@link LogicalOperator#AND}.
   * @return a {@link FileFilter} implementation composed of two {@link FileFilter} objects in an expression
   * joined by {@link LogicalOperator#AND}.
   * @see #compositeOf(FileFilter[], LogicalOperator)
   * @see org.cp.elements.lang.LogicalOperator#AND
   * @see java.io.FileFilter
   */
  @NullSafe
  public static @NotNull FileFilter and(FileFilter... fileFilters) {
    return compositeOf(fileFilters, LogicalOperator.AND);
  }

  /**
   * Composes two {@link FileFilter} objects in an expression joined by {@link LogicalOperator#OR}.
   *
   * @param fileFilters array of {@link FileFilter FileFilters} to compose into an expression
   * joined by {@link LogicalOperator#OR}.
   * @return a {@link FileFilter} implementation composed of two {@link FileFilter} objects in an expression
   * joined by {@link LogicalOperator#OR}.
   * @see #compositeOf(FileFilter[], LogicalOperator)
   * @see org.cp.elements.lang.LogicalOperator#OR
   * @see java.io.FileFilter
   */
  @NullSafe
  public static @NotNull FileFilter or(FileFilter... fileFilters) {
    return compositeOf(fileFilters, LogicalOperator.OR);
  }

  /**
   * Composes two {@link FileFilter} objects in an expression joined by {@link LogicalOperator#XOR}.
   *
   * @param leftOperand {@link FileFilter} operand used in the left-hand side (lhs) of the logical expression.
   * @param rightOperand {@link FileFilter} operand used in the right-hand side (lhs) of the logical expression.
   * @return a {@link FileFilter} implementation composed of two {@link FileFilter} objects in an expression
   * joined by {@link LogicalOperator#XOR}.
   * @see #compositeOf(FileFilter[], LogicalOperator)
   * @see org.cp.elements.lang.LogicalOperator#XOR
   * @see java.io.FileFilter
   */
  @NullSafe
  public static @NotNull FileFilter xor(@Nullable FileFilter leftOperand, @Nullable FileFilter rightOperand) {
    return nullSafeFileFilter(compose(leftOperand, rightOperand, LogicalOperator.XOR));
  }

  /**
   * Utility method used to guard against a {@literal null} {@link FileFilter}, returning a default {@link FileFilter}
   * rejecting all {@link File Files} if the given {@link FileFilter} is {@literal null}.
   *
   * @param fileFilter {@link FileFilter} to evaluate.
   * @return the given {@link FileFilter} if not {@literal null}, otherwise returns a default {@link FileFilter}
   * that rejects all {@link File Files}.
   * @see java.io.FileFilter
   */
  @NullSafe
  private static @NotNull FileFilter nullSafeFileFilter(@Nullable FileFilter fileFilter) {
    return fileFilter != null ? fileFilter : DEFAULT_FILE_FILTER;
  }

  /**
   * Utility method used to guard against a {@literal null} {@link LogicalOperator}, returning the default
   * {@link LogicalOperator#AND} operator if the given {@link LogicalOperator } is {@literal null}.
   *
   * @param operator {@link LogicalOperator} to evaluate.
   * @return the given {@link LogicalOperator} if not {@literal null}, otherwise returns {@link LogicalOperator#AND}.
   * @see org.cp.elements.lang.LogicalOperator
   */
  @NullSafe
  private static @NotNull LogicalOperator nullSafeOperator(@Nullable LogicalOperator operator) {
    return operator != null ? operator : DEFAULT_OPERATOR;
  }

  private final FileFilter leftOperand;
  private final FileFilter rightOperand;

  private final LogicalOperator operator;

  /**
   * Default, private constructor used to construct a Singleton instance of {@link ComposableFileFilter} that can
   * compose 2 or more individual {@link FileFilter} objects into a single, {@link Composite} {@link FileFilter}
   * object.
   */
  private ComposableFileFilter() {

    this.leftOperand = file -> false;
    this.rightOperand = file -> false;
    this.operator = LogicalOperator.AND;
  }

  /**
   * Constructs a new instance of {@link ComposableFileFilter} initialized with {@link FileFilter left}
   * and {@link FileFilter right} operands joined by the given, required {@link LogicalOperator}
   * when used in the filtering process.
   *
   * @param leftOperand {@link FileFilter} used in the left-hand side (lhs) of the logical expression;
   * must not be {@literal null}.
   * @param rightOperand {@link FileFilter} used in the right-hand side (rhs) of the logical expression;
   * must not be {@literal null}.
   * @param operator {@link LogicalOperator} used to join the left and right {@link FileFilter} operands.
   * @throws java.lang.IllegalArgumentException if the {@link LogicalOperator} or either {@link FileFilter}
   * operand is {@literal null}.
   * @see org.cp.elements.lang.LogicalOperator
   * @see java.io.FileFilter
   */
  protected ComposableFileFilter(@NotNull FileFilter leftOperand, @NotNull FileFilter rightOperand,
      @NotNull LogicalOperator operator) {

    this.leftOperand = ObjectUtils.requireObject(leftOperand, "Left operand is required");
    this.rightOperand = ObjectUtils.requireObject(rightOperand, "Right operand is required");
    this.operator = ObjectUtils.requireObject(operator, "Operator is required");
  }

  /**
   * Composes two {@link FileFilter} objects into a single, {@link Composite} {@link FileFilter} object
   * joined by {@link LogicalOperator#AND}.
   *
   * @param leftOperand {@link FileFilter} used in the left-hand side (lhs) of the logical expression.
   * @param rightOperand {@link FileFilter} used in the right-hand side (rhs) of the logical expression.
   * @return a single, {@link Composite} {@link FileFilter} object composed of the given {@link FileFilter} objects
   * using {@link LogicalOperator#AND}.
   * @see #compose(FileFilter, FileFilter, LogicalOperator)
   * @see org.cp.elements.lang.LogicalOperator#AND
   * @see java.io.FileFilter
   */
  @NullSafe
  @Override
  public @NotNull FileFilter compose(@Nullable FileFilter leftOperand, @Nullable FileFilter rightOperand) {
    return nullSafeFileFilter(compose(leftOperand, rightOperand, LogicalOperator.AND));
  }

  /**
   * Returns the {@link FileFilter} operand on the left-hand side (lhs) of the logical expression.
   *
   * @return {@link FileFilter} operand used in the left-hand side (lhs) of the logical expression.
   * @see java.io.FileFilter
   */
  protected @NotNull FileFilter getLeftOperand() {
    return this.leftOperand;
  }

  /**
   * Returns the {@link LogicalOperator} used to compose (join) the {@link FileFilter} operands in a logical expression.
   *
   * @return the {@link LogicalOperator} used to compose (join) the {@link FileFilter} operands in a logical expression.
   * @see org.cp.elements.lang.LogicalOperator
   */
  protected @NotNull LogicalOperator getOperator() {
    return this.operator;
  }

  /**
   * Returns the {@link FileFilter} operand on the right-hand side (rhs) of the logical expression.
   *
   * @return {@link FileFilter} operand used in the right-hand side (rhs) of the logical expression.
   * @see java.io.FileFilter
   */
  protected @NotNull FileFilter getRightOperand() {
    return this.rightOperand;
  }

  /**
   * Determines whether the given {@link File} matches the criteria of this {@link FileFilter}.
   *
   * @param file {@link File} to evaluate.
   * @return a boolean value indicating whether the given {@link File} matches the criteria of this {@link FileFilter}.
   * @see java.io.FileFilter#accept(File)
   * @see java.io.File
   */
  @Override
  @SuppressWarnings("unchecked")
  public boolean accept(File file) {
    return getOperator().evaluate(() -> getLeftOperand().accept(file), () -> getRightOperand().accept(file));
  }
}
