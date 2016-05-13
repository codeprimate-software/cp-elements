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

package org.cp.elements.io;

import java.io.File;
import java.io.FileFilter;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Filter;
import org.cp.elements.lang.LogicalOperator;

/**
 * The ComposableFileFilter class is a {@link FileFilter} implementation composed of multiple {@link FileFilter}s
 * joined by logical operators, AND and OR.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.cp.elements.lang.Filter
 * @see org.cp.elements.lang.LogicalOperator
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ComposableFileFilter implements FileFilter, Filter<File> {

  private final FileFilter leftOperand;
  private final FileFilter rightOperand;

  private final LogicalOperator operator;

  /**
   * Constructs an instance of the ComposableFileFilter class initialized with the specified logical operator used
   * to compose both the left and right {@link FileFilter} operands in the filtering operation.
   *
   * @param operator the {@link LogicalOperator} used in the composition of the two {@link FileFilter} operands.
   * @param leftOperand left {@link FileFilter} operand.
   * @param rightOperand right {@link FileFilter} operand.
   * @throws java.lang.NullPointerException if the logical operator or either {@link FileFilter} operands are null.
   * @see java.io.FileFilter
   * @see org.cp.elements.lang.LogicalOperator
   */
  private ComposableFileFilter(LogicalOperator operator, FileFilter leftOperand, FileFilter rightOperand) {
    Assert.notNull(operator, "The logical operator must be specified");
    Assert.notNull(leftOperand, "The left FileFilter operand cannot be null");
    Assert.notNull(rightOperand, "The right FileFilter operand cannot be null");

    this.operator = operator;
    this.leftOperand = leftOperand;
    this.rightOperand = rightOperand;
  }

  /**
   * Constructs a ComposableFileFilter composed of the two {@link FileFilter} operands joined by
   * the given {@link LogicalOperator}.
   *
   * @param operator the {@link LogicalOperator} used in the composition of the two {@link FileFilter} operands.
   * @param leftOperand left {@link FileFilter} operand.
   * @param rightOperand right {@link FileFilter} operand.
   * @return a ComposableFileFilter consisting of the two {@link FileFilter} operands joined by the given logical
   * operator. Returns the left operand if the right is null, the right operand if the left is null, or both operands
   * joined by the logical operator if neither is null.
   * @see java.io.FileFilter
   * @see org.cp.elements.lang.LogicalOperator
   */
  protected static FileFilter compose(LogicalOperator operator, FileFilter leftOperand, FileFilter rightOperand) {
    return (leftOperand == null ? rightOperand : (rightOperand == null ? leftOperand :
      new ComposableFileFilter(operator, leftOperand, rightOperand)));
  }

  /**
   * Composes two {@link FileFilter} operands in an expression joined by the logical AND operator.
   *
   * @param leftOperand left {@link FileFilter} operand.
   * @param rightOperand right {@link FileFilter} operand.
   * @return a {@link FileFilter} implementation composed of two {@link FileFilter} operands in an expression
   * joined by the logical AND operator.
   * @see #compose(org.cp.elements.lang.LogicalOperator, java.io.FileFilter, java.io.FileFilter)
   * @see org.cp.elements.lang.LogicalOperator#AND
   * @see java.io.FileFilter
   */
  public static FileFilter and(FileFilter leftOperand, FileFilter rightOperand) {
    return compose(LogicalOperator.AND, leftOperand, rightOperand);
  }

  /**
   * Composes two {@link FileFilter} operands in an expression joined by the logical OR operator.
   *
   * @param leftOperand left {@link FileFilter} operand.
   * @param rightOperand right {@link FileFilter} operand.
   * @return a {@link FileFilter} implementation composed of two {@link FileFilter} operands in an expression
   * joined by the logical OR operator.
   * @see #compose(org.cp.elements.lang.LogicalOperator, java.io.FileFilter, java.io.FileFilter)
   * @see org.cp.elements.lang.LogicalOperator#OR
   * @see java.io.FileFilter
   */
  public static FileFilter or(FileFilter leftOperand, FileFilter rightOperand) {
    return compose(LogicalOperator.OR, leftOperand, rightOperand);
  }

  /**
   * Composes two {@link FileFilter} operands in an expression joined by the logical XOR operator.
   *
   * @param leftOperand left {@link FileFilter} operand.
   * @param rightOperand right {@link FileFilter} operand.
   * @return a {@link FileFilter} implementation composed of two {@link FileFilter} operands in an expression
   * joined by the logical XOR operator.
   * @see #compose(org.cp.elements.lang.LogicalOperator, java.io.FileFilter, java.io.FileFilter)
   * @see org.cp.elements.lang.LogicalOperator#XOR
   * @see java.io.FileFilter
   */
  public static FileFilter xor(FileFilter leftOperand, FileFilter rightOperand) {
    return compose(LogicalOperator.XOR, leftOperand, rightOperand);
  }

  /**
   * Returns the left operand in the composed {@link FileFilter} expression.
   *
   * @return the left operand in the composed {@link FileFilter} expression.
   * @see java.io.FileFilter
   */
  protected FileFilter getLeftOperand() {
    return this.leftOperand;
  }

  /**
   * Returns the {@link LogicalOperator} used to compose the left and right operands
   * in the {@link FileFilter} expression.
   *
   * @return the {@link LogicalOperator} used to compose the left and right operands
   * in the {@link FileFilter} expression.
   * @see org.cp.elements.lang.LogicalOperator
   */
  protected LogicalOperator getOperator() {
    return this.operator;
  }

  /**
   * Returns the right operand in the composed {@link FileFilter} expression.
   *
   * @return the right operand in the composed {@link FileFilter} expression.
   * @see java.io.FileFilter
   */
  protected FileFilter getRightOperand() {
    return this.rightOperand;
  }

  /**
   * Determines whether the given {@link File} matches the criteria of this {@link FileFilter}.
   *
   * @param file the {@link File} to evaluate.
   * @return a boolean value indicating whether the given {@link File} matches the criteria of this {@link FileFilter}.
   * @see java.io.FileFilter#accept(java.io.File)
   * @see java.io.File
   */
  @Override
  @SuppressWarnings("unchecked")
  public boolean accept(File file) {
    return getOperator().evaluate(() -> getLeftOperand().accept(file), () -> getRightOperand().accept(file));
  }

}
