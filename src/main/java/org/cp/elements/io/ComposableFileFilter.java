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
 * The ComposableFileFilter class is a FileFilter implementation...
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
   * to compose both the left and right FileFilter operands in the filtering operation.
   *
   * @param operator the LogicalOperator used to compose (combine) the FileFilter operands in a joint logical operation.
   * @param leftOperand the left FileFilter operand.
   * @param rightOperand the right FileFilter operand.
   * @throws java.lang.NullPointerException if the logical operator or either FileFilter operand references are null.
   * @see java.io.FileFilter
   * @see org.cp.elements.lang.LogicalOperator
   */
  private ComposableFileFilter(final LogicalOperator operator,
                               final FileFilter leftOperand,
                               final FileFilter rightOperand)
  {
    Assert.notNull(operator, "The logical operator must be specified!");
    Assert.notNull(leftOperand, "The left FileFilter operand must not be null!");
    Assert.notNull(rightOperand, "The right FileFilter operand must not be null!");
    this.operator = operator;
    this.leftOperand = leftOperand;
    this.rightOperand = rightOperand;
  }

  /**
   * Composes the two FileFilter operands joined by the LogicalOperator.
   *
   * @param operator the LogicalOperator used to compose (combine) the FileFilter operands in a joint logical operation.
   * @param leftOperand the left FileFilter operand.
   * @param rightOperand the right FileFilter operand.
   * @return a FileFilter implementation composed of the two FileFilter operands joined by the logical operator.
   * Returns the left operand if the right is null, the right operand if the left is null, and the composed operands
   * using the logical operator if neither is null.
   * @see java.io.FileFilter
   * @see org.cp.elements.lang.LogicalOperator
   */
  protected static FileFilter compose(final LogicalOperator operator,
                                      final FileFilter leftOperand,
                                      final FileFilter rightOperand)
  {
    return (leftOperand == null ? rightOperand : (rightOperand == null ? leftOperand :
      new ComposableFileFilter(operator, leftOperand, rightOperand)));
  }

  /**
   * Composes two FileFilter operands joined by the logical AND operator.
   *
   * @param leftOperand the left FileFilter operand.
   * @param rightOperand the right FileFilter operand.
   * @return a FileFilter implementation composed of two FileFilter operands joined by the logical AND operator.
   * @see #compose(org.cp.elements.lang.LogicalOperator, java.io.FileFilter, java.io.FileFilter)
   * @see java.io.FileFilter
   * @see org.cp.elements.lang.LogicalOperator#AND
   */
  public static FileFilter and(final FileFilter leftOperand, final FileFilter rightOperand) {
    return compose(LogicalOperator.AND, leftOperand, rightOperand);
  }

  /**
   * Composes two FileFilter operands joined by the logical OR operator.
   *
   * @param leftOperand the left FileFilter operand.
   * @param rightOperand the right FileFilter operand.
   * @return a FileFilter implementation composed of two FileFilter operands joined by the logical OR operator.
   * @see #compose(org.cp.elements.lang.LogicalOperator, java.io.FileFilter, java.io.FileFilter)
   * @see java.io.FileFilter
   * @see org.cp.elements.lang.LogicalOperator#OR
   */
  public static FileFilter or(final FileFilter leftOperand, final FileFilter rightOperand) {
    return compose(LogicalOperator.OR, leftOperand, rightOperand);
  }

  /**
   * Composes two FileFilter operands joined by the logical XOR operator.
   *
   * @param leftOperand the left FileFilter operand.
   * @param rightOperand the right FileFilter operand.
   * @return a FileFilter implementation composed of two FileFilter operands joined by the logical XOR operator.
   * @see #compose(org.cp.elements.lang.LogicalOperator, java.io.FileFilter, java.io.FileFilter)
   * @see java.io.FileFilter
   * @see org.cp.elements.lang.LogicalOperator#XOR
   */
  public static FileFilter xor(final FileFilter leftOperand, final FileFilter rightOperand) {
    return compose(LogicalOperator.XOR, leftOperand, rightOperand);
  }

  /**
   * Gets the left operand in the composed FileFilter expression.
   *
   * @return the left operand in the composed FileFilter expression.
   * @see #getOperator()
   * @see #getRightOperand()
   * @see java.io.FileFilter
   */
  protected FileFilter getLeftOperand() {
    return leftOperand;
  }

  /**
   * Gets the logical operator used to combine the left and right operands in the FileFilter expression.
   *
   * @return the LogicalOperator used to combine the left and right operands in the FileFilter expression.
   * @see #getLeftOperand()
   * @see #getRightOperand()
   * @see org.cp.elements.lang.LogicalOperator
   */
  protected LogicalOperator getOperator() {
    return operator;
  }

  /**
   * Gets the right operand in the composed FileFilter expression.
   *
   * @return the right operand in the composed FileFilter expression.
   * @see #getLeftOperand()
   * @see #getOperator()
   * @see java.io.FileFilter
   */
  protected FileFilter getRightOperand() {
    return rightOperand;
  }

  /**
   * Determines whether the given File matches the criteria of this FileFilter.
   *
   * @param file the File to evaluate and filter by this FileFilter.
   * @return a boolean value indicating whether the given File matches the criteria of this FileFilter.
   * @see java.io.File
   * @see java.io.FileFilter#accept(java.io.File)
   */
  @Override
  public boolean accept(final File file) {
    return getOperator().evaluate(getLeftOperand().accept(file), getRightOperand().accept(file));
  }

}
