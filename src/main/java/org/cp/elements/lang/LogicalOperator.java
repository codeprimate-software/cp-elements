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
package org.cp.elements.lang;

import java.util.function.Supplier;

/**
 * Abstract base class representations for the standard logical operators AND ({@literal &&}) and OR ({@literal ||}).
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.RelationalOperator
 * @since 1.0.0
 */
@SuppressWarnings("all")
public abstract class LogicalOperator {

  public static final LogicalOperator AND = new LogicalAnd();
  public static final LogicalOperator OR = new LogicalOr();
  public static final LogicalOperator XOR = new LogicalXor();

  /**
   * This method will negate outcome of the given logical operator when performing an evaluation on a group
   * of conditions.
   *
   * @param op the LogicalOperator (such as AND or OR) to negate.
   * @return the given logical operator decorated with logical not.
   * @see LogicalNot
   */
  public static LogicalOperator negate(LogicalOperator op) {
    return new LogicalNot(op.getOpposite());
  }

  /**
   * Indicates whether this logical operator is a binary operator.  A binary operator is an operator with two operands.
   * With LogicalOperator implementations, such as AND and OR, this is expanded to mean two or more operands.
   *
   * @return a boolean value indicating if this logical operator is a binary operator.
   * @see #isTernary()
   * @see #isUnary()
   */
  public abstract boolean isBinary();

  /**
   * Indicates whether this logical operator is the ternary operator, which in Java is represented as
   * (condition ? trueValue : falseValue), or as a if-then-else statement (ifTrue ? thenValue : elseValue).
   *
   * @return a boolean value indicating if this logical operator is the ternary operator.
   * @see #isBinary()
   * @see #isUnary()
   */
  public abstract boolean isTernary();

  /**
   * Indicates whether this logical operator is a unary operator.  A unary operator is an operator with only
   * one operand, such as the NOT operator.
   *
   * @return a boolean value indicating if this logical operator is an unary operator.
   * @see #isBinary()
   * @see #isTernary()
   */
  public abstract boolean isUnary();

  /**
   * Returns a description of this logical operator, such as 'and' or 'or'.
   *
   * @return a String value describing this logical operator.
   * @see #getSymbol()
   */
  public abstract String getDescription();

  /**
   * Gets the logical opposite of this logical operator.  For instance, the logical opposite of AND is OR;
   * the logical opposite of OR is AND.  The logical opposite of NOT is NOT NOT (double negative) resulting in positive.
   * For instance, NOT(NOT(TRUE)) is TRUE.
   *
   * @return the logical opposite of this logical operator.
   */
  public abstract LogicalOperator getOpposite();

  /**
   * Gets the symbolic representation of this logical operator.  For instance, the symbolic representation of AND
   * in Java is {@literal &&}, and the symbolic representation of OR in Java is {@literal ||}.
   *
   * @return a String value representing the symbolic representation of this logical operator.
   * @see #getDescription()
   */
  public abstract String getSymbol();

  /**
   * Evaluates the given array of boolean conditions by applying this logical operator to all conditions collectively.
   *
   * @param conditions a boolean array of conditions to which this logical operator is applied.
   * @return a boolean value resulting from the evaluation of the given boolean conditions with this logical operator.
   */
  public abstract boolean evaluate(boolean... conditions);

  /**
   * Evaluates the given array of boolean based {@link Supplier}s by applying this logical operation to all conditions
   * supplied collectively.
   *
   * @param suppliers an array of {@link Supplier}s supplying the boolean conditions to evaluate.
   * @return a boolean value resulting from the evaluation of the given {@link Supplier}s of boolean values
   * with this logical operater.
   * @see java.util.function.Supplier
   */
  @SuppressWarnings({ "unchecked", "varargs" })
  public abstract boolean evaluate(Supplier<Boolean>... suppliers);

  /**
   * Gets a String representation of the logical operator.
   *
   * @return a String value representing the logical operator.
   * @see org.cp.elements.lang.LogicalOperator#getDescription
   */
  @Override
  public String toString() {
    return getDescription();
  }

  /**
   * The abstract base class for all subclasses of the LogicalOperator class.
   */
  protected abstract static class AbstractLogicalOperator extends LogicalOperator {

    private final String description;
    private final String symbol;

    /**
     * Constructs a instance of the LogicalOperator class with the given description and symbol
     * for the logical operator.
     *
     * @param description a String value describing the logical operator.
     * @param symbol a String value symbolically represending the logical operator.
     * @throws IllegalArgumentException if either the description or symbol are not specified.
     */
    protected AbstractLogicalOperator(String description, String symbol) {
      Assert.hasText(description, "The description of this logical operator must be specified");
      Assert.hasText(symbol, "The symbolic representation of this logical operator must be specified");
      this.description = description;
      this.symbol = symbol;
    }

    /**
     * Indicates whether this logical operator is a binary operation.
     *
     * A binary operator is an operator with two operands. With LogicalOperator implementations, such as AND and OR,
     * this is expanded to mean two or more operands.
     *
     * @return a boolean value indicating if this logical operator is a binary operator.
     * @see #isTernary()
     * @see #isUnary()
     */
    @Override
    public boolean isBinary() {
      return false;
    }

    /**
     * Indicates whether this logical operator is the ternary operator, which in Java is represented as
     * (condition ? trueValue : falseValue), or as a if-then-else statement (ifTrue ? thenValue : elseValue).
     *
     * @return a boolean value indicating if this logical operator is the ternary operator.
     * @see #isBinary()
     * @see #isUnary()
     */
    @Override
    public boolean isTernary() {
      return false;
    }

    /**
     * Indicates whether this logical operator is a unary operator.  A unary operator is an operator with only
     * one operand, such as the NOT operator.
     *
     * @return a boolean value indicating if this logical operator is an unary operator.
     * @see #isBinary()
     * @see #isTernary()
     */
    @Override
    public boolean isUnary() {
      return false;
    }

    /**
     * Gets a description of this logical operator, such as 'and' or 'or'.
     *
     * @return a String value describing this logical operator.
     * @see #getSymbol()
     */
    @Override
    public String getDescription() {
      return this.description;
    }

    /**
     * Gets the logical opposite of this logical operator.  For instance, the logical opposite of AND is OR;
     * the logical opposite of OR is AND.  The logical opposite of NOT is NOT NOT (double negative) resulting
     * in positive.
     * For instance, NOT(NOT(TRUE)) is TRUE.
     *
     * @return the logical opposite of this logical operator.
     */
    @Override
    public LogicalOperator getOpposite() {
      throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
    }

    /**
     * Gets the symbolic representation of this logical operator.  For instance, the symbolic representation of AND
     * in Java is {@literal &&}, and the symbolic representation of OR in Java is {@literal ||}.
     *
     * @return a String value representing the symbolic representation of this logical operator.
     * @see #getDescription()
     */
    @Override
    public String getSymbol() {
      return this.symbol;
    }
  }

  /**
   * LogicalAnd is a {@link LogicalOperator} implementation of the binary, logical AND operator.
   */
  static final class LogicalAnd extends AbstractLogicalOperator {

    /**
     * Constructs an instance of the LogicalAnd operator.
     */
    LogicalAnd() {
      super("and", "&&");
    }

    /* (non-Javadoc) */
    @Override
    public boolean isBinary() {
      return true;
    }

    /* (non-Javadoc) */
    @Override
    public LogicalOperator getOpposite() {
      return LogicalOperator.OR;
    }

    /* (non-Javadoc) */
    @Override
    public boolean evaluate(boolean... conditions) {
      for (boolean condition : conditions) {
        if (!condition) {
          return false;
        }
      }

      return true;
    }

    /* (non-Javadoc) */
    @Override
    @SafeVarargs
    public final boolean evaluate(Supplier<Boolean>... suppliers) {
      for (Supplier<Boolean> supplier : suppliers) {
        if (!Boolean.TRUE.equals(supplier.get())) {
          return false;
        }
      }

      return true;
    }
  }

  /**
   * LogicalNot is a {@link LogicalOperator} implementation of the unary, logical NOT operator.
   */
  static final class LogicalNot extends AbstractLogicalOperator {

    private final LogicalOperator op;

    /**
     * Constructs an instance of the LogicalNot operator.
     *
     * @param op the LogicalOperator to negate.
     */
    LogicalNot(LogicalOperator op) {
      super("not", "!");
      Assert.notNull(op, "The LogicalOperator to negate cannot be null");
      this.op = op;
    }

    /* (non-Javadoc) */
    @Override
    public boolean isUnary() {
      return true;
    }

    /* (non-Javadoc) */
    @SuppressWarnings("unused")
    LogicalOperator getOp() {
      return this.op;
    }

    /* (non-Javadoc) */
    @Override
    public LogicalOperator getOpposite() {
      return this;
    }

    /* (non-Javadoc) */
    @Override
    public boolean evaluate(boolean... conditions) {
      return !this.op.evaluate(conditions);
    }

    /* (non-Javadoc) */
    @Override
    @SafeVarargs
    public final boolean evaluate(Supplier<Boolean>... suppliers) {
      return !this.op.evaluate(suppliers);
    }
  }

  /**
   * LogicalOr is a {@link LogicalOperator} implementation of the binary, logical OR operator.
   */
  static final class LogicalOr extends AbstractLogicalOperator {

    /**
     * Constructs an instance of the LogicalOr operator.
     */
    LogicalOr() {
      super("or", "||");
    }

    /* (non-Javadoc) */
    @Override
    public boolean isBinary() {
      return true;
    }

    /* (non-Javadoc) */
    @Override
    public LogicalOperator getOpposite() {
      return LogicalOperator.AND;
    }

    /* (non-Javadoc) */
    @Override
    public boolean evaluate(boolean... conditions) {
      for (boolean condition : conditions) {
        if (condition) {
          return true;
        }
      }

      return false;
    }

    /* (non-Javadoc) */
    @Override
    @SafeVarargs
    public final boolean evaluate(Supplier<Boolean>... suppliers) {
      for (Supplier<Boolean> supplier : suppliers) {
        if (Boolean.TRUE.equals(supplier.get())) {
          return true;
        }
      }

      return false;
    }
  }

  /**
   * LogicalXor is a {@link LogicalOperator} implementation of the binary, logical XOR operator.
   */
  static final class LogicalXor extends AbstractLogicalOperator {

    /**
     * Constructs an instance of the LogicalXor operator.
     */
    LogicalXor() {
      super("xor", "^");
    }

    /* (non-Javadoc) */
    @Override
    public boolean isBinary() {
      return true;
    }

    /* (non-Javadoc) */
    @Override
    public LogicalOperator getOpposite() {
      return this;
    }

    @Override
    public boolean evaluate(boolean... conditions) {
      int count = 0;

      for (int index = 0; index < conditions.length && count < 2; index++) {
        if (conditions[index]) {
          count++;
        }
      }

      return (count == 1);
    }

    /* (non-Javadoc) */
    @Override
    @SafeVarargs
    public final boolean evaluate(Supplier<Boolean>... suppliers) {
      int count = 0;

      for (int index = 0; index < suppliers.length && count < 2; index++) {
        if (Boolean.TRUE.equals(suppliers[index].get())) {
          count++;
        }
      }

      return (count == 1);
    }
  }
}
