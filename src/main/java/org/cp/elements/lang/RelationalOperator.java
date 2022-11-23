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

/**
 * Abstract base class defining {@link Class type} representations for the relation operators:
 * [{@literal ==}, {@literal >}, {@literal >=}, {@literal <}, {@literal <=} ].
 *
 * @author John J. Blum
 * @param <T> {@link Comparable} {@link Class type} for the {@link Object values} evaluated by this operator.
 * @see java.lang.Comparable
 * @see org.cp.elements.lang.LogicalOperator
 * @since 1.0.0
 */
public abstract class RelationalOperator<T extends Comparable<T>> {

  /**
   * Gets the RelationalOperator performing equality comparisons to determine whether all provided values are equal to
   * the given expected value.
   *
   * @param value the expected object value of the equality comparison.
   * @param <T> the expected Class type for the object used in the equality comparison.
   * @return a RelationalOperator for the equality comparison.
   */
  public static <T extends Comparable<T>> RelationalOperator<T> equalTo(T value) {
    return new EqualToOperator<>(value);
  }

  /**
   * Gets the RelationalOperator performing greater than comparisons to determine whether all provided values
   * are greater than the given lower bound value.
   *
   * @param lowerBound the Comparable lower bounded value.
   * @param <T> the expected Class type for the object used in the greater than comparison.
   * @return a RelationalOperator for the greater than comparison.
   */
  public static <T extends Comparable<T>> RelationalOperator<T> greaterThan(T lowerBound) {
    return new GreaterThanOperator<>(lowerBound);
  }

  /**
   * Gets the RelationalOperator performing greater than and less than comparisons to determine whether all provided
   * values are greater than some lower bound value and also less than some upper bound value.
   *
   * @param lowerBound the Comparable lower bounded value.
   * @param upperBound the Comparable upper bounded value.
   * @param <T> the expected Class type for the object used in the greater than and less than comparison.
   * @return a RelationalOperator for the greater than and less than comparison.
   */
  public static <T extends Comparable<T>> RelationalOperator<T> greaterThanAndLessThan(T lowerBound, T upperBound) {
    return ComposableRelationalOperator.compose(greaterThan(lowerBound), LogicalOperator.AND, lessThan(upperBound));
  }

  /**
   * Gets the RelationalOperator performing greater than and less than equal to comparisons to determine whether all
   * provided values are greater than some lower bound value and also less than equal to some upper bound value.
   *
   * @param lowerBound the Comparable lower bounded value.
   * @param upperBound the Comparable upper bounded value.
   * @param <T> the expected Class type for the object used in the greater than and less than equal to comparison.
   * @return a RelationalOperator for the greater than and less than equal to comparison.
   */
  public static <T extends Comparable<T>> RelationalOperator<T> greaterThanAndLessThanEqualTo(
      T lowerBound, T upperBound) {

    return ComposableRelationalOperator
      .compose(greaterThan(lowerBound), LogicalOperator.AND, lessThanEqualTo(upperBound));
  }

  /**
   * Gets the RelationalOperator performing greater than equal to comparisons to determine whether all provided values
   * are greater than equal to the given lower bound value.
   *
   * @param lowerBound the Comparable lower bounded value.
   * @param <T> the expected Class type for the object used in the greater than equal to comparison.
   * @return a RelationalOperator for the greater than equal to comparison.
   */
  public static <T extends Comparable<T>> RelationalOperator<T> greaterThanEqualTo(T lowerBound) {
    return new GreaterThanEqualToOperator<>(lowerBound);
  }

  /**
   * Gets the RelationalOperator performing greater than equal to and less than comparisons to determine whether all
   * provided values are greater than equal to some lower bound value and also less than some upper bound value.
   *
   * @param lowerBound the Comparable lower bounded value.
   * @param upperBound the Comparable upper bounded value.
   * @param <T> the expected Class type for the object used in the greater than equal to and less than comparison.
   * @return a RelationalOperator for the greater than equal to and less than comparison.
   */
  public static <T extends Comparable<T>> RelationalOperator<T> greaterThanEqualToAndLessThan(
      T lowerBound, T upperBound) {

    return ComposableRelationalOperator
      .compose(greaterThanEqualTo(lowerBound), LogicalOperator.AND, lessThan(upperBound));
  }

  /**
   * Gets the RelationalOperator performing greater than equal to and less than equal to comparisons to determine
   * whether all provided values are greater than equal to some lower bound value and also less than equal to some
   * upper bound value.
   *
   * @param lowerBound the Comparable lower bounded value.
   * @param upperBound the Comparable upper bounded value.
   * @param <T> the expected Class type for the object used in the greater than equal to and less than equal to
   * comparison.
   * @return a RelationalOperator for the greater than equal to and less than equal to comparison.
   */
  public static <T extends Comparable<T>> RelationalOperator<T> greaterThanEqualToAndLessThanEqualTo(
      T lowerBound, T upperBound) {

    return ComposableRelationalOperator
      .compose(greaterThanEqualTo(lowerBound), LogicalOperator.AND, lessThanEqualTo(upperBound));
  }

  /**
   * Gets the RelationalOperator performing less than comparisons to determine whether all provided values are less than
   * the given upper bound value.
   *
   * @param lowerBound the Comparable upper bounded value.
   * @param <T> the expected Class type for the object used in the less than comparison.
   * @return a RelationalOperator for the less than comparison.
   */
  public static <T extends Comparable<T>> RelationalOperator<T> lessThan(T lowerBound) {
    return new LessThanOperator<>(lowerBound);
  }

  /**
   * Gets the RelationalOperator performing less than or greater than comparisons to determine whether all provided
   * values are less than some upper bound value or possibly greater than some lower bound value.
   *
   * @param lowerBound the Comparable upper bounded value.
   * @param upperBound the Comparable lower bounded value.
   * @param <T> the expected Class type for the object used in the less than or greater than comparison.
   * @return a RelationalOperator for the less than or greater than comparison.
   */
  public static <T extends Comparable<T>> RelationalOperator<T> lessThanOrGreaterThan(T lowerBound, T upperBound) {
    return ComposableRelationalOperator.compose(lessThan(lowerBound), LogicalOperator.OR, greaterThan(upperBound));
  }

  /**
   * Gets the RelationalOperator performing less than or greater than equal to comparisons to determine whether all
   * provided values are less than some upper bound value or possibly greater than equal to some lower bound value.
   *
   * @param lowerBound the Comparable lower upper bounded value.
   * @param upperBound the Comparable upper lower bounded value.
   * @param <T> the expected Class type for the object used in the less than or greater than equal to comparison.
   * @return a RelationalOperator for the less than or greater than equal to comparison.
   */
  public static <T extends Comparable<T>> RelationalOperator<T> lessThanOrGreaterThanEqualTo(
      T lowerBound, T upperBound) {

    return ComposableRelationalOperator
      .compose(lessThan(lowerBound), LogicalOperator.OR, greaterThanEqualTo(upperBound));
  }

  /**
   * Gets the RelationalOperator performing less than equal to comparisons to determine whether all provided values
   * are less than equal to the given upper bound value.
   *
   * @param lowerBound the Comparable upper bounded value.
   * @param <T> the expected Class type for the object used in the less than equal to comparison.
   * @return a RelationalOperator for the less than equal to comparison.
   */
  public static <T extends Comparable<T>> RelationalOperator<T> lessThanEqualTo(T lowerBound) {
    return new LessThanEqualToOperator<>(lowerBound);
  }

  /**
   * Gets the RelationalOperator performing less than equal to or greater than comparisons to determine whether all
   * provided values are less than equal to some upper bound value or possibly greater than some lower bound value.
   *
   * @param lowerBound the Comparable upper bounded value.
   * @param upperBound the Comparable lower bounded value.
   * @param <T> the expected Class type for the object used in the less than equal to or greater than comparison.
   * @return a RelationalOperator for the less than equal to or greater than comparison.
   */
  public static <T extends Comparable<T>> RelationalOperator<T> lessThanEqualToOrGreaterThan(
      T lowerBound, T upperBound) {

    return ComposableRelationalOperator
      .compose(lessThanEqualTo(lowerBound), LogicalOperator.OR, greaterThan(upperBound));
  }

  /**
   * Gets the RelationalOperator performing less than equal to or greater than equal to comparisons to determine whether
   * all provided values are less than equal to some upper bound value or possibly greater than equal to some lower
   * bound value.
   *
   * @param lowerBound the Comparable upper bounded value.
   * @param upperBound the Comparable lower bounded value.
   * @param <T> the expected Class type for the object used in the less than equal to or greater than equal to
   * comparison.
   * @return a RelationalOperator for the less than equal to or greater than equal to comparison.
   */
  public static <T extends Comparable<T>> RelationalOperator<T> lessThanEqualToOrGreaterThanEqualTo(
      T lowerBound, T upperBound) {

    return ComposableRelationalOperator
      .compose(lessThanEqualTo(lowerBound), LogicalOperator.OR, greaterThanEqualTo(upperBound));
  }

  /**
   * Gets a String describing the relational operator, such as 'equal to'.
   *
   * @return a String value describing the relational operator.
   */
  public abstract String getDescription();

  /**
   * Gets the symbolic representation of the relational operator, such as {@literal ==}, {@literal >}, {@literal >=},
   * {@literal <}, {@literal <=} and so on.
   *
   * @return a String value symbolizing the relational operator.
   */
  public abstract String getSymbol();

  /**
   * Performs the relational comparison between the actual and expected value(s) provided when the relational operator
   * was instantiated.
   *
   * @param actualValue the actual Comparable value used in the relational operation to perform the evaluation.
   * @return a boolean value indicating whether the actual Comparable value satisfies the constraints of the
   * relational comparison, with respect to it's expected values.
   */
  public abstract boolean evaluate(T actualValue);

  /**
   * Gets a String describing this relational operator.
   *
   * @return a String value to describe this relational operator.
   * @see org.cp.elements.lang.RelationalOperator#getDescription()
   */
  @Override
  public String toString() {
    return getDescription();
  }

  /**
   * Common abstract base class for all ReltaionalOperator implementation classes.
   *
   * @param <T> the Class type of Comparable object expected in the relational comparison.
   * @see org.cp.elements.lang.RelationalOperator.EqualToOperator
   * @see org.cp.elements.lang.RelationalOperator.GreaterThanOperator
   * @see org.cp.elements.lang.RelationalOperator.GreaterThanEqualToOperator
   * @see org.cp.elements.lang.RelationalOperator.LessThanOperator
   * @see org.cp.elements.lang.RelationalOperator.LessThanEqualToOperator
   * @see org.cp.elements.lang.RelationalOperator.ComposableRelationalOperator
   */
  protected abstract static class AbstractRelationalOperator<T extends Comparable<T>> extends RelationalOperator<T> {

    private final String description;
    private final String symbol;

    private final T expectedValue;

    /**
     * Private constructor used by the ComposableRelationalOperator.  Constructs an instance of the RelationalOperator
     * with a description and symbol for the relational operator.  In this case, the relational operator does not have
     * an expected value since the relational operator is composed of relational operators that do.
     *
     * @param description a String describing the relational operator.
     * @param symbol a String to symbolically represent the relational operator.
     * @throws IllegalArgumentException if either the description or symbol are not specified.
     */
    private AbstractRelationalOperator(String description, String symbol) {
      Assert.hasText(description, "The description of the relational operator must be specified!");
      Assert.hasText(symbol, "The symbol of the relation operator must be specified!");
      this.description = description;
      this.symbol = symbol;
      this.expectedValue = null;
    }

    /**
     * Constructor to create new instances of the RelationalOperator class given a new and different kind of
     * relational operator.
     *
     * @param description a String value describing the relational operator.
     * @param symbol a String value to symbolically represent the relational operator.
     * @param expectedValue the Comparable value expected during the evaluation of the relational operation.
     * @throws IllegalArgumentException if either the description or symbol are not specified.
     * @throws NullPointerException if the expected Comparable value is not specified.
     */
    protected AbstractRelationalOperator(String description, String symbol, T expectedValue) {
      Assert.hasText(description, "The description of the relational operator must be specified!");
      Assert.hasText(symbol, "The symbol of the relation operator must be specified!");
      Assert.notNull(expectedValue, "The expected value in the {0} comparison cannot be null!", description);
      this.description = description;
      this.symbol = symbol;
      this.expectedValue = expectedValue;
    }

    /**
     * Gets a String describing the relational operator, such as 'equal to'.
     *
     * @return a String describing the relational operator.
     */
    @Override
    public String getDescription() {
      return this.description;
    }

    /**
     * Gets the expected Comparable value used in the relational comparison to constrain actual values.
     *
     * @return the expected Comparable value used in the relational comparison to constrain the actual values.
     */
    protected final T getExpectedValue() {
      return this.expectedValue;
    }

    /**
     * Gets the symbolic representation of the relational operator, such as:
     * [ {@literal ==}, {@literal >}, {@literal >=}, {@literal <}, {@literal <=} ] and so on.
     *
     * @return a String symbolizing the relational operator.
     */
    @Override
    public String getSymbol() {
      return this.symbol;
    }

    /**
     * Verifies the validity of the actual Comparable value with the expected Comparable value.
     *
     * @param actualValue the actual Comparable value in the relational comparison.
     * @throws NullPointerException if the actual Comparable value is null.
     */
    protected void validate(T actualValue) {
      Assert.notNull(actualValue, "The actual value in the {0} comparison cannot be null!", this.description);
    }
  }

  /**
   * The ComposableRelationalOperator class allows various relational operators to be combined into a compound
   * relational operation using a logical operator (such as AND or OR).
   *
   * @param <T> the Class type of the Comparable operands used in the relational comparison.
   * @see org.cp.elements.lang.LogicalOperator
   */
  protected static final class ComposableRelationalOperator<T extends Comparable<T>>
      extends AbstractRelationalOperator<T> {

    /**
     * Factory method used in composing a compound relational operator comparison, such as
     * {@literal >} {@literal &&} {@literal <} or {@literal <} {@literal ||} {@literal >}.
     *
     * @param <T> the Class type of the Comparable operands used in the relational comparison.
     * @param leftOperand a RelationalOperator operand on the left-side of the logical operation.
     * @param operator the LogicalOperator used to the combine the two ReltaionalOperator operands in a compound
     * relational comparison.
     * @param rightOperand a RelationalOperator operand on the right-side of the logical operation.
     * @return the left RelationalOperator operand if the right operand is null, or the right RelationalOperator
     * operand if the left is null, or a new ComposableRelationalOperator combining the two individual
     * RelationalOperator operands into a compound relational comparison using the specified LogicalOperator.
     */
    public static <T extends Comparable<T>> RelationalOperator<T> compose(
        RelationalOperator<T> leftOperand, LogicalOperator operator, RelationalOperator<T> rightOperand) {

      return leftOperand == null ? rightOperand
        : rightOperand == null ? leftOperand
        : new ComposableRelationalOperator<>(leftOperand, operator, rightOperand);
    }

    private final LogicalOperator operator;

    private final RelationalOperator<T> leftOperand;
    private final RelationalOperator<T> rightOperand;

    /**
     * Constructs an instance of the ComposableRelationalOperator class with the specified relational operator operands
     * and combining logical operator.
     *
     * @param leftOperand a RelationalOperator operand on the left-side of the logical operation.
     * @param operator the LogicalOperator used to the combine the two ReltaionalOperator operands in a compound
     * relational comparison.
     * @param rightOperand a RelationalOperator operand on the right-side of the logical operation.
     */
    private ComposableRelationalOperator(RelationalOperator<T> leftOperand, LogicalOperator operator,
        RelationalOperator<T> rightOperand) {

      super(StringUtils.singleSpaceObjects(leftOperand, operator, rightOperand),
        StringUtils.singleSpaceObjects(leftOperand.getSymbol(), operator.getSymbol(), rightOperand.getSymbol()));

      this.leftOperand = leftOperand;
      this.operator = operator;
      this.rightOperand = rightOperand;
    }

    /**
     * Gets the left-side operand in the logical operation.
     *
     * @return the RelationalOperator constituting the operand on the left-side of the logical operation.
     */
    RelationalOperator<T> getLeftOperand() {
      return this.leftOperand;
    }

    /**
     * Gets the logical operator used to combine the two independent RelationaOperator operands into a compound
     * relational comparison.
     *
     * @return the LogicalOperator used to combine the two RelationalOperators into a compound relational comparison.
     * @see org.cp.elements.lang.LogicalOperator
     */
    LogicalOperator getOperator() {
      return this.operator;
    }

    /**
     * Gets the right-side operand in the logical operation.
     *
     * @return the RelationalOperator constituting the operand on the right-side of the logical operation.
     */
    RelationalOperator<T> getRightOperand() {
      return this.rightOperand;
    }

    /**
     * Performs the relational comparison between the provided actual value and the expected value(s), as determined
     * when the relational operator was instantiated.
     *
     * @param actualValue the actual Comparable value used in the relational operation to perform the evaluation.
     * @return a boolean value indicating whether the actual Comparable value satisfies the constraints of the
     * relational comparison, with respect to it's expected values.
     */
    @Override
    public boolean evaluate(T actualValue) {
      return getOperator().evaluate(getLeftOperand().evaluate(actualValue), getRightOperand().evaluate(actualValue));
    }
  }

  /**
   * The EqualToOperator class implements equality relational comparisons.
   *
   * @param <T> the Class type of the Comparable operands used in the relational comparison.
   */
  private static final class EqualToOperator<T extends Comparable<T>> extends AbstractRelationalOperator<T> {

    public EqualToOperator(T expectedValue) {
      super("equal to", "==", expectedValue);
    }

    @Override
    public boolean evaluate(T actualValue) {
      validate(actualValue);
      return (actualValue.equals(getExpectedValue()));
      //return (actualValue.compareTo(getExpectedValue()) == 0);
    }
  }

  /**
   * The GreaterThanOperator class implements the greater than relational comparison.
   *
   * @param <T> the Class type of the Comparable operands used in the relational comparison.
   */
  private static final class GreaterThanOperator<T extends Comparable<T>> extends AbstractRelationalOperator<T> {

    public GreaterThanOperator(T expectedValue) {
      super("greater than", ">", expectedValue);
    }

    @Override
    public boolean evaluate(T actualValue) {
      validate(actualValue);
      return (actualValue.compareTo(getExpectedValue()) > 0);
    }
  }

  /**
   * The GreaterThanEqualToOperator class implements the greater than equal to relational comparison.
   *
   * @param <T> the Class type of the Comparable operands used in the relational comparison.
   */
  private static final class GreaterThanEqualToOperator<T extends Comparable<T>> extends AbstractRelationalOperator<T> {

    public GreaterThanEqualToOperator(T expectedValue) {
      super("greater than equal to", ">=", expectedValue);
    }

    @Override
    public boolean evaluate(T actualValue) {
      validate(actualValue);
      return (actualValue.compareTo(getExpectedValue()) >= 0);
    }
  }

  /**
   * The LessThanOperator class implements the less than relational comparison.
   *
   * @param <T> the Class type of the Comparable operands used in the relational comparison.
   */
  private static final class LessThanOperator<T extends Comparable<T>> extends AbstractRelationalOperator<T> {

    public LessThanOperator(T expectedValue) {
      super("less than", "<", expectedValue);
    }

    @Override
    public boolean evaluate(T actualValue) {
      validate(actualValue);
      return (actualValue.compareTo(getExpectedValue()) < 0);
    }
  }

  /**
   * The LessThanEqualToOperator class implements the less than equal to relational comparison.
   *
   * @param <T> the Class type of the Comparable operands used in the relational comparison.
   */
  private static final class LessThanEqualToOperator<T extends Comparable<T>> extends AbstractRelationalOperator<T> {

    public LessThanEqualToOperator(T expectedValue) {
      super("less than equal to", "<=", expectedValue);
    }

    @Override
    public boolean evaluate(T actualValue) {
      validate(actualValue);
      return (actualValue.compareTo(getExpectedValue()) <= 0);
    }
  }
}
