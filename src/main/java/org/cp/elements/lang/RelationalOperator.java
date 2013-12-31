/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.lang;

/**
 * The RelationalOperator class defines class representations for the relation operators (==, >, >=, < and <=).
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.lang.LogicalOperator
 * @since 1.0.0
 */
public abstract class RelationalOperator<T extends Comparable<T>> {

  /**
   * Gets the RelationalOperator performing equality comparisons to determine whether all provided values are equal to
   * the given expected value.
   * <p/>
   * @param value the expected object value of the equality comparison.
   * @param <T> the expected Class type for the object used in the equality comparison.
   * @return a RelationalOperator for the equality comparison.
   */
  public static <T extends Comparable<T>> RelationalOperator<T> equalTo(final T value) {
    return new EqualToOperator<T>(value);
  }

  /**
   * Gets the RelationalOperator performing greater than comparisons to determine whether all provided values
   * are greater than the given lower bound value.
   * <p/>
   * @param lowerBound the Comparable lower bounded value.
   * @param <T> the expected Class type for the object used in the greater than comparison.
   * @return a RelationalOperator for the greater than comparison.
   */
  public static <T extends Comparable<T>> RelationalOperator<T> greaterThan(final T lowerBound) {
    return new GreaterThanOperator<T>(lowerBound);
  }

  /**
   * Gets the RelationalOperator performing greater than and less than comparisons to determine whether all provided
   * values are greater than some lower bound value and also less than some upper bound value.
   * <p/>
   * @param lowerBound the Comparable lower bounded value.
   * @param upperBound the Comparable upper bounded value.
   * @param <T> the expected Class type for the object used in the greater than and less than comparison.
   * @return a RelationalOperator for the greater than and less than comparison.
   */
  public static <T extends Comparable<T>> RelationalOperator<T> greaterThanAndLessThan(final T lowerBound, final T upperBound) {
    return ComposableRelationalOperator.compose(greaterThan(lowerBound), LogicalOperator.AND, lessThan(upperBound));
  }

  /**
   * Gets the RelationalOperator performing greater than and less than equal to comparisons to determine whether all
   * provided values are greater than some lower bound value and also less than equal to some upper bound value.
   * <p/>
   * @param lowerBound the Comparable lower bounded value.
   * @param upperBound the Comparable upper bounded value.
   * @param <T> the expected Class type for the object used in the greater than and less than equal to comparison.
   * @return a RelationalOperator for the greater than and less than equal to comparison.
   */
  public static <T extends Comparable<T>> RelationalOperator<T> greaterThanAndLessThanEqualTo(final T lowerBound, final T upperBound) {
    return ComposableRelationalOperator.compose(greaterThan(lowerBound), LogicalOperator.AND, lessThanEqualTo(upperBound));
  }

  /**
   * Gets the RelationalOperator performing greater than equal to comparisons to determine whether all provided values
   * are greater than equal to the given lower bound value.
   * <p/>
   * @param lowerBound the Comparable lower bounded value.
   * @param <T> the expected Class type for the object used in the greater than equal to comparison.
   * @return a RelationalOperator for the greater than equal to comparison.
   */
  public static <T extends Comparable<T>> RelationalOperator<T> greaterThanEqualTo(final T lowerBound) {
    return new GreaterThanEqualToOperator<T>(lowerBound);
  }

  /**
   * Gets the RelationalOperator performing greater than equal to and less than comparisons to determine whether all
   * provided values are greater than equal to some lower bound value and also less than some upper bound value.
   * <p/>
   * @param lowerBound the Comparable lower bounded value.
   * @param upperBound the Comparable upper bounded value.
   * @param <T> the expected Class type for the object used in the greater than equal to and less than comparison.
   * @return a RelationalOperator for the greater than equal to and less than comparison.
   */
  public static <T extends Comparable<T>> RelationalOperator<T> greaterThanEqualToAndLessThan(final T lowerBound, final T upperBound) {
    return ComposableRelationalOperator.compose(greaterThanEqualTo(lowerBound), LogicalOperator.AND, lessThan(upperBound));
  }

  /**
   * Gets the RelationalOperator performing greater than equal to and less than equal to comparisons to determine
   * whether all provided values are greater than equal to some lower bound value and also less than equal to some
   * upper bound value.
   * <p/>
   * @param lowerBound the Comparable lower bounded value.
   * @param upperBound the Comparable upper bounded value.
   * @param <T> the expected Class type for the object used in the greater than equal to and less than equal to
   * comparison.
   * @return a RelationalOperator for the greater than equal to and less than equal to comparison.
   */
  public static <T extends Comparable<T>> RelationalOperator<T> greaterThanEqualToAndLessThanEqualTo(final T lowerBound, final T upperBound) {
    return ComposableRelationalOperator.compose(greaterThanEqualTo(lowerBound), LogicalOperator.AND, lessThanEqualTo(upperBound));
  }

  /**
   * Gets the RelationalOperator performing less than comparisons to determine whether all provided values are less than
   * the given upper bound value.
   * <p/>
   * @param lowerBound the Comparable upper bounded value.
   * @param <T> the expected Class type for the object used in the less than comparison.
   * @return a RelationalOperator for the less than comparison.
   */
  public static <T extends Comparable<T>> RelationalOperator<T> lessThan(final T lowerBound) {
    return new LessThanOperator<T>(lowerBound);
  }

  /**
   * Gets the RelationalOperator performing less than or greater than comparisons to determine whether all provided
   * values are less than some upper bound value or possibly greater than some lower bound value.
   * <p/>
   * @param lowerBound the Comparable upper bounded value.
   * @param upperBound the Comparable lower bounded value.
   * @param <T> the expected Class type for the object used in the less than or greater than comparison.
   * @return a RelationalOperator for the less than or greater than comparison.
   */
  public static <T extends Comparable<T>> RelationalOperator<T> lessThanOrGreaterThan(final T lowerBound, final T upperBound) {
    return ComposableRelationalOperator.compose(lessThan(lowerBound), LogicalOperator.OR, greaterThan(upperBound));
  }

  /**
   * Gets the RelationalOperator performing less than or greater than equal to comparisons to determine whether all
   * provided values are less than some upper bound value or possibly greater than equal to some lower bound value.
   * <p/>
   * @param lowerBound the Comparable lower upper bounded value.
   * @param upperBound the Comparable upper lower bounded value.
   * @param <T> the expected Class type for the object used in the less than or greater than equal to comparison.
   * @return a RelationalOperator for the less than or greater than equal to comparison.
   */
  public static <T extends Comparable<T>> RelationalOperator<T> lessThanOrGreaterThanEqualTo(final T lowerBound, final T upperBound) {
    return ComposableRelationalOperator.compose(lessThan(lowerBound), LogicalOperator.OR, greaterThanEqualTo(upperBound));
  }

  /**
   * Gets the RelationalOperator performing less than equal to comparisons to determine whether all provided values
   * are less than equal to the given upper bound value.
   * <p/>
   * @param lowerBound the Comparable upper bounded value.
   * @param <T> the expected Class type for the object used in the less than equal to comparison.
   * @return a RelationalOperator for the less than equal to comparison.
   */
  public static <T extends Comparable<T>> RelationalOperator<T> lessThanEqualTo(final T lowerBound) {
    return new LessThanEqualToOperator<T>(lowerBound);
  }

  /**
   * Gets the RelationalOperator performing less than equal to or greater than comparisons to determine whether all
   * provided values are less than equal to some upper bound value or possibly greater than some lower bound value.
   * <p/>
   * @param lowerBound the Comparable upper bounded value.
   * @param upperBound the Comparable lower bounded value.
   * @param <T> the expected Class type for the object used in the less than equal to or greater than comparison.
   * @return a RelationalOperator for the less than equal to or greater than comparison.
   */
  public static <T extends Comparable<T>> RelationalOperator<T> lessThanEqualToOrGreaterThan(final T lowerBound, final T upperBound) {
    return ComposableRelationalOperator.compose(lessThanEqualTo(lowerBound), LogicalOperator.OR, greaterThan(upperBound));
  }

  /**
   * Gets the RelationalOperator performing less than equal to or greater than equal to comparisons to determine whether
   * all provided values are less than equal to some upper bound value or possibly greater than equal to some lower
   * bound value.
   * <p/>
   * @param lowerBound the Comparable upper bounded value.
   * @param upperBound the Comparable lower bounded value.
   * @param <T> the expected Class type for the object used in the less than equal to or greater than equal to
   * comparison.
   * @return a RelationalOperator for the less than equal to or greater than equal to comparison.
   */
  public static <T extends Comparable<T>> RelationalOperator<T> lessThanEqualToOrGreaterThanEqualTo(final T lowerBound, final T upperBound) {
    return ComposableRelationalOperator.compose(lessThanEqualTo(lowerBound), LogicalOperator.OR, greaterThanEqualTo(upperBound));
  }

  /**
   * Gets a String describing the relational operator, such as 'equal to'.
   * <p/>
   * @return a String value describing the relational operator.
   */
  public abstract String getDescription();

  /**
   * Gets the symbolic representation of the relational operator, such as ==, >, >=, <, <= and so on.
   * <p/>
   * @return a String value symbolizing the relational operator.
   */
  public abstract String getSymbol();

  /**
   * Performs the relational comparison between the actual and expected value(s) provided when the relational operator
   * was instantiated.
   * <p/>
   * @param actualValue the actual Comparable value used in the relational operation to perform the evaluation.
   * @return a boolean value indicating whether the actual Comparable value satisfies the constraints of the
   * relational comparison, with respect to it's expected values.
   */
  public abstract boolean evaluate(T actualValue);

  /**
   * Gets a String describing this relational operator.
   * <p/>
   * @return a String value to describe this relational operator.
   * @see org.cp.elements.lang.RelationalOperator#getDescription()
   */
  @Override
  public String toString() {
    return getDescription();
  }

  /**
   * Common abstract base class for all ReltaionalOperator implementation classes.
   * <p/>
   * @param <T> the Class type of Comparable object expected in the relational comparison.
   * @see org.cp.elements.lang.RelationalOperator.EqualToOperator
   * @see org.cp.elements.lang.RelationalOperator.GreaterThanOperator
   * @see org.cp.elements.lang.RelationalOperator.GreaterThanEqualToOperator
   * @see org.cp.elements.lang.RelationalOperator.LessThanOperator
   * @see org.cp.elements.lang.RelationalOperator.LessThanEqualToOperator
   * @see org.cp.elements.lang.RelationalOperator.ComposableRelationalOperator
   */
  protected static abstract class AbstractRelationalOperator<T extends Comparable<T>> extends RelationalOperator<T> {

    private final String description;
    private final String symbol;

    private final T expectedValue;

    /**
     * Private constructor used by the ComposableRelationalOperator.  Constructs an instance of the RelationalOperator
     * with a description and symbol for the relational operator.  In this case, the relational operator does not have
     * an expected value since the relational operator is composed of relational operators that do.
     * <p/>
     * @param description a String describing the relational operator.
     * @param symbol a String to symbolically represent the relational operator.
     * @throws IllegalArgumentException if either the description or symbol are not specified.
     */
    private AbstractRelationalOperator(final String description, final String symbol) {
      Assert.notBlank(description, "The description of the relational operator must be specified!");
      Assert.notBlank(symbol, "The symbol of the relation operator must be specified!");
      this.description = description;
      this.symbol = symbol;
      this.expectedValue = null;
    }

    /**
     * Constructor to create new instances of the RelationalOperator class given a new and different kind of
     * relational operator.
     * <p/>
     * @param description a String value describing the relational operator.
     * @param symbol a String value to symbolically represent the relational operator.
     * @param expectedValue the Comparable value expected during the evaluation of the relational operation.
     * @throws IllegalArgumentException if either the description or symbol are not specified.
     * @throws NullPointerException if the expected Comparable value is not specified.
     */
    protected AbstractRelationalOperator(final String description, final String symbol, final T expectedValue) {
      Assert.notBlank(description, "The description of the relational operator must be specified!");
      Assert.notBlank(symbol, "The symbol of the relation operator must be specified!");
      Assert.notNull(expectedValue, "The expected value in the {0} comparison cannot be null!", description);
      this.description = description;
      this.symbol = symbol;
      this.expectedValue = expectedValue;
    }

    /**
     * Gets a String describing the relational operator, such as 'equal to'.
     * <p/>
     * @return a String describing the relational operator.
     */
    @Override
    public String getDescription() {
      return this.description;
    }

    /**
     * Gets the expected Comparable value used in the relational comparison to constrain actual values.
     * <p/>
     * @return the expected Comparable value used in the relational comparison to constrain the actual values.
     */
    protected final T getExpectedValue() {
      return expectedValue;
    }

    /**
     * Gets the symbolic representation of the relational operator, such as ==, >, >=, <, <= and so on.
     * <p/>
     * @return a String symbolizing the relational operator.
     */
    @Override
    public String getSymbol() {
      return symbol;
    }

    /**
     * Verifies the validity of the actual Comparable value with the expected Comparable value.
     * <p/>
     * @param actualValue the actual Comparable value in the relational comparison.
     * @throws NullPointerException if the actual Comparable value is null.
     */
    protected void validate(final T actualValue) {
      Assert.notNull(actualValue, "The actual value in the {0} comparison cannot be null!", this.description);
    }
  }

  /**
   * The ComposableRelationalOperator class allows various relational operators to be combined into a compound
   * relational operation using a logical operator (such as AND or OR).
   * <p/>
   * @param <T> the Class type of the Comparable operands used in the relational comparison.
   * @see org.cp.elements.lang.LogicalOperator
   */
  protected static final class ComposableRelationalOperator<T extends Comparable<T>> extends AbstractRelationalOperator<T> {

    private final LogicalOperator operator;

    private final RelationalOperator<T> leftOperand;
    private final RelationalOperator<T> rightOperand;

    /**
     * Constructs an instance of the ComposableRelationalOperator class with the specified relational operator operands
     * and combining logical operator.
     * <p/>
     * @param leftOperand a RelationalOperator operand on the left-side of the logical operation.
     * @param operator the LogicalOperator used to the combine the two ReltaionalOperator operands in a compound
     * relational comparison.
     * @param rightOperand a RelationalOperator operand on the right-side of the logical operation.
     */
    private ComposableRelationalOperator(final RelationalOperator<T> leftOperand,
                                         final LogicalOperator operator,
                                         final RelationalOperator<T> rightOperand)
    {
      super(StringUtils.singleSpaceValues(leftOperand, operator, rightOperand),
        StringUtils.singleSpaceValues(leftOperand.getSymbol(), operator.getSymbol(), rightOperand.getSymbol()));
      this.leftOperand = leftOperand;
      this.operator = operator;
      this.rightOperand = rightOperand;
    }

    /**
     * Factory method used in composing a compound relational operator comparison, such as > && <, or < || >.
     * <p/>
     * @param leftOperand a RelationalOperator operand on the left-side of the logical operation.
     * @param operator the LogicalOperator used to the combine the two ReltaionalOperator operands in a compound
     * relational comparison.
     * @param rightOperand a RelationalOperator operand on the right-side of the logical operation.
     * @param <T> the Class type of the Comparable operands used in the relational comparison.
     * @return the left RelationalOperator operand if the right operand is null, or the right RelationalOperator
     * operand if the left is null, or a new ComposableRelationalOperator combining the two individual
     * RelationalOperator operands into a compound relational comparison using the specified LogicalOperator.
     */
    public static <T extends Comparable<T>> RelationalOperator<T> compose(final RelationalOperator<T> leftOperand,
                                                                          final LogicalOperator operator,
                                                                          final RelationalOperator<T> rightOperand)
    {
      return (leftOperand == null ? rightOperand : (rightOperand == null ? leftOperand
        : new ComposableRelationalOperator<T>(leftOperand, operator, rightOperand)));
    }

    /**
     * Gets the left-side operand in the logical operation.
     * <p/>
     * @return the RelationalOperator constituting the operand on the left-side of the logical operation.
     */
    protected RelationalOperator<T> getLeftOperand() {
      return leftOperand;
    }

    /**
     * Gets the logical operator used to combine the two independent RelationaOperator operands into a compound
     * relational comparison.
     * <p/>
     * @return the LogicalOperator used to combine the two RelationalOperators into a compound relational comparison.
     * @see org.cp.elements.lang.LogicalOperator
     */
    protected LogicalOperator getOperator() {
      return operator;
    }

    /**
     * Gets the right-side operand in the logical operation.
     * <p/>
     * @return the RelationalOperator constituting the operand on the right-side of the logical operation.
     */
    protected RelationalOperator<T> getRightOperand() {
      return rightOperand;
    }

    /**
     * Performs the relational comparison between the provided actual value and the expected value(s), as determined
     * when the relational operator was instantiated.
     * <p/>
     * @param actualValue the actual Comparable value used in the relational operation to perform the evaluation.
     * @return a boolean value indicating whether the actual Comparable value satisfies the constraints of the
     * relational comparison, with respect to it's expected values.
     */
    @Override
    public boolean evaluate(final T actualValue) {
      return getOperator().evaluate(getLeftOperand().evaluate(actualValue), getRightOperand().evaluate(actualValue));
    }
  }

  /**
   * The EqualToOperator class implements equality relational comparisons.
   * <p/>
   * @param <T> the Class type of the Comparable operands used in the relational comparison.
   */
  private static final class EqualToOperator<T extends Comparable<T>> extends AbstractRelationalOperator<T> {

    public EqualToOperator(final T expectedValue) {
      super("equal to", "==", expectedValue);
    }

    @Override
    public boolean evaluate(final T actualValue) {
      validate(actualValue);
      return (actualValue.equals(getExpectedValue()));
      //return (actualValue.compareTo(getExpectedValue()) == 0);
    }
  }

  /**
   * The GreaterThanOperator class implements the greater than relational comparison.
   * <p/>
   * @param <T> the Class type of the Comparable operands used in the relational comparison.
   */
  private static final class GreaterThanOperator<T extends Comparable<T>> extends AbstractRelationalOperator<T> {

    public GreaterThanOperator(final T expectedValue) {
      super("greater than", ">", expectedValue);
    }

    @Override
    public boolean evaluate(final T actualValue) {
      validate(actualValue);
      return (actualValue.compareTo(getExpectedValue()) > 0);
    }
  }

  /**
   * The GreaterThanEqualToOperator class implements the greater than equal to relational comparison.
   * <p/>
   * @param <T> the Class type of the Comparable operands used in the relational comparison.
   */
  private static final class GreaterThanEqualToOperator<T extends Comparable<T>> extends AbstractRelationalOperator<T> {

    public GreaterThanEqualToOperator(final T expectedValue) {
      super("greater than equal to", ">=", expectedValue);
    }

    @Override
    public boolean evaluate(final T actualValue) {
      validate(actualValue);
      return (actualValue.compareTo(getExpectedValue()) >= 0);
    }
  }

  /**
   * The LessThanOperator class implements the less than relational comparison.
   * <p/>
   * @param <T> the Class type of the Comparable operands used in the relational comparison.
   */
  private static final class LessThanOperator<T extends Comparable<T>> extends AbstractRelationalOperator<T> {

    public LessThanOperator(final T expectedValue) {
      super("less than", "<", expectedValue);
    }

    @Override
    public boolean evaluate(final T actualValue) {
      validate(actualValue);
      return (actualValue.compareTo(getExpectedValue()) < 0);
    }
  }

  /**
   * The LessThanEqualToOperator class implements the less than equal to relational comparison.
   * <p/>
   * @param <T> the Class type of the Comparable operands used in the relational comparison.
   */
  private static final class LessThanEqualToOperator<T extends Comparable<T>> extends AbstractRelationalOperator<T> {

    public LessThanEqualToOperator(final T expectedValue) {
      super("less than equal to", "<=", expectedValue);
    }

    @Override
    public boolean evaluate(final T actualValue) {
      validate(actualValue);
      return (actualValue.compareTo(getExpectedValue()) <= 0);
    }
  }

}
