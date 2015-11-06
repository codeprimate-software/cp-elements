/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * 
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * 
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * 
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * 
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.lang;

import org.cp.elements.lang.annotation.DSL;

/**
 * The LangExtensions class provides methods to write natural language expressions for various conditions, such as
 * equality comparisons, identity checks, null checks, negation and so on, and operations such as conversion, etc.
 * 
 * @author John J. Blum
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class LangExtensions {

  /**
   * The assertThat operator used to assert the state of an object, such as it's equality, identity, nullity,
   * relational value, and so on.
   *
   * @param <T> the class type of the object subject to assertion.
   * @param obj the Object to be asserted.
   * @return an instance of the AssertThat DSL expression for making assertions about an object's state.
   * @see org.cp.elements.lang.annotation.DSL
   */
  @DSL
  public static <T> AssertThat<T> assertThat(final T obj) {
    return new AssertThatExpression<>(obj);
  }

  public interface AssertThat<T> extends DslExtension {

    void isEqualTo(T obj);

    void isFalse();

    void isNotNull();

    void isNull();

    void isSameAs(T obj);

    void isTrue();

    AssertThat<T> not();

  }

  private static final class AssertThatExpression<T> implements AssertThat<T> {

    private static final boolean DEFAULT_EXPECTED = true;

    private static final String NOT = "not ";

    private final boolean expected;

    private final T obj;

    private AssertThatExpression(final T obj) {
      this(obj, DEFAULT_EXPECTED);
    }

    private AssertThatExpression(final T obj, final boolean expected) {
      this.obj = obj;
      this.expected = expected;
    }

    private String negate(final String value) {
      return (expected ? value : "");
    }

    private boolean notEqualToExpected(final boolean actual) {
      return !(actual == expected);
    }

    public void isEqualTo(final T obj) {
      if (notEqualToExpected(is(this.obj).equalTo(obj))) {
        throw new AssertionFailedException(String.format("(%1$s) is %2$sequal to object (%3$s)",
          this.obj, negate(NOT), obj));
      }
    }

    public void isFalse() {
      if (notEqualToExpected(is(obj).False())) {
        throw new AssertionFailedException(String.format("(%1$s) is %2$sfalse", obj, negate(NOT)));
      }
    }

    public void isNotNull() {
      not().isNull();
    }

    public void isNull() {
      if (notEqualToExpected(is(obj).Null())) {
        throw new AssertionFailedException(String.format("(%1$s) is %2$snull", obj, negate(NOT)));
      }
    }

    public void isSameAs(final T obj) {
      if (notEqualToExpected(is(this.obj).sameAs(obj))) {
        throw new AssertionFailedException(String.format("(%1$s) is %2$sthe same as object (%3$s)",
          this.obj, negate(NOT), obj));
      }
    }

    public void isTrue() {
      if (notEqualToExpected(is(obj).True())) {
        throw new AssertionFailedException(String.format("(%1$s) is %2$strue", obj, negate(NOT)));
      }
    }

    public AssertThat<T> not() {
      return new AssertThatExpression<>(this.obj, !expected);
    }
  }

  /**
   * The is operator can be used to make logical determinations about an object such as boolean, equality, identity,
   * relational or type comparisons with other objects, and so on.
   * 
   * @param <T> the type of Object as the subject of the is operator.
   * @param obj the Object that is the subject of the operation.
   * @return an instance of the is operator.
   * @see org.cp.elements.lang.annotation.DSL
   */
  @DSL
  public static <T> Is<T> is(final T obj) {
    return new IsExpression<>(obj);
  }

  /**
   * The Is interface defines operations to classify a single object based on it's identity, state, type or relationship
   * to another object.
   * 
   * @param <T> the type of Object as the subject of the is operator.
   * @see org.cp.elements.lang.DslExtension
   */
  public interface Is<T> extends DslExtension {

    /**
     * Determines whether the Class object provided to the is operator is assignable from the Class type parameter.
     * 
     * @param type the Class type to check for assignment compatibility.
     * @return a boolean value indicating if the Class type parameter is assignable to the Class object provided in
     * this is operator.
     * @see java.lang.Class#isAssignableFrom(Class)
     */
    boolean assignableFrom(Class type);

    /**
     * Determines whether the object provided to the is operator is equal to the object parameter.  The objects are
     * considered equal as determined by their compareTo method.  This implies that the objects in the equality
     * comparison must implement the Comparable interface.
     * 
     * @param obj the Object parameter used in the equality comparison.
     * @return a boolean value indicating whether the objects are equal.
     * @see java.lang.Comparable#compareTo(Object)
     */
    boolean equalByComparison(T obj);

    /**
     * Determines whether the object provided to the is operator is equal to the object parameter.  The objects are
     * considered equal when neither is null, both refer to the same object, or both objects have the same value as
     * determined by their equals method.
     * 
     * @param obj the Object parameter used in the equality comparison.
     * @return a boolean value indicating whether the objects are equal.
     * @see java.lang.Object#equals(Object)
     */
    boolean equalTo(T obj);

    /**
     * Determines whether the object provided to the is operator actually evaluates to the value false.  An object
     * is false if and only if the value is actually false and not null or some other value (such as true).
     * 
     * @return a boolean value of true if the object in question is indeed the value false.
     * @see java.lang.Boolean#FALSE
     */
    boolean False();

    /**
     * Determines whether the object provided to the is operator is greater than the specified value, as determined
     * by the Comparable object's compareTo method.
     * 
     * @param lowerBound the lower bound value for which the object must be greater than.
     * @return a boolean value indicating if the object is greater than the specified value.
     * @see java.lang.Comparable#compareTo(Object)
     */
    boolean greaterThan(T lowerBound);

    /**
     * Determines whether the object provided to the is operator is greater than some specified lower bound value
     * and also less than some specified upper bound value, as determined by the Comparable object's compareTo method.
     * 
     * @param lowerBound the lower bound value for which the object must be greater than.
     * @param upperBound the upper bound value for which the object must be less than.
     * @return a boolean value indicating if the object is greater than the lower bound value and less than the
     * upper bound value.
     * @see java.lang.Comparable#compareTo(Object)
     */
    boolean greaterThanAndLessThan(T lowerBound, T upperBound);

    /**
     * Determines whether the object provided to the is operator is greater than some specified lower bound value
     * and also less than or equal to some specified upper bound value, as determined by the Comparable object's
     * compareTo method.
     * 
     * @param lowerBound the lower bound value for which the object must be greater than.
     * @param upperBound the upper bound value for which the object must be less than or equal to.
     * @return a boolean value indicating if the object is greater than the lower bound value and less than or equal to
     * the upper bound value.
     * @see java.lang.Comparable#compareTo(Object)
     */
    boolean greaterThanAndLessThanEqualTo(T lowerBound, T upperBound);

    /**
     * Determines whether the object provided to the is operator is greater than or equal to the specified value,
     * as determined by the Comparable object's compareTo method.
     * 
     * @param lowerBound the lower bound value for which the object must be greater than or equal to.
     * @return a boolean value indicating if the object is greater than or equal to the specified value.
     * @see java.lang.Comparable#compareTo(Object)
     */
    boolean greaterThanEqualTo(T lowerBound);

    /**
     * Determines whether the object provided to the is operator is greater than or equal to some specified
     * lower bound value and also less than some specified upper bound value, as determined by the Comparable object's
     * compareTo method.
     * 
     * @param lowerBound the lower bound value for which the object must be greater than or equal to.
     * @param upperBound the upper bound value for which the object must be less than.
     * @return a boolean value indicating if the object is greater than or equal to the lower bound value and less than
     * the upper bound value.
     * @see java.lang.Comparable#compareTo(Object)
     */
    boolean greaterThanEqualToAndLessThan(T lowerBound, T upperBound);

    /**
     * Determines whether the object provided to the is operator is greater than or equal to some specified
     * lower bound value and also less than or equal to some specified upper bound value, as determined by
     * the Comparable object's compareTo method.
     * 
     * @param lowerBound the lower bound value for which the object must be greater than or equal to.
     * @param upperBound the upper bound value for which the object must be less than or equal to.
     * @return a boolean value indicating if the object is greater than or equal to the lower bound value
     * and less than or equal to the upper bound value.
     * @see java.lang.Comparable#compareTo(Object)
     */
    boolean greaterThanEqualToAndLessThanEqualTo(T lowerBound, T upperBound);

    /**
     * Determines whether the object provided to the is operator is an instance of the specified class type.
     * 
     * @param type the Class object used in determining if the object in question is an instance of the
     * specified Class.
     * @return a boolean value indicating whether the object in question is an instance of the specified Class.
     */
    boolean instanceOf(Class type);

    /**
     * Determines whether the object provided to the is operator is less than the specified value, as determined by
     * the Comparable object's compareTo method.
     * 
     * @param upperBound the upper bound value for which the object must be less than.
     * @return a boolean value indicating if the object is less than the specified value.
     * @see java.lang.Comparable#compareTo(Object)
     */
    boolean lessThan(T upperBound);

    /**
     * Determines whether the object provided to the is operator is less than some specified lower upper bound value
     * or greater than some specified upper lower bound value, as determined by the Comparable object's
     * compareTo method.
     * 
     * @param upperBound the upper bound value for which the object must be less than.
     * @param lowerBound the lower bound value for which the object must be greater than.
     * @return a boolean value indicating if the object is less than the upper bound value
     * or is greater than the lower bound value.
     * @see java.lang.Comparable#compareTo(Object)
     */
    boolean lessThanOrGreaterThan(T upperBound, T lowerBound);

    /**
     * Determines whether the object provided to the is operator is less than some specified lower upper bound value
     * or greater than or equal to some specified upper lower bound value, as determined by the Comparable object's
     * compareTo method.
     * 
     * @param upperBound the upper bound value for which the object must be less than.
     * @param lowerBound the lower bound value for which the object must be greater than or equal to.
     * @return a boolean value indicating if the object is less than the upper bound value or is greater than
     * or equal to the lower bound value.
     * @see java.lang.Comparable#compareTo(Object)
     */
    boolean lessThanOrGreaterThanEqualTo(T upperBound, T lowerBound);

    /**
     * Determines whether the object provided to the is operator is less than or equal to the specified value,
     * as determined by the Comparable object's compareTo method.
     * 
     * @param upperBound the upper bound value for which the object must be less than or equal to.
     * @return a boolean value indicating if the object is less than or equal to the specified value.
     * @see java.lang.Comparable#compareTo(Object)
     */
    boolean lessThanEqualTo(T upperBound);

    /**
     * Determines whether the object provided to the is operator is less than or equal to some specified
     * lower upper bound value or greater than some specified upper lower bound value, as determined by
     * the Comparable object's compareTo method.
     * 
     * @param upperBound the upper bound value for which the object must be less than or equal to.
     * @param lowerBound the lower bound value for which the object must be greater than.
     * @return a boolean value indicating if the object is less than or equal to the upper bound value
     * or is greater than the lower bound value.
     * @see java.lang.Comparable#compareTo(Object)
     */
    boolean lessThanEqualToOrGreaterThan(T upperBound, T lowerBound);

    /**
     * Determines whether the object provided to the is operator is less than or equal to some specified
     * lower upper bound value or greater than or equal to some specified upper lower bound value, as determined by
     * the Comparable object's compareTo method.
     * 
     * @param upperBound the upper bound value for which the object must be less than or equal to.
     * @param lowerBound the lower bound value for which the object must be greater than or equal to.
     * @return a boolean value indicating if the object is less than or equal to the upper bound value
     * or is greater than or equal to the lower bound value.
     * @see java.lang.Comparable#compareTo(Object)
     */
    boolean lessThanEqualToOrGreaterThanEqualTo(T upperBound, T lowerBound);

    /**
     * Negates the expected outcome/result of this operator.
     * 
     * @return the instance of this Is operator negated.
     */
    Is<T> not();

    /**
     * Shortcut method for the not().Null() operation.  Determines whether the object provided to the is operator
     * is not null.
     * 
     * @return a boolean value indicating whether the object in question is not null.
     */
    boolean notNull();

    /**
     * Determines whether the object provided to the is operator is null.
     * 
     * @return a boolean value indicating whether the object in question is null.
     */
    boolean Null();

    /**
     * Shortcut method for the not().sameAs(:Object) operation.  Determines whether the object provided to
     * the is operator is *not* the same as, or does not refer to the same object in memory
     * as the given object parameter.
     *
     * @param obj the Object reference used to determine if the object in question is not a reference to the same object
     * in memory.
     * @return a boolean value indicating whether the object in question and object parameter reference do not refer to
     * the same object.
     */
    boolean notSameAs(T obj);

    /**
     * Determines whether the object provided to the is operator is the same as, or refers to the same object
     * in memory as the given object parameter.
     * 
     * @param obj the Object reference used to determine if the object in question is a reference to the same object
     * in memory.
     * @return a boolean value indicating whether the object in question and object parameter reference refers to
     * the same object.
     */
    boolean sameAs(T obj);

    /**
     * Determines whether the object provided to the is operator actually evaluates to the value true.  An object
     * is true if and only if the value is actually true and not null or some other value (such as false).
     * 
     * @return a boolean value of true if the object in question is indeed the value true
     * @see java.lang.Boolean#TRUE
     */
    boolean True();

  }

  /**
   * The IsExpression class is an implementation of the Is interface, is operator.  Note, this implementation is Thread-safe,
   * although it is very unlikely that a Thread will share an instance of this class since every invocation of the
   * is() operator factory method will return a new instance of this class, at least for the time being.
   * 
   * @param <T> the Object's type.
   * @see org.cp.elements.lang.LangExtensions.Is
   */
  private static final class IsExpression<T> implements Is<T> {

    private static final boolean DEFAULT_EXPECTED = true;

    private final boolean expected;

    private final T obj;

    private IsExpression(final T obj) {
      this(obj, DEFAULT_EXPECTED);
    }

    private IsExpression(final T obj, final boolean expected) {
      this.obj = obj;
      this.expected = expected;
    }

    private boolean equalToExpected(final boolean actualOutcome) {
      return (actualOutcome == expected);
    }

    private LogicalOperator getOp(final LogicalOperator op) {
      return (expected ? op : op.getOpposite());
    }

    @SuppressWarnings("unchecked")
    private Comparable<T> toComparable(T obj) {
      return (Comparable<T>) obj;
    }

    public boolean assignableFrom(final Class type) {
      return equalToExpected(this.obj != null && type != null && ((Class<?>) this.obj).isAssignableFrom(type));
    }

    public boolean equalByComparison(final T obj) {
      return equalToExpected(toComparable(this.obj).compareTo(obj) == 0);
    }

    public boolean equalTo(final T obj) {
      return equalToExpected(this.obj != null && this.obj.equals(obj));
    }

    public boolean False() {
      return equalToExpected(Boolean.FALSE.equals(this.obj));
    }

    public boolean greaterThan(final T lowerBound) {
      return equalToExpected(toComparable(this.obj).compareTo(lowerBound) > 0);
    }

    public boolean greaterThanAndLessThan(final T lowerBound, final T upperBound) {
      return getOp(LogicalOperator.AND).evaluate(greaterThan(lowerBound), lessThan(upperBound));
    }

    public boolean greaterThanAndLessThanEqualTo(final T lowerBound, final T upperBound) {
      return getOp(LogicalOperator.AND).evaluate(greaterThan(lowerBound), lessThanEqualTo(upperBound));
    }

    public boolean greaterThanEqualTo(final T lowerBound) {
      return equalToExpected(toComparable(this.obj).compareTo(lowerBound) >= 0);
    }

    public boolean greaterThanEqualToAndLessThan(final T lowerBound, final T upperBound) {
      return getOp(LogicalOperator.AND).evaluate(greaterThanEqualTo(lowerBound), lessThan(upperBound));
    }

    public boolean greaterThanEqualToAndLessThanEqualTo(final T lowerBound, final T upperBound) {
      return getOp(LogicalOperator.AND).evaluate(greaterThanEqualTo(lowerBound), lessThanEqualTo(upperBound));
    }

    public boolean instanceOf(final Class type) {
      return equalToExpected(type != null && type.isInstance(this.obj));
    }

    public boolean lessThan(final T upperBound) {
      return equalToExpected(toComparable(this.obj).compareTo(upperBound) < 0);
    }

    public boolean lessThanOrGreaterThan(final T upperBound, final T lowerBound) {
      return getOp(LogicalOperator.OR).evaluate(lessThan(upperBound), greaterThan(lowerBound));
    }

    public boolean lessThanOrGreaterThanEqualTo(final T upperBound, final T lowerBound) {
      return getOp(LogicalOperator.OR).evaluate(lessThan(upperBound), greaterThanEqualTo(lowerBound));
    }

    public boolean lessThanEqualTo(final T upperBound) {
      return equalToExpected(toComparable(this.obj).compareTo(upperBound) <= 0);
    }

    public boolean lessThanEqualToOrGreaterThan(final T upperBound, final T lowerBound) {
      return getOp(LogicalOperator.OR).evaluate(lessThanEqualTo(upperBound), greaterThan(lowerBound));
    }

    public boolean lessThanEqualToOrGreaterThanEqualTo(final T upperBound, final T lowerBound) {
      return getOp(LogicalOperator.OR).evaluate(lessThanEqualTo(upperBound), greaterThanEqualTo(lowerBound));
    }

    public Is<T> not() {
      return new IsExpression<>(this.obj, !expected);
    }

    public boolean notNull() {
      return not().Null();
    }

    public boolean Null() {
      return equalToExpected(this.obj == null);
    }

    public boolean notSameAs(final T obj) {
      return not().sameAs(obj);
    }

    public boolean sameAs(final T obj) {
      return equalToExpected(this.obj == obj);
    }

    public boolean True() {
      return equalToExpected(Boolean.TRUE.equals(this.obj));
    }
  }

}
