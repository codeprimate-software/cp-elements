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

import java.text.MessageFormat;
import java.util.Collection;
import java.util.Map;

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
   * The assertThat operator is used to assert the state of an object, such as it's equality, identity, nullity,
   * relational value, and so on.
   *
   * @param <T> the class type of the object subject to assertion.
   * @param obj the Object to be asserted.
   * @return an instance of the AssertThat DSL expression for making assertions about an object's state.
   * @see org.cp.elements.lang.annotation.DSL
   * @see org.cp.elements.lang.LangExtensions.AssertThatExpression
   */
  @DSL
  public static <T> AssertThat<T> assertThat(final T obj) {
    return new AssertThatExpression<>(obj);
  }

  public interface AssertThat<T> extends DslExtension {

    void isAssignableTo(Class type);

    void isComparableTo(Comparable<T> obj);

    void isEqualTo(T obj);

    void isNotEqualTo(T obj);

    void isFalse();

    void isGreaterThan(T lowerBound);

    void isGreaterThanAndLessThan(T lowerBound, T upperBound);

    void isGreaterThanAndLessThanEqualTo(T lowerBound, T upperBound);

    void isGreaterThanEqualTo(T lowerBound);

    void isGreaterThanEqualToAndLessThan(T lowerBound, T upperBound);

    void isGreaterThanEqualToAndLessThanEqualTo(T lowerBound, T upperBound);

    void hasText();

    void holdsLock(Object lock);

    void isInstanceOf(Class type);

    void isLessThan(T upperBound);

    void isLessThanOrGreaterThan(T upperBound, T lowerBound);

    void isLessThanOrGreaterThanEqualTo(T upperBound, T lowerBound);

    void isLessThanEqualTo(T upperBound);

    void isLessThanEqualToOrGreaterThan(T upperBound, T lowerBound);

    void isLessThanEqualToOrGreaterThanEqualTo(T upperBound, T lowerBound);

    AssertThat<T> not();

    void isNotBlank();

    void isNotEmpty();

    void isNotNull();

    void isNull();

    void isSameAs(T obj);

    void isNotSameAs(T obj);

    void isTrue();

    AssertThat<T> throwing(RuntimeException e);

    AssertThat<T> using(String message, Object... args);

    AssertThat<T> when(Condition condition);

  }

  private static final class AssertThatExpression<T> implements AssertThat<T> {

    private static final boolean DEFAULT_EXPECTED = true;

    private static final String NOT = "not ";

    private final boolean expected;

    private final T obj;

    private Condition condition;

    private RuntimeException cause;

    private String message;

    private AssertThatExpression() {
      this(null, DEFAULT_EXPECTED);
    }

    private AssertThatExpression(final T obj) {
      this(obj, DEFAULT_EXPECTED);
    }

    private AssertThatExpression(final T obj, final boolean expected) {
      this.obj = obj;
      this.expected = expected;
      this.condition = () -> true;
    }

    private boolean conditionHolds() {
      return condition.evaluate();
    }

    private boolean notEqualToExpected(final boolean actual) {
      return !(actual == expected);
    }

    public void isAssignableTo(final Class type) {
      if (conditionHolds()) {
        if (notEqualToExpected(is(obj).assignableTo(type))) {
          throwAssertionError("(%1$s) is %2$sassignable to (%3$s)", obj, negate(NOT), ObjectUtils.getName(type));
        }
      }
    }

    @SuppressWarnings("unchecked")
    public void isComparableTo(final Comparable<T> comparable) {
      if (conditionHolds()) {
        if (notEqualToExpected(is((Comparable<T>) obj).comparableTo(comparable))) {
          throwAssertionError("(%1$s) is %2$scomparable to (%3$s)", obj, negate(NOT), comparable);
        }
      }
    }

    public void isEqualTo(final T obj) {
      if (conditionHolds()) {
        if (notEqualToExpected(is(this.obj).equalTo(obj))) {
          throwAssertionError("(%1$s) is %2$sequal to (%3$s)", this.obj, negate(NOT), obj);
        }
      }
    }

    public void isNotEqualTo(final T obj) {
      not().isEqualTo(obj);
    }

    public void isFalse() {
      if (conditionHolds()) {
        if (notEqualToExpected(is(obj).False())) {
          throwAssertionError("(%1$s) is %2$sfalse", obj, negate(NOT));
        }
      }
    }

    public void isGreaterThan(final T lowerBound) {
      if (conditionHolds()) {
        if (notEqualToExpected(is(obj).greaterThan(lowerBound))) {
          throwAssertionError("(%1$s) is %2$sgreater than (%3$s)", obj, negate(NOT), lowerBound);
        }
      }
    }

    public void isGreaterThanAndLessThan(final T lowerBound, final T upperBound) {
      if (conditionHolds()) {
        if (notEqualToExpected(is(obj).greaterThanAndLessThan(lowerBound, upperBound))) {
          throwAssertionError("(%1$s) is %2$sgreater than (%3$s) and less than (%4$s)", obj, negate(NOT),
            lowerBound, upperBound);
        }
      }
    }

    public void isGreaterThanAndLessThanEqualTo(final T lowerBound, final T upperBound) {
      if (conditionHolds()) {
        if (notEqualToExpected(is(obj).greaterThanAndLessThanEqualTo(lowerBound, upperBound))) {
          throwAssertionError("(%1$s) is %2$sgreater than (%3$s) and less than equal to (%4$s)", obj, negate(NOT),
            lowerBound, upperBound);
        }
      }
    }

    public void isGreaterThanEqualTo(final T lowerBound) {
      if (conditionHolds()) {
        if (notEqualToExpected(is(obj).greaterThanEqualTo(lowerBound))) {
          throwAssertionError("(%1$s) is %2$sgreater than equal to (%3$s)", obj, negate(NOT), lowerBound);
        }
      }
    }

    public void isGreaterThanEqualToAndLessThan(final T lowerBound, final T upperBound) {
      if (conditionHolds()) {
        if (notEqualToExpected(is(obj).greaterThanEqualToAndLessThan(lowerBound, upperBound))) {
          throwAssertionError("(%1$s) is %2$sgreater than equal to (%3$s) and less than (%4$s)", obj, negate(NOT),
            lowerBound, upperBound);
        }
      }
    }

    public void isGreaterThanEqualToAndLessThanEqualTo(final T lowerBound, final T upperBound) {
      if (conditionHolds()) {
        if (notEqualToExpected(is(obj).greaterThanEqualToAndLessThanEqualTo(lowerBound, upperBound))) {
          throwAssertionError("(%1$s) is %2$sgreater than equal to (%3$s) and less than equal to (%4$s)",
            obj, negate(NOT), lowerBound, upperBound);
        }
      }
    }

    public void hasText() {
      isNotBlank();
    }

    public void holdsLock(final Object lock) {
      if (conditionHolds()) {
        if (notEqualToExpected(Thread.holdsLock(lock))) {
          throwAssertionError("(%1$s) %2$slock (%3$s)", Thread.currentThread(),
            (expected ? "does not hold " : "holds "), lock);
        }
      }
    }

    public void isInstanceOf(final Class type) {
      if (conditionHolds()) {
        if (notEqualToExpected(is(obj).instanceOf(type))) {
          throwAssertionError("(%1$s) is %2$san instance of (%3$s)", obj, negate(NOT), ObjectUtils.getName(type));
        }
      }
    }

    public void isLessThan(final T upperBound) {
      if (conditionHolds()) {
        if (notEqualToExpected(is(obj).lessThan(upperBound))) {
          throwAssertionError("(%1$s) is %2$sless than (%3$s)", obj, negate(NOT), upperBound);
        }
      }
    }

    public void isLessThanOrGreaterThan(final T upperBound, final T lowerBound) {
      if (conditionHolds()) {
        if (notEqualToExpected(is(obj).lessThanOrGreaterThan(upperBound, lowerBound))) {
          throwAssertionError("(%1$s) is %2$sless than (%3$s) or greater than (%4$s)", obj, negate(NOT),
            upperBound, lowerBound);
        }
      }
    }

    public void isLessThanOrGreaterThanEqualTo(final T upperBound, final T lowerBound) {
      if (conditionHolds()) {
        if (notEqualToExpected(is(obj).lessThanOrGreaterThanEqualTo(upperBound, lowerBound))) {
          throwAssertionError("(%1$s) is %2$sless than (%3$s) or greater than equal to (%4$s)", obj, negate(NOT),
            upperBound, lowerBound);
        }
      }
    }

    public void isLessThanEqualTo(final T upperBound) {
      if (conditionHolds()) {
        if (notEqualToExpected(is(obj).lessThanEqualTo(upperBound))) {
          throwAssertionError("(%1$s) is %2$sless than equal to (%3$s)", obj, negate(NOT), upperBound);
        }
      }
    }

    public void isLessThanEqualToOrGreaterThan(final T upperBound, final T lowerBound) {
      if (conditionHolds()) {
        if (notEqualToExpected(is(obj).lessThanEqualToOrGreaterThan(upperBound, lowerBound))) {
          throwAssertionError("(%1$s) is %2$sless than equal to (%3$s) or greater than (%4$s)", obj, negate(NOT),
            upperBound, lowerBound);
        }
      }
    }

    public void isLessThanEqualToOrGreaterThanEqualTo(final T upperBound, final T lowerBound) {
      if (conditionHolds()) {
        if (notEqualToExpected(is(obj).lessThanEqualToOrGreaterThanEqualTo(upperBound, lowerBound))) {
          throwAssertionError("(%1$s) is %2$sless than equal to (%3$s) or greater than equal to (%4$s)",
            obj, negate(NOT), upperBound, lowerBound);
        }
      }
    }

    public AssertThat<T> not() {
      AssertThat<T> expression = new AssertThatExpression<>(this.obj, !expected);
      expression = expression.when(this.condition);
      return expression;
    }

    public void isNotBlank() {
      if (conditionHolds()) {
        if (notEqualToExpected(is(obj).notBlank())) {
          throwAssertionError("(%1$s) is %2$sblank", obj, (expected ? StringUtils.EMPTY_STRING : NOT));
        }
      }
    }

    public void isNotEmpty() {
      if (conditionHolds()) {
        if (notEqualToExpected(is(obj).notEmpty())) {
          throwAssertionError("(%1$s) is %2$sempty", obj, (expected ? StringUtils.EMPTY_STRING : NOT));
        }
      }
    }

    public void isNotNull() {
      not().isNull();
    }

    public void isNull() {
      if (conditionHolds()) {
        if (notEqualToExpected(is(obj).Null())) {
          throwAssertionError("(%1$s) is %2$snull", obj, negate(NOT));
        }
      }
    }

    public void isSameAs(final T obj) {
      if (conditionHolds()) {
        if (notEqualToExpected(is(this.obj).sameAs(obj))) {
          throwAssertionError("(%1$s) is %2$sthe same as (%3$s)", this.obj, negate(NOT), obj);
        }
      }
    }

    public void isNotSameAs(final T obj) {
      not().isSameAs(obj);
    }

    public void isTrue() {
      if (conditionHolds()) {
        if (notEqualToExpected(is(obj).True())) {
          throwAssertionError("(%1$s) is %2$strue", obj, negate(NOT));
        }
      }
    }

    public AssertThat<T> throwing(final RuntimeException cause) {
      this.cause = cause;
      return this;
    }

    public AssertThat<T> using(final String message, final Object... args) {
      this.message = format(message, args);
      return this;
    }

    public AssertThat<T> when(final Condition condition) {
      this.condition = (condition != null ? condition : () -> true);
      return this;
    }

    private String format(final String message, final Object... args) {
      return stringFormat(messageFormat(message, args), args);
    }

    private String messageFormat(final String message, final Object... args) {
      return MessageFormat.format(message, args);
    }

    private String stringFormat(final String message, final Object... args) {
      return String.format(message, args);
    }

    private String negate(final String value) {
      return (expected ? value : "");
    }

    private void throwAssertionError(final String defaultMessage, final Object... args) {
      throw (is(cause).notNull() ? cause : new AssertionFailedException(withMessage(defaultMessage, args)));
    }

    private String withMessage(final String defaultMessage, final Object... args) {
      return (is(message).notBlank() ? message : format(defaultMessage, args));
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
     * Determines whether the Class object provided to the is operator is assignable to the Class type parameter.
     * 
     * @param type the Class type used to check for assignment compatibility.
     * @return a boolean value indicating if the Class object provided to the is operator is assignable to
     * the Class type parameter.
     * @see java.lang.Class#isAssignableFrom(Class)
     */
    boolean assignableTo(Class<?> type);

    /**
     * Determines whether the object provided to the is operator is equal to the object parameter.  The objects are
     * considered equal as determined by their compareTo method.  This implies that the objects in the equality
     * comparison must implement the Comparable interface.
     * 
     * @param obj the Object parameter used in the equality comparison.
     * @return a boolean value indicating whether the objects are equal.
     * @see java.lang.Comparable#compareTo(Object)
     */
    boolean comparableTo(T obj);

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
     * Shortcut for not().equalTo(:Object). Determines whether the object provided to the is operator is not equal to
     * the object parameter.  The objects are considered unequal when either is null, both are objects of
     * different types, or both objects are unequal in value as determined by their equals method.
     *
     * @param obj the Object parameter used in the equality comparison.
     * @return a boolean value indicating whether the objects are unequal.
     * @see #not()
     * @see #equalTo(Object)
     */
    boolean notEqualTo(T obj);

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
     * Determines whether the String object provided to the is operator is not blank (or rather, has actual text data).
     *
     * @return a boolean value indicating whether the String has actual text data.
     */
    boolean notBlank();

    /**
     * Determines whether the String object provided to the is operator is not empty.
     *
     * @return a boolean value indicating whether the String is not empty.
     */
    boolean notEmpty();

    /**
     * Shortcut method for the not().Null() operation.  Determines whether the object provided to the is operator
     * is not null.
     * 
     * @return a boolean value indicating whether the object in question is not null.
     * @see #not()
     * @see #Null()
     */
    boolean notNull();

    /**
     * Determines whether the object provided to the is operator is null.
     * 
     * @return a boolean value indicating whether the object in question is null.
     */
    boolean Null();

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
     * Shortcut for not().isSameAs(:Object).  Determines whether the object provided to the is operator is *not*
     * the same as, or does not refer to the same object in memory as the given object parameter.
     *
     * @param obj the Object reference used to determine if the object in question is not a reference to the same object
     * in memory.
     * @return a boolean value indicating whether the object in question and object parameter do not refer to
     * the same object.
     * @see #not()
     * @see #sameAs(Object)
     */
    boolean notSameAs(T obj);

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

    private Class<?> toClass(final Object obj) {
      return (obj instanceof Class ? (Class<?>) obj : obj.getClass());
    }

    @SuppressWarnings("unchecked")
    private Comparable<T> toComparable(final T obj) {
      return (Comparable<T>) obj;
    }

    public boolean assignableTo(final Class<?> type) {
      return equalToExpected(obj != null && type != null && type.isAssignableFrom(toClass(obj)));
    }

    public boolean comparableTo(final T obj) {
      return equalToExpected(toComparable(this.obj).compareTo(obj) == 0);
    }

    public boolean equalTo(final T obj) {
      return equalToExpected(this.obj != null && this.obj.equals(obj));
    }

    public boolean notEqualTo(final T obj) {
      return not().equalTo(obj);
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

    public boolean notBlank() {
      return StringUtils.hasText(ObjectUtils.toString(obj));
    }

    public boolean notEmpty() {
      boolean result = (obj instanceof Object[] && ((Object[]) obj).length != 0);
      result |= (obj instanceof Collection && !((Collection) obj).isEmpty());
      result |= (obj instanceof Map && !((Map) obj).isEmpty());
      result |= (obj instanceof String && !obj.toString().isEmpty());
      return result;
    }

    public boolean notNull() {
      return not().Null();
    }

    public boolean Null() {
      return equalToExpected(this.obj == null);
    }

    public boolean sameAs(final T obj) {
      return equalToExpected(this.obj == obj);
    }

    public boolean notSameAs(final T obj) {
      return not().sameAs(obj);
    }

    public boolean True() {
      return equalToExpected(Boolean.TRUE.equals(this.obj));
    }
  }

}
