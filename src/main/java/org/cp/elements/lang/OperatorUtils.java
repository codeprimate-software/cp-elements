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

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * The OperatorUtils class provides methods to write natural language expressions for various conditions, such as 
 * equality comparisons, identity checks, null checks, negation and so on, and operations such as conversion, etc.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.lang.LogicalOperator
 * @see java.util.Arrays
 * @see java.util.Collections
 * @see java.util.List
 * @see java.util.Set
 * @since 1.0.0
 */
public abstract class OperatorUtils {

  /**
   * The from operator performs conversions on the given array and it's elements.  For instance, the array can be
   * converted into an ordered List of elements, or a unique Set of elements originating from the array.
   * <p/>
   * @param <T> the type of the elements in the array.
   * @param array the array of elements all of type T to be converted.
   * @return a From operator to perform the conversions.
   */
  public static <T> From<T> from(final T... array) {
    return new FromImpl<T>(array);
  }

  /**
   * The From interface defines operations for conversion of an Object array to a List, Set or String.
   * <p/>
   * @param <T> the element type of objects in the List or Set.
   */
  public static interface From<T> {

    /**
     * Converts an object array to a List.
     * <p/>
     * @return a List implementation containing all the elements in the given object array to the from operator.
     * @see java.util.List
     */
    public List<T> toList();

    /**
     * Converts an object array to a Set.
     * <p/>
     * @return a Set implementation containing all the elements of the given object array to the from operator.
     * @see java.util.Set
     */
    public Set<T> toSet();
  }

  /**
   * The FromImpl class is an implementation of the From interface, from operator.
   * <p/>
   * @param <T> the element type of items in the List or Set.
   */
  private static final class FromImpl<T> implements From<T> {

    private final T[] array;

    public FromImpl(final T... array) {
      this.array = array;
    }

    @SuppressWarnings("unchecked")
    public List<T> toList() {
      return (array == null ? Collections.<T>emptyList() : (array.length == 1 ? Collections.singletonList(array[0])
        : Arrays.asList(array)));
    }

    public Set<T> toSet() {
      return (array == null ? Collections.<T>emptySet() : (array.length == 1 ? Collections.singleton(array[0])
        : new HashSet<T>(toList())));
    }

    @Override
    public String toString() {
      final StringBuilder buffer = new StringBuilder("[");

      if (array != null) {
        for (int index = 0; index < array.length; index++) {
          buffer.append(index > 0 ? StringUtils.COMMA_SPACE_DELIMITER : StringUtils.EMPTY_STRING);
          buffer.append(array[index]);
        }
      }

      return buffer.append("]").toString();
    }
  }

  /**
   * The is operator can be used to make logical determinations about an object such as boolean, equality, identity,
   * relational or type comparisons with other objects, and so on.
   * <p/>
   * @param <T> the type of Object as the subject of the is operator.
   * @param obj the Object that is the subject of the operation.
   * @return an instance of the is operator.
   */
  public static <T> Is<T> is(final T obj) {
    return new IsImpl<T>(obj);
  }

  /**
   * The Is interface defines operations to classify a single object based on it's identity, state, type or relationship
   * to another object.
   * <p/>
   * @param <T> the type of Object as the subject of the is operator.
   */
  public static interface Is<T> {

    /**
     * Determines whether the Class object provided to the is operator is assignable from the Class type parameter.
     * <p/>
     * @param type the Class type to check for assignment compatibility.
     * @return a boolean value indicating if the Class type parameter is assignable to the Class object provided in
     * this is operator.
     * @see java.lang.Class#isAssignableFrom(Class)
     */
    public boolean assignableFrom(Class type);

    /**
     * Determines whether the object provided to the is operator is equal to the object parameter.  The objects are
     * considered equal as determined by their compareTo method.  This implies that the objects in the equality
     * comparison must implement the Comparable interface.
     * <p/>
     * @param obj the Object parameter used in the equality comparison.
     * @return a boolean value indicating whether the objects are equal.
     * @see java.lang.Comparable#compareTo(Object)
     */
    public boolean equalByComparison(T obj);

    /**
     * Determines whether the object provided to the is operator is equal to the object parameter.  The objects are
     * considered equal when neither is null, both refer to the same object, or both objects have the same value as
     * determined by their equals method.
     * <p/>
     * @param obj the Object parameter used in the equality comparison.
     * @return a boolean value indicating whether the objects are equal.
     * @see java.lang.Object#equals(Object)
     */
    public boolean equalTo(T obj);

    /**
     * Determines whether the object provided to the is operator actually evaluates to the value false.  An object
     * is false if and only if the value is actually false and not null or some other value (such as true).
     * <p/>
     * @return a boolean value of true if the object in question is indeed the value false.
     * @see java.lang.Boolean#FALSE
     */
    public boolean False();

    /**
     * Determines whether the object provided to the is operator is greater than the specified value, as determined
     * by the Comparable object's compareTo method.
     * <p/>
     * @param lowerBound the lower bound value for which the object must be greater than.
     * @return a boolean value indicating if the object is greater than the specified value.
     * @see java.lang.Comparable#compareTo(Object)
     */
    public boolean greaterThan(T lowerBound);

    /**
     * Determines whether the object provided to the is operator is greater than some specified lower bound value
     * and also less than some specified upper bound value, as determined by the Comparable object's compareTo method.
     * <p/>
     * @param lowerBound the lower bound value for which the object must be greater than.
     * @param upperBound the upper bound value for which the object must be less than.
     * @return a boolean value indicating if the object is greater than the lower bound value and less than the
     * upper bound value.
     * @see java.lang.Comparable#compareTo(Object)
     */
    public boolean greaterThanAndLessThan(T lowerBound, T upperBound);

    /**
     * Determines whether the object provided to the is operator is greater than some specified lower bound value
     * and also less than or equal to some specified upper bound value, as determined by the Comparable object's
     * compareTo method.
     * <p/>
     * @param lowerBound the lower bound value for which the object must be greater than.
     * @param upperBound the upper bound value for which the object must be less than or equal to.
     * @return a boolean value indicating if the object is greater than the lower bound value and less than or equal to
     * the upper bound value.
     * @see java.lang.Comparable#compareTo(Object)
     */
    public boolean greaterThanAndLessThanEqualTo(T lowerBound, T upperBound);

    /**
     * Determines whether the object provided to the is operator is greater than or equal to the specified value,
     * as determined by the Comparable object's compareTo method.
     * <p/>
     * @param lowerBound the lower bound value for which the object must be greater than or equal to.
     * @return a boolean value indicating if the object is greater than or equal to the specified value.
     * @see java.lang.Comparable#compareTo(Object)
     */
    public boolean greaterThanEqualTo(T lowerBound);

    /**
     * Determines whether the object provided to the is operator is greater than or equal to some specified
     * lower bound value and also less than some specified upper bound value, as determined by the Comparable object's
     * compareTo method.
     * <p/>
     * @param lowerBound the lower bound value for which the object must be greater than or equal to.
     * @param upperBound the upper bound value for which the object must be less than.
     * @return a boolean value indicating if the object is greater than or equal to the lower bound value and less than
     * the upper bound value.
     * @see java.lang.Comparable#compareTo(Object)
     */
    public boolean greaterThanEqualToAndLessThan(T lowerBound, T upperBound);

    /**
     * Determines whether the object provided to the is operator is greater than or equal to some specified
     * lower bound value and also less than or equal to some specified upper bound value, as determined by
     * the Comparable object's compareTo method.
     * <p/>
     * @param lowerBound the lower bound value for which the object must be greater than or equal to.
     * @param upperBound the upper bound value for which the object must be less than or equal to.
     * @return a boolean value indicating if the object is greater than or equal to the lower bound value
     * and less than or equal to the upper bound value.
     * @see java.lang.Comparable#compareTo(Object)
     */
    public boolean greaterThanEqualToAndLessThanEqualTo(T lowerBound, T upperBound);

    /**
     * Determines whether the object provided to the is operator is an instance of the specified class type.
     * <p/>
     * @param type the Class object used in determining if the object in question is an instance of the
     * specified Class.
     * @return a boolean value indicating whether the object in question is an instance of the specified Class.
     */
    public boolean instanceOf(Class type);

    /**
     * Determines whether the object provided to the is operator is less than the specified value, as determined by
     * the Comparable object's compareTo method.
     * <p/>
     * @param upperBound the upper bound value for which the object must be less than.
     * @return a boolean value indicating if the object is less than the specified value.
     * @see java.lang.Comparable#compareTo(Object)
     */
    public boolean lessThan(T upperBound);

    /**
     * Determines whether the object provided to the is operator is less than some specified lower upper bound value
     * or greater than some specified upper lower bound value, as determined by the Comparable object's
     * compareTo method.
     * <p/>
     * @param upperBound the upper bound value for which the object must be less than.
     * @param lowerBound the lower bound value for which the object must be greater than.
     * @return a boolean value indicating if the object is less than the upper bound value
     * or is greater than the lower bound value.
     * @see java.lang.Comparable#compareTo(Object)
     */
    public boolean lessThanOrGreaterThan(T upperBound, T lowerBound);

    /**
     * Determines whether the object provided to the is operator is less than some specified lower upper bound value
     * or greater than or equal to some specified upper lower bound value, as determined by the Comparable object's
     * compareTo method.
     * <p/>
     * @param upperBound the upper bound value for which the object must be less than.
     * @param lowerBound the lower bound value for which the object must be greater than or equal to.
     * @return a boolean value indicating if the object is less than the upper bound value or is greater than
     * or equal to the lower bound value.
     * @see java.lang.Comparable#compareTo(Object)
     */
    public boolean lessThanOrGreaterThanEqualTo(T upperBound, T lowerBound);

    /**
     * Determines whether the object provided to the is operator is less than or equal to the specified value,
     * as determined by the Comparable object's compareTo method.
     * <p/>
     * @param upperBound the upper bound value for which the object must be less than or equal to.
     * @return a boolean value indicating if the object is less than or equal to the specified value.
     * @see java.lang.Comparable#compareTo(Object)
     */
    public boolean lessThanEqualTo(T upperBound);

    /**
     * Determines whether the object provided to the is operator is less than or equal to some specified
     * lower upper bound value or greater than some specified upper lower bound value, as determined by
     * the Comparable object's compareTo method.
     * <p/>
     * @param upperBound the upper bound value for which the object must be less than or equal to.
     * @param lowerBound the lower bound value for which the object must be greater than.
     * @return a boolean value indicating if the object is less than or equal to the upper bound value
     * or is greater than the lower bound value.
     * @see java.lang.Comparable#compareTo(Object)
     */
    public boolean lessThanEqualToOrGreaterThan(T upperBound, T lowerBound);

    /**
     * Determines whether the object provided to the is operator is less than or equal to some specified
     * lower upper bound value or greater than or equal to some specified upper lower bound value, as determined by
     * the Comparable object's compareTo method.
     * <p/>
     * @param upperBound the upper bound value for which the object must be less than or equal to.
     * @param lowerBound the lower bound value for which the object must be greater than or equal to.
     * @return a boolean value indicating if the object is less than or equal to the upper bound value
     * or is greater than or equal to the lower bound value.
     * @see java.lang.Comparable#compareTo(Object)
     */
    public boolean lessThanEqualToOrGreaterThanEqualTo(T upperBound, T lowerBound);

    /**
     * Negates the expected outcome/result of this operator.
     * <p/>
     * @return the instance of this Is operator negated.
     */
    public Is<T> not();

    /**
     * Shortcut method of the not().Null() operation.  Determines whether the object provided to the is operator
     * is not null.
     * <p/>
     * @return a boolean value indicating whether the object in question is not null.
     */
    public boolean notNull();

    /**
     * Determines whether the object provided to the is operator is null.
     * <p/>
     * @return a boolean value indicating whether the object in question is null.
     */
    public boolean Null();

    /**
     * Determines whether the object provided to the is operator is the same as, or refers to the same object
     * in memory as the given object parameter.
     * <p/>
     * @param obj the Object reference used to determine if the object in question is a reference to the same object
     * in memory.
     * @return a boolean value indicating whether the object in question and object parameter reference refer to
     * the same object.
     */
    public boolean sameAs(T obj);

    /**
     * Determines whether the object provided to the is operator actually evaluates to the value true.  An object
     * is true if and only if the value is actually true and not null or some other value (such as false).
     * <p/>
     * @return a boolean value of true if the object in question is indeed the value true
     * @see java.lang.Boolean#TRUE
     */
    public boolean True();
  }

  /**
   * The IsImpl class is an implementation of the Is interface, is operator.  Note, this implementation is Thread-safe,
   * although it is very unlikely that a Thread will share an instance of this class since every invocation of the
   * is() operator factory method will return a new instance of this class, at least for the time being.
   * <p/>
   * @param <T> the Object's type.
   */
  private static final class IsImpl<T> implements Is<T> {

    private final boolean expectedOutcome;

    private final T obj;

    private IsImpl(final T obj) {
      this(obj, true);
    }

    private IsImpl(final T obj, final boolean expectedOutcome) {
      this.obj = obj;
      this.expectedOutcome = expectedOutcome;
    }

    private LogicalOperator getOp(final LogicalOperator op) {
      return (expectedOutcome ? op : op.getOpposite());
    }

    private boolean getOutcome(final boolean actualOutcome) {
      return (actualOutcome == expectedOutcome);
    }

    @SuppressWarnings("unchecked")
    private Comparable<T> toComparable(T obj) {
      return (Comparable<T>) obj;
    }

    public boolean assignableFrom(final Class type) {
      return getOutcome(this.obj != null && type != null && ((Class<?>) this.obj).isAssignableFrom(type));
    }

    public boolean equalByComparison(final T obj) {
      return getOutcome(toComparable(this.obj).compareTo(obj) == 0);
    }

    public boolean equalTo(final T obj) {
      return getOutcome(this.obj != null && this.obj.equals(obj));
    }

    public boolean False() {
      return getOutcome(Boolean.FALSE.equals(this.obj));
    }

    public boolean greaterThan(final T lowerBound) {
      return getOutcome(toComparable(this.obj).compareTo(lowerBound) > 0);
    }

    public boolean greaterThanAndLessThan(final T lowerBound, final T upperBound) {
      return getOp(LogicalOperator.AND).evaluate(greaterThan(lowerBound), lessThan(upperBound));
    }

    public boolean greaterThanAndLessThanEqualTo(final T lowerBound, final T upperBound) {
      return getOp(LogicalOperator.AND).evaluate(greaterThan(lowerBound), lessThanEqualTo(upperBound));
    }

    public boolean greaterThanEqualTo(final T lowerBound) {
      return getOutcome(toComparable(this.obj).compareTo(lowerBound) >= 0);
    }

    public boolean greaterThanEqualToAndLessThan(final T lowerBound, final T upperBound) {
      return getOp(LogicalOperator.AND).evaluate(greaterThanEqualTo(lowerBound), lessThan(upperBound));
    }

    public boolean greaterThanEqualToAndLessThanEqualTo(final T lowerBound, final T upperBound) {
      return getOp(LogicalOperator.AND).evaluate(greaterThanEqualTo(lowerBound), lessThanEqualTo(upperBound));
    }

    public boolean instanceOf(final Class type) {
      return getOutcome(type != null && type.isInstance(this.obj));
    }

    public boolean lessThan(final T upperBound) {
      return getOutcome(toComparable(this.obj).compareTo(upperBound) < 0);
    }

    public boolean lessThanOrGreaterThan(final T upperBound, final T lowerBound) {
      return getOp(LogicalOperator.OR).evaluate(lessThan(upperBound), greaterThan(lowerBound));
    }

    public boolean lessThanOrGreaterThanEqualTo(final T upperBound, final T lowerBound) {
      return getOp(LogicalOperator.OR).evaluate(lessThan(upperBound), greaterThanEqualTo(lowerBound));
    }

    public boolean lessThanEqualTo(final T upperBound) {
      return getOutcome(toComparable(this.obj).compareTo(upperBound) <= 0);
    }

    public boolean lessThanEqualToOrGreaterThan(final T upperBound, final T lowerBound) {
      return getOp(LogicalOperator.OR).evaluate(lessThanEqualTo(upperBound), greaterThan(lowerBound));
    }

    public boolean lessThanEqualToOrGreaterThanEqualTo(final T upperBound, final T lowerBound) {
      return getOp(LogicalOperator.OR).evaluate(lessThanEqualTo(upperBound), greaterThanEqualTo(lowerBound));
    }

    public Is<T> not() {
      return new IsImpl<T>(this.obj, !expectedOutcome);
    }

    public boolean notNull() {
      return not().Null();
    }

    public boolean Null() {
      return getOutcome(this.obj == null);
    }

    public boolean sameAs(final T obj) {
      return getOutcome(this.obj == obj);
    }

    public boolean True() {
      return getOutcome(Boolean.TRUE.equals(this.obj));
    }
  }

}
