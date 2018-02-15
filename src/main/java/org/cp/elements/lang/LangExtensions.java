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

package org.cp.elements.lang;

import static org.cp.elements.lang.LangExtensions.SafeNavigationHandler.newSafeNavigationHandler;
import static org.cp.elements.lang.reflect.MethodInvocation.newMethodInvocation;
import static org.cp.elements.lang.reflect.ProxyFactory.newProxyFactory;

import java.lang.reflect.Method;
import java.text.MessageFormat;
import java.util.Collection;
import java.util.Map;
import java.util.Optional;
import java.util.function.Supplier;

import org.cp.elements.lang.annotation.Experimental;
import org.cp.elements.lang.annotation.FluentApi;
import org.cp.elements.lang.reflect.MethodInterceptor;
import org.cp.elements.lang.reflect.MethodInvocation;
import org.cp.elements.lang.reflect.ProxyFactory;

/**
 * The {@link LangExtensions} class provides methods to write natural language expressions for various conditions,
 * such as equality comparisons, identity checks, null checks, negation and so on, and operations
 * such as conversion, etc.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.Assert
 * @see org.cp.elements.lang.FluentApiExtension
 * @see org.cp.elements.lang.annotation.FluentApi
 * @see org.cp.elements.lang.annotation.Experimental
 * @see org.cp.elements.lang.reflect.ProxyFactory
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class LangExtensions {

  /**
   * Safe-navigation operator used to safely navigate the series of {@link Object} {@link Method} invocations
   * in a call chain, for example...
   *
   * <code>
   *   obj.getX().getY().getZ()...
   * </code>
   *
   * @param <T> {@link Class} type of given {@link Object} to Proxy.
   * @param obj {@link Object} to Proxy and on which the {@link Method} invocation, call chain begins.
   * @param interfaces array of {@link Class interfaces} for the Proxy {@link Class} to implement.
   * @return a Proxy for the given {@link Object} implement the provided {@link Class interfaces}.
   * @see org.cp.elements.lang.reflect.ProxyFactory#newProxyFactory(Object, Class[])
   * @see org.cp.elements.lang.reflect.ProxyFactory#adviseWith(MethodInterceptor[])
   * @see SafeNavigationHandler#newSafeNavigationHandler(ProxyFactory)
   * @see org.cp.elements.lang.reflect.ProxyFactory#newProxy()
   */
  @FluentApi
  @Experimental
  @SuppressWarnings("unchecked")
  public static <T> T $(T obj, Class<?>... interfaces) {

    ProxyFactory<T> proxyFactory = newProxyFactory(obj, interfaces);

    return (T) proxyFactory.adviseWith(newSafeNavigationHandler(proxyFactory)).newProxy();
  }

  protected static class SafeNavigationHandler<T> implements FluentApiExtension,
      org.cp.elements.lang.reflect.MethodInterceptor<T> {

    private static final Object DUMMY = new Object();

    /**
     * Factory method used to construct an instance of {@link SafeNavigationHandler} initialized with
     * the given {@link ProxyFactory} used to evaluate the next {@link Object} in the {@link Method} invocation
     * call chain.
     *
     * @param <T> {@link Class} type of the {@link Object} to proxy.
     * @param proxyFactory {@link ProxyFactory} used to evaluate the next {@link Object} in
     * the {@link Method} invocation call chain.
     * @return an instance of the {@link SafeNavigationHandler}.
     * @see org.cp.elements.lang.reflect.ProxyFactory
     * @see #SafeNavigationHandler(ProxyFactory)
     */
    static <T> SafeNavigationHandler newSafeNavigationHandler(ProxyFactory<T> proxyFactory) {
      return new SafeNavigationHandler<>(proxyFactory);
    }

    private final ProxyFactory<T> proxyFactory;

    /**
     * Constructs an instance of {@link SafeNavigationHandler} initialized with the given {@link ProxyFactory}
     * used to evaluate the next {@link Object} in the {@link Method} invocation call chain.
     *
     * @param proxyFactory {@link ProxyFactory} used to evaluate the next {@link Object} in
     * the {@link Method} invocation call chain.
     * @throws IllegalArgumentException if {@link ProxyFactory} is {@literal null}.
     * @see org.cp.elements.lang.reflect.ProxyFactory
     */
    private SafeNavigationHandler(ProxyFactory<T> proxyFactory) {
      Assert.notNull(proxyFactory, "ProxyFactory must not be null");
      this.proxyFactory = proxyFactory;
    }

    /**
     * Return a reference to the {@link ProxyFactory} used to evaluate the next {@link Object}
     * in the {@link Method} invocation call chain.
     *
     * @return a reference toe the {@link ProxyFactory} used to evaluate the next {@link Object}
     * in the {@link Method} invocation call chain.
     * @see org.cp.elements.lang.reflect.ProxyFactory
     */
    private ProxyFactory<T> getProxyFactory() {
      return this.proxyFactory;
    }

    /**
     * Returns the target {@link Object} of the {@link Method} interception.
     *
     * @return the target {@link Object} of the {@link Method} interception.
     * @see org.cp.elements.lang.reflect.MethodInterceptor#getTarget()
     */
    @Override
    @SuppressWarnings("unchecked")
    public T getTarget() {
      return getProxyFactory().getTarget();
    }

    /**
     * Intercepts the {@link Method} invocation on the target {@link Object} to handle {@literal null-safe} navigation.
     *
     * @param <R> {@link Class} type of the return value.
     * @param methodInvocation {@link MethodInvocation} for the currently invoked {@link Method} in a chain of
     * {@link Object} accessor invocations using dot notation to navigate the {@link Object} graph.
     * E.g. obj.getX().getY().getZ();
     * @return an {@link Optional} to capture the return value of the {@link Object} invocation,
     * which might be {@literal null}.
     * @see org.cp.elements.lang.reflect.MethodInvocation
     * @see java.util.Optional
     */
    @Override
    @SuppressWarnings("unchecked")
    public <R> Optional<R> intercept(MethodInvocation methodInvocation) {
      R nextTarget = resolveNextTarget(methodInvocation);
      Class<?> targetType = resolveTargetType(methodInvocation);

      return (canProxy(nextTarget, targetType) ? Optional.of($(nextTarget, targetType))
        : Optional.ofNullable(nextTarget));
    }

    /**
     * Invokes the given {@link Method} with the array of {@link Object} arguments on the resolved target.
     *
     * @param proxy {@link Object Proxy} on which the {@link Method} was invoked in order to
     * intercept the {@link Method} call.
     * @param method {@link Method} to invoke.
     * @param args array of {@link Object} arguments to pass to the {@link Method} invocation.
     * @return the return value of the {@link Method} invocation, or {@literal null}
     * if the {@link Method} does not return a value.
     * @throws Throwable if the {@link Method} invocation fails.
     * @see org.cp.elements.lang.reflect.MethodInvocation#newMethodInvocation(Object, Method, Object...)
     * @see #intercept(MethodInvocation)
     * @see #resolveTarget(Object)
     * @see java.lang.Object
     * @see java.lang.reflect.Method
     * @see java.util.Optional
     */
    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
      return intercept(newMethodInvocation(resolveTarget(proxy), method, args)).orElse(null);
    }

    /* (non-Javadoc) */
    @SuppressWarnings("unchecked")
    private <R> R resolveNextTarget(MethodInvocation methodInvocation) {
      return (R) Optional.ofNullable(getTarget())
        .map(target -> methodInvocation.makeAccessible().invoke(target).orElse(null))
          .orElse(null);
    }

    /* (non-Javadoc) */
    private Object resolveTarget(Object proxy) {
      return Optional.ofNullable((Object) getTarget()).orElse(proxy);
    }

    /* (non-Javadoc) */
    private Class<?> resolveTargetType(MethodInvocation methodInvocation) {
      return methodInvocation.getMethod().getReturnType();
    }

    /* (non-Javadoc) */
    private boolean canProxy(Object target, Class<?>... types) {
      return getProxyFactory().canProxy(Optional.ofNullable(target).orElse(DUMMY), types);
    }
  }

  /**
   * The {@literal assertThat} operator is used to assert the state of an object, such as it's equality,
   * identity, nullity, relational value, and so on.
   *
   * @param <T> {@link Class} type of the object being asserted.
   * @param obj Object to be asserted.
   * @return a new instance of the {@link AssertThat} {@link FluentApi} expression for making assertions
   * about an @{@link Object object's} state.
   * @see org.cp.elements.lang.LangExtensions.AssertThatExpression
   * @see org.cp.elements.lang.annotation.FluentApi
   */
  @FluentApi
  public static <T> AssertThat<T> assertThat(T obj) {
    return new AssertThatExpression<>(obj);
  }

  /**
   * The {@link AssertThat} interface is a contract for implementing objects that assert the state of an {@link Object}
   * or component of the application or system.
   *
   * @param <T> {@link Class} type of the object to evaluate and assert.
   * @see org.cp.elements.lang.LangExtensions.AssertThatExpression
   * @see org.cp.elements.lang.LangExtensions.AssertThatWrapper
   * @see org.cp.elements.lang.FluentApiExtension
   */
  public interface AssertThat<T> extends FluentApiExtension {

    /**
     * Asserts whether the object to evaluate is assignable to the given Class type.  The object evaluated
     * maybe a Class object, an instance of a Class or null.
     *
     * @param type the Class type with which to determine assignment compatibility.
     * @throws AssertionException if the object being evaluated is not assignable to the Class type.
     * @see java.lang.Class#isAssignableFrom(Class)
     * @see #isInstanceOf(Class)
     */
    void isAssignableTo(Class type);

    /**
     * Asserts whether the {@link Object} to evaluate is {@link Comparable} to the given {@link Object}.
     *
     * This assertion performs an equality comparison as determined by the {@link Comparable} criteria
     * based on the {@link Comparable} {@link Class type} of the {@link Object objects}.
     *
     * @param obj {@link Comparable} object to compare with the {@link Object} being evaluated.
     * @throws AssertionException if the {@link Object} being evaluated is not comparable
     * to the given {@link Comparable} object.
     * @see java.lang.Comparable#compareTo(Object)
     */
    void isComparableTo(Comparable<T> obj);

    /**
     * Asserts whether the {@link Object} to evaluate is {@link Comparable} to the given {@link Object}.
     *
     * This assertion performs an equality comparison as determined by the {@link Comparable} criteria
     * based on the {@link Comparable} {@link Class type} of the {@link Object objects}.
     *
     * @param obj {@link Comparable} object to compare with the {@link Object} being evaluated.
     * @throws AssertionException if the {@link Object} being evaluated is not comparable
     * to the given {@link Comparable} object.
     * @see java.lang.Comparable#compareTo(Object)
     */
    void isNotComparableTo(Comparable<T> obj);

    /**
     * Asserts whether the object to evaluate is equal to the given object.  The objects are deemed equal
     * as determined by the Object.equals method.
     *
     * @param obj the object used in the equality comparison with the object being evaluated.
     * @throws AssertionException if the object being evaluated is not equal to the given object.
     * @see java.lang.Object#equals(Object)
     * @see #isSameAs(Object)
     */
    void isEqualTo(T obj);

    /**
     * Asserts whether the object to evaluate is not equal to the given object.  The objects are deemed unequal
     * as determined by the Object.equals method.
     *
     * @param obj the object used in the equality comparison with the object being evaluated.
     * @throws AssertionException if the object being evaluated is equal to the given object.
     * @see java.lang.Object#equals(Object)
     * @see #isEqualTo(Object)
     * @see #not()
     */
    void isNotEqualTo(T obj);

    /**
     * Asserts whether the object to evaluate is false.
     *
     * @throws AssertionException if the object being evaluated is not false.
     * @see #isTrue()
     */
    void isFalse();

    /**
     * Assert that the object to evaluate is greater than the given Comparable value.
     *
     * @param lowerBound the Comparable value used as the lower bound in the relational comparison.
     * @throws AssertionException if the object being evaluated is less than or equal to the lower bound.
     */
    void isGreaterThan(T lowerBound);

    /**
     * Assert that the object to evaluate is within the range of (greater than and less than)
     * the given Comparable values.
     *
     * @param lowerBound the Comparable value used as the lower bound in the relational comparison.
     * @param upperBound the Comparable value used as the upper bound in the relational comparison.
     * @throws AssertionException if the object being evaluated is less than equal to the lower bound
     * or greater than equal to the upper bound.
     */
    void isGreaterThanAndLessThan(T lowerBound, T upperBound);

    /**
     * Assert that the object to evaluate is within the range of (greater than and less than equal to)
     * the given Comparable values.
     *
     * @param lowerBound the Comparable value used as the lower bound in the relational comparison.
     * @param upperBound the Comparable value used as the upper bound in the relational comparison.
     * @throws AssertionException if the object being evaluated is less than equal to the lower bound
     * or greater than the upper bound.
     */
    void isGreaterThanAndLessThanEqualTo(T lowerBound, T upperBound);

    /**
     * Assert that the object to evaluate is greater than or equal to the given Comparable value.
     *
     * @param lowerBound the Comparable value used as the lower bound in the relational comparison.
     * @throws AssertionException if the object being evaluated is less than the lower bound.
     */
    void isGreaterThanEqualTo(T lowerBound);

    /**
     * Assert that the object to evaluate is within the range of (greater than equal to and less than)
     * the given Comparable values.
     *
     * @param lowerBound the Comparable value used as the lower bound in the relational comparison.
     * @param upperBound the Comparable value used as the upper bound in the relational comparison.
     * @throws AssertionException if the object being evaluated is less than the lower bound
     * or greater than equal to the upper bound.
     */
    void isGreaterThanEqualToAndLessThan(T lowerBound, T upperBound);

    /**
     * Assert that the object to evaluate is within the range of (greater than equal to and less than equal to)
     * the given Comparable values.
     *
     * @param lowerBound the Comparable value used as the lower bound in the relational comparison.
     * @param upperBound the Comparable value used as the upper bound in the relational comparison.
     * @throws AssertionException if the object being evaluated is less than the lower bound
     * or greater than the upper bound.
     */
    void isGreaterThanEqualToAndLessThanEqualTo(T lowerBound, T upperBound);

    /**
     * Asserts that the object to evaluate has actual textual information.  The object's String value has text
     * if and only if the value contains at least 1 character that is not whitespace.
     *
     * @throws AssertionException if the object being evaluated has no text.
     * @see #isNotBlank()
     */
    void hasText();

    /**
     * Assert that the current Thread holds the specified lock inside a synchronized block.
     *
     * @param lock the Object lock that must be held by the current Thread.
     * @throws AssertionException if the current Thread does not hold the specified lock.
     * @see java.lang.Thread#holdsLock(Object)
     */
    void holdsLock(Object lock);

    /**
     * Asserts that the object to evaluate is an instance of the specified Class type.
     *
     * @param type the Class type used in the instance of check for the object being evaluated.
     * @throws AssertionException if the object being evaluated is not an instance of the Class type.
     * @see java.lang.Class#isInstance(Object)
     * @see #isAssignableTo(Class)
     */
    void isInstanceOf(Class type);

    /**
     * Asserts that the object to evaluate is less than the given Comparable value.
     *
     * @param upperBound the Comparable value used as the upper bound in the relational comparison.
     * @throws AssertionException if the object being evaluated is not less than the upper bound.
     */
    void isLessThan(T upperBound);

    /**
     * Asserts that the object to evaluate is outside the bounds of the given Comparable values.
     *
     * @param upperBound the Comparable value used as the lower upper bound in the relational comparison.
     * @param lowerBound the Comparable value used as the upper lower bound in the relational comparison.
     * @throws AssertionException if the object being evaluated is not less than the upper bound
     * or greater than the lower bound.
     */
    void isLessThanOrGreaterThan(T upperBound, T lowerBound);

    /**
     * Asserts that the object to evaluate is outside the bounds of the given Comparable values.
     *
     * @param upperBound the Comparable value used as the lower upper bound in the relational comparison.
     * @param lowerBound the Comparable value used as the upper lower bound in the relational comparison.
     * @throws AssertionException if the object being evaluated is not less than the upper bound
     * or greater than equal to the lower bound.
     */
    void isLessThanOrGreaterThanEqualTo(T upperBound, T lowerBound);

    /**
     * Asserts that the object to evaluate is less than equal to the given Comparable value.
     *
     * @param upperBound the Comparable value used as the upper bound in the relational comparison.
     * @throws AssertionException if the object being evaluated is not less than equal to the upper bound.
     */
    void isLessThanEqualTo(T upperBound);

    /**
     * Asserts that the object to evaluate is outside the bounds of the given Comparable values.
     *
     * @param upperBound the Comparable value used as the lower upper bound in the relational comparison.
     * @param lowerBound the Comparable value used as the upper lower bound in the relational comparison.
     * @throws AssertionException if the object being evaluated is not less than equal to the upper bound
     * or greater than the lower bound.
     */
    void isLessThanEqualToOrGreaterThan(T upperBound, T lowerBound);

    /**
     * Asserts that the object to evaluate is outside the bounds of the given Comparable values.
     *
     * @param upperBound the Comparable value used as the lower upper bound in the relational comparison.
     * @param lowerBound the Comparable value used as the upper lower bound in the relational comparison.
     * @throws AssertionException if the object being evaluated is not less than equal to the upper bound
     * or greater than equal to the lower bound.
     */
    void isLessThanEqualToOrGreaterThanEqualTo(T upperBound, T lowerBound);

    /**
     * Assert that the object to evaluate is not blank, or rather, has text.  The object String value is blank
     * if it contains only whitespace characters or null.
     *
     * @throws AssertionException if the object being evaluated is blank, or has no actual test.
     * @see #hasText()
     */
    void isNotBlank();

    /**
     * Assert that the object to evaluate is not empty.  The object String value is empty if it is equal to
     * the empty String.
     *
     * @throws AssertionException if the object being evaluated is empty.
     */
    void isNotEmpty();

    /**
     * Asserts that the object to evaluate is not null.
     *
     * @throws AssertionException if the object being evaluated is null.
     * @see #isNull()
     * @see #not()
     */
    void isNotNull();

    /**
     * Asserts that the object to evaluate is null.
     *
     * @throws AssertionException if the object being evaluated is not null.
     */
    void isNull();

    /**
     * Asserts that the object to evaluate is the same instance as the given object.  This assertion deems the objects
     * are the same as determined by the identity comparison (==).
     *
     * @param obj the object used in the identity comparison with the object being evaluated.
     * @throws AssertionException if the objects are not the same.
     * @see #isEqualTo(Object)
     */
    void isSameAs(T obj);

    /**
     * Asserts that the object to evaluate is not the same instance as the given object.  This assertion deems
     * the objects are not the same as determined by the identity comparison (==).
     *
     * @param obj the object used in the identity comparison with the object being evaluated.
     * @throws AssertionException if the objects are the same.
     * @see #isSameAs(Object)
     * @see #not()
     */
    void isNotSameAs(T obj);

    /**
     * Asserts that the object to evaluate is true.
     *
     * @throws AssertionException if the object being evaluated is not true.
     * @see #isFalse()
     */
    void isTrue();

    /**
     * Negates this assertion.
     *
     * @return a negated instance of this assertion.
     */
    AssertThat<T> not();

    /**
     * Uses the provided {@link String message} and {@link Object message arguments} in the {@link AssertionException}
     * thrown when an assertion fails.
     *
     * @param message {@link String} containing the message used in the {@link AssertionException}.
     * @param args array of {@link Object arguments} used to populate the placeholders of the {@link String message}.
     * @return this assertion instance.
     */
    AssertThat<T> stating(String message, Object... args);

    /**
     * Uses the supplied {@link Supplier message} in the {@link AssertionException} thrown when an assertion fails.
     *
     * @param message {@link Supplier} containing the message used in the {@link AssertionException}.
     * @return this assertion instance.
     * @see java.util.function.Supplier
     */
    AssertThat<T> stating(Supplier<String> message);

    /**
     * Throws the provided RuntimeException when an assertion fails.
     *
     * @param e the RuntimeException to throw when an assertion fails.
     * @return this assertion instance.
     */
    AssertThat<T> throwing(RuntimeException e);

    /**
     * Transforms this assertion into an adapted assertion of the same type using the provided Transformer.
     *
     * @param assertionTransformer the Transformer used to transform this assertion into another assertion
     * of the same type.
     * @return an instance of AssertThat wrapping this assertion.
     * @see org.cp.elements.lang.Transformer
     */
    AssertThat<T> transform(Transformer<AssertThat<T>> assertionTransformer);

    /**
     * Enables or disables this assertion based on the provided {@link Condition}.
     *
     * @param condition {@link Condition} used to enable or disable this assertion at runtime.
     * @return this assertion instance.
     * @see org.cp.elements.lang.Condition
     */
    AssertThat<T> when(Condition condition);

  }

  /**
   * The AssertThatExpression class is the default implementation of the AssertThat interface
   * implementing all the assertion operations.
   *
   * @param <T> the type of object to evaluation and perform the assertion.
   */
  private static final class AssertThatExpression<T> implements AssertThat<T> {

    private static final boolean DEFAULT_EXPECTED = true;

    private static final String NOT = "not ";

    private final boolean expected;

    private final T obj;

    private Condition condition;

    private RuntimeException cause;

    private Supplier<String> message;

    private Transformer<AssertThat<T>> transformer;

    /* (non-Javadoc) */
    private AssertThatExpression(T obj) {
      this(obj, DEFAULT_EXPECTED);
    }

    /* (non-Javadoc) */
    private AssertThatExpression(T obj, boolean expected) {

      this.obj = obj;
      this.expected = expected;
      this.condition = () -> true;
    }

    /* (non-Javadoc) */
    private boolean conditionHolds() {
      return this.condition.evaluate();
    }

    /* (non-Javadoc) */
    private boolean notEqualToExpected(boolean actual) {
      return !(actual == this.expected);
    }

    /* (non-Javadoc) */
    public void isAssignableTo(Class type) {

      if (conditionHolds()) {
        if (notEqualToExpected(is(this.obj).assignableTo(type))) {
          throwAssertionError("[%1$s] is %2$sassignable to [%3$s]",
            this.obj, negate(NOT), ObjectUtils.getName(type));
        }
      }
    }

    /* (non-Javadoc) */
    @SuppressWarnings("unchecked")
    public void isComparableTo(Comparable<T> comparable) {

      if (conditionHolds()) {
        if (notEqualToExpected(is((Comparable<T>) this.obj).comparableTo(comparable))) {
          throwAssertionError("[%1$s] is %2$scomparable to [%3$s]", this.obj, negate(NOT), comparable);
        }
      }
    }

    /* (non-Javadoc) */
    public void isNotComparableTo(Comparable<T> comparable) {
      not().isComparableTo(comparable);
    }

    /* (non-Javadoc) */
    public void isEqualTo(T obj) {

      if (conditionHolds()) {
        if (notEqualToExpected(is(this.obj).equalTo(obj))) {
          throwAssertionError("[%1$s] is %2$sequal to [%3$s]", this.obj, negate(NOT), obj);
        }
      }
    }

    /* (non-Javadoc) */
    public void isNotEqualTo(T obj) {
      not().isEqualTo(obj);
    }

    /* (non-Javadoc) */
    public void isFalse() {

      if (conditionHolds()) {
        if (notEqualToExpected(is(this.obj).False())) {
          throwAssertionError("[%1$s] is %2$sfalse", this.obj, negate(NOT));
        }
      }
    }

    /* (non-Javadoc) */
    public void isGreaterThan(T lowerBound) {

      if (conditionHolds()) {
        if (notEqualToExpected(is(this.obj).greaterThan(lowerBound))) {
          throwAssertionError("[%1$s] is %2$sgreater than [%3$s]", this.obj, negate(NOT), lowerBound);
        }
      }
    }

    /* (non-Javadoc) */
    public void isGreaterThanAndLessThan(T lowerBound, T upperBound) {

      if (conditionHolds()) {
        if (notEqualToExpected(is(this.obj).greaterThanAndLessThan(lowerBound, upperBound))) {
          throwAssertionError("[%1$s] is %2$sgreater than [%3$s] and less than [%4$s]",
            this.obj, negate(NOT), lowerBound, upperBound);
        }
      }
    }

    /* (non-Javadoc) */
    public void isGreaterThanAndLessThanEqualTo(T lowerBound, T upperBound) {

      if (conditionHolds()) {
        if (notEqualToExpected(is(this.obj).greaterThanAndLessThanEqualTo(lowerBound, upperBound))) {
          throwAssertionError("[%1$s] is %2$sgreater than [%3$s] and less than equal to [%4$s]",
            this.obj, negate(NOT), lowerBound, upperBound);
        }
      }
    }

    /* (non-Javadoc) */
    public void isGreaterThanEqualTo(T lowerBound) {

      if (conditionHolds()) {
        if (notEqualToExpected(is(this.obj).greaterThanEqualTo(lowerBound))) {
          throwAssertionError("[%1$s] is %2$sgreater than equal to [%3$s]",
            this.obj, negate(NOT), lowerBound);
        }
      }
    }

    /* (non-Javadoc) */
    public void isGreaterThanEqualToAndLessThan(T lowerBound, T upperBound) {

      if (conditionHolds()) {
        if (notEqualToExpected(is(this.obj).greaterThanEqualToAndLessThan(lowerBound, upperBound))) {
          throwAssertionError("[%1$s] is %2$sgreater than equal to [%3$s] and less than [%4$s]",
            this.obj, negate(NOT), lowerBound, upperBound);
        }
      }
    }

    /* (non-Javadoc) */
    public void isGreaterThanEqualToAndLessThanEqualTo(T lowerBound, T upperBound) {

      if (conditionHolds()) {
        if (notEqualToExpected(is(this.obj).greaterThanEqualToAndLessThanEqualTo(lowerBound, upperBound))) {
          throwAssertionError("[%1$s] is %2$sgreater than equal to [%3$s] and less than equal to [%4$s]",
            this.obj, negate(NOT), lowerBound, upperBound);
        }
      }
    }

    /* (non-Javadoc) */
    public void hasText() {
      isNotBlank();
    }

    /* (non-Javadoc) */
    public void holdsLock(Object lock) {
      if (conditionHolds()) {
        if (notEqualToExpected(Thread.holdsLock(lock))) {
          throwAssertionError("[%1$s] %2$slock [%3$s]", Thread.currentThread(),
            this.expected ? "does not hold " : "holds ", lock);
        }
      }
    }

    /* (non-Javadoc) */
    public void isInstanceOf(Class type) {

      if (conditionHolds()) {
        if (notEqualToExpected(is(this.obj).instanceOf(type))) {
          throwAssertionError("[%1$s] is %2$san instance of [%3$s]",
            this.obj, negate(NOT), ObjectUtils.getName(type));
        }
      }
    }

    /* (non-Javadoc) */
    public void isLessThan(T upperBound) {

      if (conditionHolds()) {
        if (notEqualToExpected(is(this.obj).lessThan(upperBound))) {
          throwAssertionError("[%1$s] is %2$sless than [%3$s]", this.obj, negate(NOT), upperBound);
        }
      }
    }

    /* (non-Javadoc) */
    public void isLessThanOrGreaterThan(T upperBound, T lowerBound) {

      if (conditionHolds()) {
        if (notEqualToExpected(is(this.obj).lessThanOrGreaterThan(upperBound, lowerBound))) {
          throwAssertionError("[%1$s] is %2$sless than [%3$s] or greater than [%4$s]",
            this.obj, negate(NOT), upperBound, lowerBound);
        }
      }
    }

    /* (non-Javadoc) */
    public void isLessThanOrGreaterThanEqualTo(T upperBound, T lowerBound) {

      if (conditionHolds()) {
        if (notEqualToExpected(is(this.obj).lessThanOrGreaterThanEqualTo(upperBound, lowerBound))) {
          throwAssertionError("[%1$s] is %2$sless than [%3$s] or greater than equal to [%4$s]",
            this.obj, negate(NOT), upperBound, lowerBound);
        }
      }
    }

    /* (non-Javadoc) */
    public void isLessThanEqualTo(T upperBound) {

      if (conditionHolds()) {
        if (notEqualToExpected(is(this.obj).lessThanEqualTo(upperBound))) {
          throwAssertionError("[%1$s] is %2$sless than equal to [%3$s]",
            this.obj, negate(NOT), upperBound);
        }
      }
    }

    /* (non-Javadoc) */
    public void isLessThanEqualToOrGreaterThan(T upperBound, T lowerBound) {

      if (conditionHolds()) {
        if (notEqualToExpected(is(this.obj).lessThanEqualToOrGreaterThan(upperBound, lowerBound))) {
          throwAssertionError("[%1$s] is %2$sless than equal to [%3$s] or greater than [%4$s]",
            this.obj, negate(NOT), upperBound, lowerBound);
        }
      }
    }

    /* (non-Javadoc) */
    public void isLessThanEqualToOrGreaterThanEqualTo(T upperBound, T lowerBound) {

      if (conditionHolds()) {
        if (notEqualToExpected(is(this.obj).lessThanEqualToOrGreaterThanEqualTo(upperBound, lowerBound))) {
          throwAssertionError("[%1$s] is %2$sless than equal to [%3$s] or greater than equal to [%4$s]",
            this.obj, negate(NOT), upperBound, lowerBound);
        }
      }
    }

    /* (non-Javadoc) */
    public void isNotBlank() {

      if (conditionHolds()) {
        if (notEqualToExpected(is(this.obj).notBlank())) {
          throwAssertionError("[%1$s] is %2$sblank", this.obj,
            this.expected ? StringUtils.EMPTY_STRING : NOT);
        }
      }
    }

    /* (non-Javadoc) */
    public void isNotEmpty() {

      if (conditionHolds()) {
        if (notEqualToExpected(is(this.obj).notEmpty())) {
          throwAssertionError("[%1$s] is %2$sempty", this.obj,
            this.expected ? StringUtils.EMPTY_STRING : NOT);
        }
      }
    }

    /* (non-Javadoc) */
    public void isNotNull() {
      not().isNull();
    }

    /* (non-Javadoc) */
    public void isNull() {

      if (conditionHolds()) {
        if (notEqualToExpected(is(this.obj).Null())) {
          throwAssertionError("[%1$s] is %2$snull", this.obj, negate(NOT));
        }
      }
    }

    /* (non-Javadoc) */
    public void isSameAs(T obj) {

      if (conditionHolds()) {
        if (notEqualToExpected(is(this.obj).sameAs(obj))) {
          throwAssertionError("[%1$s] is %2$sthe same as [%3$s]", this.obj, negate(NOT), obj);
        }
      }
    }

    /* (non-Javadoc) */
    public void isNotSameAs(T obj) {
      not().isSameAs(obj);
    }

    /* (non-Javadoc) */
    public void isTrue() {

      if (conditionHolds()) {
        if (notEqualToExpected(is(this.obj).True())) {
          throwAssertionError("[%1$s] is %2$strue", this.obj, negate(NOT));
        }
      }
    }

    /* (non-Javadoc) */
    public AssertThat<T> not() {

      AssertThat<T> expression = new AssertThatExpression<>(this.obj, !this.expected);

      expression = this.transformer != null ? this.transformer.transform(expression) : expression;
      expression = expression.throwing(this.cause);
      expression = this.message != null ? expression.stating(this.message) : expression;
      expression = expression.when(this.condition);

      return expression;
    }

    /* (non-Javadoc) */
    public AssertThat<T> stating(String message, Object... args) {
      return stating(() -> format(message, args));
    }

    /* (non-Javadoc) */
    public AssertThat<T> stating(Supplier<String> message) {
      this.message = message;
      return this;
    }

    /* (non-Javadoc) */
    public AssertThat<T> throwing(RuntimeException cause) {
      this.cause = cause;
      return this;
    }

    /* (non-Javadoc) */
    @Override
    public AssertThat<T> transform(Transformer<AssertThat<T>> assertionTransformer) {
      this.transformer = assertionTransformer;
      return assertionTransformer.transform(this);
    }

    /* (non-Javadoc) */
    public AssertThat<T> when(Condition condition) {
      this.condition = condition != null ? condition : () -> true;
      return this;
    }

    /* (non-Javadoc) */
    private String format(String message, Object... args) {
      return stringFormat(messageFormat(message, args), args);
    }

    /* (non-Javadoc) */
    private String messageFormat(String message, Object... args) {
      return MessageFormat.format(message, args);
    }

    /* (non-Javadoc) */
    private String stringFormat(String message, Object... args) {
      return String.format(message, args);
    }

    /* (non-Javadoc) */
    private String negate(String value) {
      return this.expected ? value : "";
    }

    /* (non-Javadoc) */
    @SuppressWarnings("all")
    private void throwAssertionError(String defaultMessage, Object... args) {
      throw ObjectUtils.returnValueOrDefaultIfNull(this.cause,
        () -> new AssertionException(withMessage(defaultMessage, args)));
    }

    /* (non-Javadoc) */
    private String withMessage(String defaultMessage, Object... args) {

      String suppliedMessage = Optional.ofNullable(this.message).map(Supplier::get).orElse(null);

      return is(suppliedMessage).notBlank() ? suppliedMessage : format(defaultMessage, args);
    }
  }

  /**
   * The AssertThatWrapper class is a Decorator used to decorate or modify the existing behavior and/or functionality
   * of an existing assertion (AssertThat instance).  This class makes it easier to extend and customize any existing
   * assertion in the transform(..) operation.
   *
   * @param <T> the type of object to evaluation and perform the assertion.
   */
  public static class AssertThatWrapper<T> implements AssertThat<T> {

    private final AssertThat<T> delegate;

    /* (non-Javadoc) */
    public AssertThatWrapper(AssertThat<T> delegate) {
      Assert.notNull(delegate, "Delegate must not be null");
      this.delegate = delegate;
    }

    /* (non-Javadoc) */
    public static <T> AssertThat<T> wrap(AssertThat<T> delegate) {
      return new AssertThatWrapper<>(delegate);
    }

    /* (non-Javadoc) */
    protected AssertThat<T> getDelegate() {
      return this.delegate;
    }

    /* (non-Javadoc) */
    @Override
    public void isAssignableTo(Class type) {
      getDelegate().isAssignableTo(type);
    }

    /* (non-Javadoc) */
    @Override
    public void isComparableTo(Comparable<T> obj) {
      getDelegate().isComparableTo(obj);
    }

    /* (non-Javadoc) */
    @Override
    public void isNotComparableTo(Comparable<T> obj) {
      getDelegate().isNotComparableTo(obj);
    }

    /* (non-Javadoc) */
    @Override
    public void isEqualTo(T obj) {
      getDelegate().isEqualTo(obj);
    }

    /* (non-Javadoc) */
    @Override
    public void isNotEqualTo(T obj) {
      getDelegate().isNotEqualTo(obj);
    }

    /* (non-Javadoc) */
    @Override
    public void isFalse() {
      getDelegate().isFalse();
    }

    /* (non-Javadoc) */
    @Override
    public void isGreaterThan(T lowerBound) {
      getDelegate().isGreaterThan(lowerBound);
    }

    /* (non-Javadoc) */
    @Override
    public void isGreaterThanAndLessThan(T lowerBound, T upperBound) {
      getDelegate().isGreaterThanAndLessThan(lowerBound, upperBound);
    }

    /* (non-Javadoc) */
    @Override
    public void isGreaterThanAndLessThanEqualTo(T lowerBound, T upperBound) {
      getDelegate().isGreaterThanAndLessThanEqualTo(lowerBound, upperBound);
    }

    /* (non-Javadoc) */
    @Override
    public void isGreaterThanEqualTo(T lowerBound) {
      getDelegate().isGreaterThanEqualTo(lowerBound);
    }

    /* (non-Javadoc) */
    @Override
    public void isGreaterThanEqualToAndLessThan(T lowerBound, T upperBound) {
      getDelegate().isGreaterThanEqualToAndLessThan(lowerBound, upperBound);
    }

    /* (non-Javadoc) */
    @Override
    public void isGreaterThanEqualToAndLessThanEqualTo(T lowerBound, T upperBound) {
      getDelegate().isGreaterThanEqualToAndLessThanEqualTo(lowerBound, upperBound);
    }

    /* (non-Javadoc) */
    @Override
    public void hasText() {
      getDelegate().hasText();
    }

    /* (non-Javadoc) */
    @Override
    public void holdsLock(Object lock) {
      getDelegate().holdsLock(lock);
    }

    /* (non-Javadoc) */
    @Override
    public void isInstanceOf(Class type) {
      getDelegate().isInstanceOf(type);
    }

    /* (non-Javadoc) */
    @Override
    public void isLessThan(T upperBound) {
      getDelegate().isLessThan(upperBound);
    }

    /* (non-Javadoc) */
    @Override
    public void isLessThanOrGreaterThan(T upperBound, T lowerBound) {
      getDelegate().isLessThanOrGreaterThan(upperBound, lowerBound);
    }

    /* (non-Javadoc) */
    @Override
    public void isLessThanOrGreaterThanEqualTo(T upperBound, T lowerBound) {
      getDelegate().isLessThanOrGreaterThanEqualTo(upperBound, lowerBound);
    }

    /* (non-Javadoc) */
    @Override
    public void isLessThanEqualTo(T upperBound) {
      getDelegate().isLessThanEqualTo(upperBound);
    }

    /* (non-Javadoc) */
    @Override
    public void isLessThanEqualToOrGreaterThan(T upperBound, T lowerBound) {
      getDelegate().isLessThanEqualToOrGreaterThan(upperBound, lowerBound);
    }

    /* (non-Javadoc) */
    @Override
    public void isLessThanEqualToOrGreaterThanEqualTo(T upperBound, T lowerBound) {
      getDelegate().isLessThanEqualToOrGreaterThanEqualTo(upperBound, lowerBound);
    }

    /* (non-Javadoc) */
    @Override
    public void isNotBlank() {
      getDelegate().isNotBlank();
    }

    /* (non-Javadoc) */
    @Override
    public void isNotEmpty() {
      getDelegate().isNotEmpty();
    }

    /* (non-Javadoc) */
    @Override
    public void isNotNull() {
      getDelegate().isNotNull();
    }

    /* (non-Javadoc) */
    @Override
    public void isNull() {
      getDelegate().isNull();
    }

    /* (non-Javadoc) */
    @Override
    public void isSameAs(T obj) {
      getDelegate().isSameAs(obj);
    }

    /* (non-Javadoc) */
    @Override
    public void isNotSameAs(T obj) {
      getDelegate().isNotSameAs(obj);
    }

    /* (non-Javadoc) */
    @Override
    public void isTrue() {
      getDelegate().isTrue();
    }

    /* (non-Javadoc) */
    @Override
    public AssertThat<T> not() {
      return new AssertThatWrapper<>(getDelegate().not());
    }

    /* (non-Javadoc) */
    @Override
    public AssertThat<T> throwing(RuntimeException e) {
      getDelegate().throwing(e);
      return this;
    }

    /* (non-Javadoc) */
    @Override
    public AssertThat<T> transform(Transformer<AssertThat<T>> assertionTransformer) {
      return new AssertThatWrapper<>(assertionTransformer.transform(getDelegate()));
    }

    /* (non-Javadoc) */
    @Override
    public AssertThat<T> stating(String message, Object... args) {
      getDelegate().stating(message, args);
      return this;
    }

    /* (non-Javadoc) */
    @Override
    public AssertThat<T> stating(Supplier<String> message) {
      getDelegate().stating(message);
      return null;
    }

    /* (non-Javadoc) */
    @Override
    public AssertThat<T> when(Condition condition) {
      getDelegate().when(condition);
      return this;
    }
  }

  /**
   * The {@literal is} operator can be used to make logical determinations about an object such as boolean, equality,
   * identity, relational or type comparisons with other objects, and so on.
   *
   * @param <T> {@link Class} type of object as the subject of the {@literal is} operator.
   * @param obj {@link Object} that is the subject of the operation.
   * @return an instance of the {@literal is} operator.
   * @see org.cp.elements.lang.LangExtensions.IsExpression
   * @see org.cp.elements.lang.annotation.FluentApi
   */
  @FluentApi
  public static <T> Is<T> is(T obj) {
    return new IsExpression<>(obj);
  }

  /**
   * The {@link Is} interface defines operations to classify a single object based on it's identity, state, type
   * or relationship to another {@link Object}.
   *
   * @param <T> {@link Class} type of objet as the subject of the {@literal is} operator.
   * @see org.cp.elements.lang.FluentApiExtension
   */
  public interface Is<T> extends FluentApiExtension {

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

    /**
     * Negates the expected outcome/result of this operator.
     *
     * @return the instance of this Is operator negated.
     */
    Is<T> not();

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

    /* (non-Javadoc) */
    private IsExpression(T obj) {
      this(obj, DEFAULT_EXPECTED);
    }

    /* (non-Javadoc) */
    private IsExpression(T obj, boolean expected) {
      this.obj = obj;
      this.expected = expected;
    }

    /* (non-Javadoc) */
    private boolean equalToExpected(boolean actualOutcome) {
      return (actualOutcome == expected);
    }

    /* (non-Javadoc) */
    private LogicalOperator getOp(LogicalOperator op) {
      return (expected ? op : op.getOpposite());
    }

    /* (non-Javadoc) */
    private Class<?> toClass(Object obj) {
      return (obj instanceof Class ? (Class<?>) obj : obj.getClass());
    }

    /* (non-Javadoc) */
    @SuppressWarnings("unchecked")
    private Comparable<T> toComparable(T obj) {
      return (Comparable<T>) obj;
    }

    /* (non-Javadoc) */
    public boolean assignableTo(Class<?> type) {
      return equalToExpected(obj != null && type != null && type.isAssignableFrom(toClass(obj)));
    }

    /* (non-Javadoc) */
    public boolean comparableTo(T obj) {
      return equalToExpected(toComparable(this.obj).compareTo(obj) == 0);
    }

    /* (non-Javadoc) */
    public boolean equalTo(T obj) {
      return equalToExpected(this.obj != null && this.obj.equals(obj));
    }

    /* (non-Javadoc) */
    public boolean notEqualTo(T obj) {
      return not().equalTo(obj);
    }

    /* (non-Javadoc) */
    public boolean False() {
      return equalToExpected(Boolean.FALSE.equals(this.obj));
    }

    /* (non-Javadoc) */
    public boolean greaterThan(T lowerBound) {
      return equalToExpected(toComparable(this.obj).compareTo(lowerBound) > 0);
    }

    /* (non-Javadoc) */
    public boolean greaterThanAndLessThan(T lowerBound, T upperBound) {
      return getOp(LogicalOperator.AND).evaluate(greaterThan(lowerBound), lessThan(upperBound));
    }

    /* (non-Javadoc) */
    public boolean greaterThanAndLessThanEqualTo(T lowerBound, T upperBound) {
      return getOp(LogicalOperator.AND).evaluate(greaterThan(lowerBound), lessThanEqualTo(upperBound));
    }

    /* (non-Javadoc) */
    public boolean greaterThanEqualTo(T lowerBound) {
      return equalToExpected(toComparable(this.obj).compareTo(lowerBound) >= 0);
    }

    /* (non-Javadoc) */
    public boolean greaterThanEqualToAndLessThan(T lowerBound, T upperBound) {
      return getOp(LogicalOperator.AND).evaluate(greaterThanEqualTo(lowerBound), lessThan(upperBound));
    }

    /* (non-Javadoc) */
    public boolean greaterThanEqualToAndLessThanEqualTo(T lowerBound, T upperBound) {
      return getOp(LogicalOperator.AND).evaluate(greaterThanEqualTo(lowerBound), lessThanEqualTo(upperBound));
    }

    /* (non-Javadoc) */
    public boolean instanceOf(Class type) {
      return equalToExpected(type != null && type.isInstance(this.obj));
    }

    /* (non-Javadoc) */
    public boolean lessThan(T upperBound) {
      return equalToExpected(toComparable(this.obj).compareTo(upperBound) < 0);
    }

    /* (non-Javadoc) */
    public boolean lessThanOrGreaterThan(T upperBound, T lowerBound) {
      return getOp(LogicalOperator.OR).evaluate(lessThan(upperBound), greaterThan(lowerBound));
    }

    /* (non-Javadoc) */
    public boolean lessThanOrGreaterThanEqualTo(T upperBound, T lowerBound) {
      return getOp(LogicalOperator.OR).evaluate(lessThan(upperBound), greaterThanEqualTo(lowerBound));
    }

    /* (non-Javadoc) */
    public boolean lessThanEqualTo(T upperBound) {
      return equalToExpected(toComparable(this.obj).compareTo(upperBound) <= 0);
    }

    /* (non-Javadoc) */
    public boolean lessThanEqualToOrGreaterThan(T upperBound, T lowerBound) {
      return getOp(LogicalOperator.OR).evaluate(lessThanEqualTo(upperBound), greaterThan(lowerBound));
    }

    /* (non-Javadoc) */
    public boolean lessThanEqualToOrGreaterThanEqualTo(T upperBound, T lowerBound) {
      return getOp(LogicalOperator.OR).evaluate(lessThanEqualTo(upperBound), greaterThanEqualTo(lowerBound));
    }

    /* (non-Javadoc) */
    public boolean notBlank() {
      return StringUtils.hasText(ObjectUtils.toString(this.obj));
    }

    /* (non-Javadoc) */
    public boolean notEmpty() {

      boolean result = (this.obj instanceof Object[] && ((Object[]) this.obj).length != 0);

      result |= (this.obj instanceof Collection && !((Collection) this.obj).isEmpty());
      result |= (this.obj instanceof Map && !((Map) this.obj).isEmpty());
      result |= (this.obj instanceof String && !this.obj.toString().isEmpty());

      return result;
    }

    /* (non-Javadoc) */
    public boolean notNull() {
      return not().Null();
    }

    /* (non-Javadoc) */
    public boolean Null() {
      return equalToExpected(this.obj == null);
    }

    /* (non-Javadoc) */
    public boolean notSameAs(T obj) {
      return not().sameAs(obj);
    }

    /* (non-Javadoc) */
    public boolean sameAs(T obj) {
      return equalToExpected(this.obj == obj);
    }

    /* (non-Javadoc) */
    public boolean True() {
      return equalToExpected(Boolean.TRUE.equals(this.obj));
    }

    /* (non-Javadoc) */
    public Is<T> not() {
      return new IsExpression<>(this.obj, !expected);
    }
  }
}
