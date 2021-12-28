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

import static org.cp.elements.lang.LangExtensions.SafeNavigationHandler.newSafeNavigationHandler;
import static org.cp.elements.lang.reflect.MethodInvocation.newMethodInvocation;
import static org.cp.elements.lang.reflect.ProxyFactory.newProxyFactory;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.text.MessageFormat;
import java.util.Collection;
import java.util.Map;
import java.util.Optional;
import java.util.function.Predicate;
import java.util.function.Supplier;

import org.cp.elements.lang.annotation.Dsl;
import org.cp.elements.lang.annotation.Experimental;
import org.cp.elements.lang.annotation.FluentApi;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.lang.reflect.MethodInterceptor;
import org.cp.elements.lang.reflect.MethodInvocation;
import org.cp.elements.lang.reflect.ProxyFactory;

/**
 * The {@link LangExtensions} class provides methods to write natural language expressions for various conditions,
 * such as equality comparisons, identity checks, null checks, negation along with operations such as conversion,
 * etc.
 *
 * @author John J. Blum
 * @see java.lang.reflect.InvocationHandler
 * @see java.lang.reflect.Method
 * @see java.util.function.Predicate
 * @see org.cp.elements.lang.Assert
 * @see org.cp.elements.lang.DslExtension
 * @see org.cp.elements.lang.FluentApiExtension
 * @see org.cp.elements.lang.annotation.Dsl
 * @see org.cp.elements.lang.annotation.FluentApi
 * @see org.cp.elements.lang.annotation.Experimental
 * @see org.cp.elements.lang.reflect.MethodInterceptor
 * @see org.cp.elements.lang.reflect.MethodInvocation
 * @see org.cp.elements.lang.reflect.ProxyFactory
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class LangExtensions {

  /**
   * Safe-navigation operator used to safely navigate the sequence of {@link Object} {@link Method} invocations
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
   * @see org.cp.elements.lang.annotation.Experimental
   * @see org.cp.elements.lang.annotation.FluentApi
   * @see org.cp.elements.lang.annotation.Dsl
   */
  @Dsl
  @FluentApi
  @Experimental
  @SuppressWarnings("unchecked")
  public static <T> T $(T obj, Class<?>... interfaces) {

    ProxyFactory<T> proxyFactory = newProxyFactory(obj, interfaces);

    return proxyFactory
      .adviseWith(newSafeNavigationHandler(proxyFactory))
      .newProxy();
  }

  /**
   * The {@link SafeNavigationHandler} class is a Java {@link InvocationHandler} and Elements {@link MethodInterceptor}
   * used to handle safe object navigation through method chaining.
   *
   * @param <T> {@link Class type} of the {@link Object} to navigate safely.
   * @see org.cp.elements.lang.reflect.MethodInterceptor
   * @see org.cp.elements.lang.FluentApiExtension
   * @see org.cp.elements.lang.DslExtension
   * @see java.lang.reflect.InvocationHandler
   */
  protected static class SafeNavigationHandler<T> implements DslExtension, FluentApiExtension,
      org.cp.elements.lang.reflect.MethodInterceptor<T> {

    private static final Object DUMMY = new Object();

    /**
     * Factory method used to construct a new instance of {@link SafeNavigationHandler} initialized with
     * the given {@link ProxyFactory} used to evaluate the next {@link Object} in the {@link Method} invocation
     * call chain.
     *
     * @param <T> {@link Class type} of the {@link Object} to proxy.
     * @param proxyFactory {@link ProxyFactory} used to evaluate the next {@link Object}
     * in the {@link Method} invocation call chain.
     * @return a new {@link SafeNavigationHandler}.
     * @throws IllegalArgumentException if {@link ProxyFactory} is {@literal null}.
     * @see org.cp.elements.lang.reflect.ProxyFactory
     * @see SafeNavigationHandler(ProxyFactory)
     */
    protected static @NotNull <T> SafeNavigationHandler<T> newSafeNavigationHandler(
        @NotNull ProxyFactory<T> proxyFactory) {

      return new SafeNavigationHandler<>(proxyFactory);
    }

    private final ProxyFactory<T> proxyFactory;

    /**
     * Constructs a new instance of {@link SafeNavigationHandler} initialized with the given {@link ProxyFactory}
     * used to evaluate the next {@link Object} in the {@link Method} invocation call chain.
     *
     * @param proxyFactory {@link ProxyFactory} used to evaluate the next {@link Object}
     * in the {@link Method} invocation call chain.
     * @throws IllegalArgumentException if {@link ProxyFactory} is {@literal null}.
     * @see org.cp.elements.lang.reflect.ProxyFactory
     */
    private SafeNavigationHandler(@NotNull ProxyFactory<T> proxyFactory) {

      Assert.notNull(proxyFactory, "ProxyFactory is required");

      this.proxyFactory = proxyFactory;
    }

    /**
     * Return a reference to the configured {@link ProxyFactory} used to evaluate the next {@link Object}
     * in the {@link Method} invocation call chain.
     *
     * @return a reference to the configured {@link ProxyFactory} used to evaluate the next {@link Object}
     * in the {@link Method} invocation call chain.
     * @see org.cp.elements.lang.reflect.ProxyFactory
     */
    private @NotNull ProxyFactory<T> getProxyFactory() {
      return this.proxyFactory;
    }

    /**
     * Returns the target {@link Object} of the {@link Method} interception.
     *
     * @return the target {@link Object} of the {@link Method} interception.
     * @see org.cp.elements.lang.reflect.MethodInterceptor#getTarget()
     * @see org.cp.elements.lang.reflect.ProxyFactory#getTarget()
     */
    @Override
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
    public <R> Optional<R> intercept(@NotNull MethodInvocation methodInvocation) {

      R nextTarget = resolveNextTarget(methodInvocation);

      Class<?> targetType = resolveTargetType(methodInvocation);

      return canProxy(nextTarget, targetType)
        ? Optional.of($(nextTarget, targetType))
        : Optional.ofNullable(nextTarget);
    }

    /**
     * Invokes the given {@link Method} with the array of {@link Object arguments} on the resolved {@link Object target}.
     *
     * @param proxy {@link Object Proxy} on which the {@link Method} was invoked in order to
     * intercept the {@link Method} call.
     * @param method {@link Method} to invoke.
     * @param arguments array of {@link Object} arguments to pass to the {@link Method} invocation.
     * @return the return value of the {@link Method} invocation, or {@literal null}
     * if the {@link Method} does not return a value.
     * @see org.cp.elements.lang.reflect.MethodInvocation#newMethodInvocation(Object, Method, Object...)
     * @see #intercept(MethodInvocation)
     * @see #resolveTarget(Object)
     * @see java.lang.Object
     * @see java.lang.reflect.Method
     * @see java.util.Optional
     */
    @Override
    public @Nullable Object invoke(@NotNull Object proxy, @NotNull Method method, Object[] arguments) {

      return intercept(newMethodInvocation(resolveTarget(proxy), method, arguments))
        .orElse(null);
    }

    @SuppressWarnings("unchecked")
    private @Nullable <R> R resolveNextTarget(@NotNull MethodInvocation methodInvocation) {

      return (R) Optional.ofNullable(getTarget())
        .flatMap(target -> methodInvocation.makeAccessible().invoke(target))
        .orElse(null);
    }

    private @Nullable Object resolveTarget(@Nullable Object proxy) {

      Object target = getTarget();

      return target != null ? target : proxy;
    }

    private @NotNull Class<?> resolveTargetType(@NotNull MethodInvocation methodInvocation) {
      return methodInvocation.getMethod().getReturnType();
    }

    private boolean canProxy(@Nullable Object target, Class<?>... types) {

      Object resolvedTarget = target != null ? target : DUMMY;

      return getProxyFactory().canProxy(resolvedTarget, types);
    }
  }

  /**
   * The {@literal assertThat} operator is used to assert the state of an object, such as its equality,
   * identity, nullity, relational value, and so on.
   *
   * @param <T> {@link Class type} of the {@link Object} being asserted.
   * @param obj {@link Object} to assert.
   * @return a new instance of the {@link AssertThat} {@link Dsl} {@link FluentApi} expression for making assertions
   * about an @{@link Object Object's} state.
   * @see org.cp.elements.lang.LangExtensions.AssertThatExpression
   * @see org.cp.elements.lang.annotation.FluentApi
   * @see org.cp.elements.lang.annotation.Dsl
   */
  @Dsl
  @FluentApi
  public static <T> AssertThat<T> assertThat(T obj) {
    return new AssertThatExpression<>(obj);
  }

  /**
   * The {@link AssertThat} interface is a contract for implementing objects that assert the state of an {@link Object}
   * or component of the application or system.
   *
   * @param <T> {@link Class type} of the {@link Object} to evaluate and assert.
   * @see org.cp.elements.lang.LangExtensions.AssertThatExpression
   * @see org.cp.elements.lang.LangExtensions.AssertThatWrapper
   * @see org.cp.elements.lang.FluentApiExtension
   * @see org.cp.elements.lang.DslExtension
   */
  public interface AssertThat<T> extends DslExtension, FluentApiExtension {

    /**
     * Asserts whether the object to evaluate is assignable to the given Class type.  The object evaluated
     * maybe a Class object, an instance of a Class or null.
     *
     * @param type the Class type with which to determine assignment compatibility.
     * @return this assertion.
     * @throws AssertionException if the object being evaluated is not assignable to the Class type.
     * @see java.lang.Class#isAssignableFrom(Class)
     * @see #isInstanceOf(Class)
     */
    @SuppressWarnings("rawtypes")
    AssertThat<T> isAssignableTo(Class type);

    /**
     * Asserts whether the {@link Object} to evaluate is {@link Comparable} to the given {@link Object}.
     *
     * This assertion performs an equality comparison as determined by the {@link Comparable} criteria
     * based on the {@link Comparable} {@link Class type} of the {@link Object objects}.
     *
     * @param obj {@link Comparable} object to compare with the {@link Object} being evaluated.
     * @return this assertion.
     * @throws AssertionException if the {@link Object} being evaluated is not comparable
     * to the given {@link Comparable} object.
     * @see java.lang.Comparable#compareTo(Object)
     */
    AssertThat<T> isComparableTo(Comparable<T> obj);

    /**
     * Asserts whether the {@link Object} to evaluate is {@link Comparable} to the given {@link Object}.
     *
     * This assertion performs an equality comparison as determined by the {@link Comparable} criteria
     * based on the {@link Comparable} {@link Class type} of the {@link Object objects}.
     *
     * @param obj {@link Comparable} object to compare with the {@link Object} being evaluated.
     * @return this assertion.
     * @throws AssertionException if the {@link Object} being evaluated is not comparable
     * to the given {@link Comparable} object.
     * @see java.lang.Comparable#compareTo(Object)
     */
    AssertThat<T> isNotComparableTo(Comparable<T> obj);

    /**
     * Asserts whether the object to evaluate is equal to the given object.  The objects are deemed equal
     * as determined by the Object.equals method.
     *
     * @param obj the object used in the equality comparison with the object being evaluated.
     * @return this assertion.
     * @throws AssertionException if the object being evaluated is not equal to the given object.
     * @see java.lang.Object#equals(Object)
     * @see #isSameAs(Object)
     */
    AssertThat<T> isEqualTo(T obj);

    /**
     * Asserts whether the object to evaluate is not equal to the given object.  The objects are deemed unequal
     * as determined by the Object.equals method.
     *
     * @param obj the object used in the equality comparison with the object being evaluated.
     * @return this assertion.
     * @throws AssertionException if the object being evaluated is equal to the given object.
     * @see java.lang.Object#equals(Object)
     * @see #isEqualTo(Object)
     * @see #not()
     */
    AssertThat<T> isNotEqualTo(T obj);

    /**
     * Asserts whether the object to evaluate is false.
     *
     * @return this assertion.
     * @throws AssertionException if the object being evaluated is not false.
     * @see #isTrue()
     */
    AssertThat<T> isFalse();

    /**
     * Assert that the object to evaluate is greater than the given Comparable value.
     *
     * @param lowerBound the Comparable value used as the lower bound in the relational comparison.
     * @return this assertion.
     * @throws AssertionException if the object being evaluated is less than or equal to the lower bound.
     */
    AssertThat<T> isGreaterThan(T lowerBound);

    /**
     * Assert that the object to evaluate is within the range of (greater than and less than)
     * the given Comparable values.
     *
     * @param lowerBound the Comparable value used as the lower bound in the relational comparison.
     * @param upperBound the Comparable value used as the upper bound in the relational comparison.
     * @return this assertion.
     * @throws AssertionException if the object being evaluated is less than equal to the lower bound
     * or greater than equal to the upper bound.
     */
    AssertThat<T> isGreaterThanAndLessThan(T lowerBound, T upperBound);

    /**
     * Assert that the object to evaluate is within the range of (greater than and less than equal to)
     * the given Comparable values.
     *
     * @param lowerBound the Comparable value used as the lower bound in the relational comparison.
     * @param upperBound the Comparable value used as the upper bound in the relational comparison.
     * @return this assertion.
     * @throws AssertionException if the object being evaluated is less than equal to the lower bound
     * or greater than the upper bound.
     */
    AssertThat<T> isGreaterThanAndLessThanEqualTo(T lowerBound, T upperBound);

    /**
     * Assert that the object to evaluate is greater than or equal to the given Comparable value.
     *
     * @param lowerBound the Comparable value used as the lower bound in the relational comparison.
     * @return this assertion.
     * @throws AssertionException if the object being evaluated is less than the lower bound.
     */
    AssertThat<T> isGreaterThanEqualTo(T lowerBound);

    /**
     * Assert that the object to evaluate is within the range of (greater than equal to and less than)
     * the given Comparable values.
     *
     * @param lowerBound the Comparable value used as the lower bound in the relational comparison.
     * @param upperBound the Comparable value used as the upper bound in the relational comparison.
     * @return this assertion.
     * @throws AssertionException if the object being evaluated is less than the lower bound
     * or greater than equal to the upper bound.
     */
    AssertThat<T> isGreaterThanEqualToAndLessThan(T lowerBound, T upperBound);

    /**
     * Assert that the object to evaluate is within the range of (greater than equal to and less than equal to)
     * the given Comparable values.
     *
     * @param lowerBound the Comparable value used as the lower bound in the relational comparison.
     * @param upperBound the Comparable value used as the upper bound in the relational comparison.
     * @return this assertion.
     * @throws AssertionException if the object being evaluated is less than the lower bound
     * or greater than the upper bound.
     */
    AssertThat<T> isGreaterThanEqualToAndLessThanEqualTo(T lowerBound, T upperBound);

    /**
     * Asserts that the object to evaluate has actual textual information.  The object's String value has text
     * if and only if the value contains at least 1 character that is not whitespace.
     *
     * @return this assertion.
     * @throws AssertionException if the object being evaluated has no text.
     * @see #isNotBlank()
     */
    AssertThat<T> hasText();

    /**
     * Assert that the current Thread holds the specified lock inside a synchronized block.
     *
     * @param lock the Object lock that must be held by the current Thread.
     * @return this assertion.
     * @throws AssertionException if the current Thread does not hold the specified lock.
     * @see java.lang.Thread#holdsLock(Object)
     */
    AssertThat<T> holdsLock(Object lock);

    /**
     * Asserts that the object to evaluate is an instance of the specified Class type.
     *
     * @param type the Class type used in the instance of check for the object being evaluated.
     * @return this assertion.
     * @throws AssertionException if the object being evaluated is not an instance of the Class type.
     * @see java.lang.Class#isInstance(Object)
     * @see #isAssignableTo(Class)
     */
    @SuppressWarnings("rawtypes")
    AssertThat<T> isInstanceOf(Class type);

    /**
     * Asserts that the object to evaluate is less than the given Comparable value.
     *
     * @param upperBound the Comparable value used as the upper bound in the relational comparison.
     * @return this assertion.
     * @throws AssertionException if the object being evaluated is not less than the upper bound.
     */
    AssertThat<T> isLessThan(T upperBound);

    /**
     * Asserts that the object to evaluate is outside the bounds of the given Comparable values.
     *
     * @param upperBound the Comparable value used as the lower upper bound in the relational comparison.
     * @param lowerBound the Comparable value used as the upper lower bound in the relational comparison.
     * @return this assertion.
     * @throws AssertionException if the object being evaluated is not less than the upper bound
     * or greater than the lower bound.
     */
    AssertThat<T> isLessThanOrGreaterThan(T upperBound, T lowerBound);

    /**
     * Asserts that the object to evaluate is outside the bounds of the given Comparable values.
     *
     * @param upperBound the Comparable value used as the lower upper bound in the relational comparison.
     * @param lowerBound the Comparable value used as the upper lower bound in the relational comparison.
     * @return this assertion.
     * @throws AssertionException if the object being evaluated is not less than the upper bound
     * or greater than equal to the lower bound.
     */
    AssertThat<T> isLessThanOrGreaterThanEqualTo(T upperBound, T lowerBound);

    /**
     * Asserts that the object to evaluate is less than equal to the given Comparable value.
     *
     * @param upperBound the Comparable value used as the upper bound in the relational comparison.
     * @return this assertion.
     * @throws AssertionException if the object being evaluated is not less than equal to the upper bound.
     */
    AssertThat<T> isLessThanEqualTo(T upperBound);

    /**
     * Asserts that the object to evaluate is outside the bounds of the given Comparable values.
     *
     * @param upperBound the Comparable value used as the lower upper bound in the relational comparison.
     * @param lowerBound the Comparable value used as the upper lower bound in the relational comparison.
     * @return this assertion.
     * @throws AssertionException if the object being evaluated is not less than equal to the upper bound
     * or greater than the lower bound.
     */
    AssertThat<T> isLessThanEqualToOrGreaterThan(T upperBound, T lowerBound);

    /**
     * Asserts that the object to evaluate is outside the bounds of the given Comparable values.
     *
     * @param upperBound the Comparable value used as the lower upper bound in the relational comparison.
     * @param lowerBound the Comparable value used as the upper lower bound in the relational comparison.
     * @return this assertion.
     * @throws AssertionException if the object being evaluated is not less than equal to the upper bound
     * or greater than equal to the lower bound.
     */
    AssertThat<T> isLessThanEqualToOrGreaterThanEqualTo(T upperBound, T lowerBound);

    /**
     * Assert that the object to evaluate is not blank, or rather, has text.  The object String value is blank
     * if it contains only whitespace characters or null.
     *
     * @return this assertion.
     * @throws AssertionException if the object being evaluated is blank, or has no actual test.
     * @see #hasText()
     */
    AssertThat<T> isNotBlank();

    /**
     * Assert that the object to evaluate is not empty.  The object String value is empty if it is equal to
     * the empty String.
     *
     * @return this assertion.
     * @throws AssertionException if the object being evaluated is empty.
     */
    AssertThat<T> isNotEmpty();

    /**
     * Asserts that the object to evaluate is not null.
     *
     * @return this assertion.
     * @throws AssertionException if the object being evaluated is null.
     * @see #isNull()
     * @see #not()
     */
    AssertThat<T> isNotNull();

    /**
     * Asserts that the object to evaluate is not the same instance as the given object.  This assertion deems
     * the objects are not the same as determined by the identity comparison (==).
     *
     * @param obj the object used in the identity comparison with the object being evaluated.
     * @return this assertion.
     * @throws AssertionException if the objects are the same.
     * @see #isSameAs(Object)
     * @see #not()
     */
    AssertThat<T> isNotSameAs(T obj);

    /**
     * Asserts that the object to evaluate is null.
     *
     * @return this assertion.
     * @throws AssertionException if the object being evaluated is not null.
     */
    AssertThat<T> isNull();

    /**
     * Asserts that the object to evaluate is the same instance as the given object.  This assertion deems the objects
     * are the same as determined by the identity comparison (==).
     *
     * @param obj the object used in the identity comparison with the object being evaluated.
     * @return this assertion.
     * @throws AssertionException if the objects are not the same.
     * @see #isEqualTo(Object)
     */
    AssertThat<T> isSameAs(T obj);

    /**
     * Asserts that the object to evaluate is true.
     *
     * @return this assertion.
     * @throws AssertionException if the object being evaluated is not true.
     * @see #isFalse()
     */
    AssertThat<T> isTrue();

    /**
     * Asserts that the {@link Object} to evaluate is valid according to the {@link Predicate}.
     *
     * @param predicate {@link Predicate} used to evaluate and validate the {@link Object}.
     * @return this assertion.
     * @throws AssertionException if the {@link Object} being evaluated/validated is not valid.
     * @see java.util.function.Predicate
     */
    AssertThat<T> isValid(Predicate<T> predicate);

    /**
     * Negates this assertion.
     *
     * @return a negated instance of this assertion.
     */
    AssertThat<T> not();

    /**
     * Uses the provided {@link String message} and {@link Object message arguments} to describe the assertion.
     *
     * @param message {@link String} containing the message describing this assertion.
     * @param args array of {@link Object} arguments used to resolve the placeholders in the {@link String message}.
     * @return this assertion instance.
     * @see #stating(String, Object...)
     */
    default AssertThat<T> describedAs(String message, Object... args) {
      return stating(message, args);
    }

    /**
     * Uses the provided {@link Supplier message} to describe the assertion.
     *
     * @param message {@link Supplier} containing the message describing this assertion.
     * @return this assertion instance.
     * @see #stating(Supplier)
     */
    default AssertThat<T> describedAs(Supplier<String> message) {
      return stating(message);
    }

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
     * @param cause the RuntimeException to throw when an assertion fails.
     * @return this assertion instance.
     */
    AssertThat<T> throwing(RuntimeException cause);

    /**
     * Transforms this assertion into an adapted assertion of the same type using the provided Transformer.
     *
     * @param assertionTransformer the Transformer used to transform this assertion into another assertion
     * of the same type.
     * @return an instance of {@link AssertThat} wrapping this assertion.
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
   * The {@link AssertThatExpression} class is the default implementation of the {@link AssertThat} interface
   * implementing all the assertion operations.
   *
   * @param <T> {@link Class type} of the {@link Object} to evaluate and make an assertion.
   */
  private static final class AssertThatExpression<T> implements AssertThat<T> {

    private static final boolean DEFAULT_EXPECTED = true;

    private static final Condition DEFAULT_CONDITION = Condition.TRUE_CONDITION;

    private static final String EMPTY_STRING = "";
    private static final String NOT = "not ";

    private final boolean expected;

    private final T obj;

    @Nullable
    private Condition condition;

    @Nullable
    private RuntimeException cause;

    @Nullable
    private Supplier<String> message;

    @Nullable
    private Transformer<AssertThat<T>> transformer;

    /**
     * Constructs a new instance of {@link AssertThatExpression} initialized with the given {@link Object}
     * used as the subject of this assertion.
     *
     * @param obj {@link Object} to evaluate and make an assertion.
     * @see #AssertThatExpression(Object, boolean)
     * @see java.lang.Object
     */
    private AssertThatExpression(@Nullable T obj) {
      this(obj, DEFAULT_EXPECTED);
    }

    /**
     * Constructs a new instance of {@link AssertThatExpression} initialized with the given {@link Object}
     * used as the subject of this assertion.
     *
     * @param obj {@link Object} to evaluate and make an assertion.
     * @param expected boolean value specifying the expected outcome of this assertion.
     * @see java.lang.Object
     */
    private AssertThatExpression(@Nullable T obj, boolean expected) {

      this.obj = obj;
      this.expected = expected;
      this.condition = DEFAULT_CONDITION;
    }

    /**
     * Return the expected outcome of this assertion when applied to the {@link Object subject}.
     *
     * Defaults to {@literal true}, implying that the assertion statement made about the {@link Object subject}
     * should result in a {@literal true} value.
     *
     * @return a boolean value indicating the expected outcome of the assertion
     * when applied to the {@link Object subject}.
     */
    private boolean getExpected() {
      return this.expected;
    }

    /**
     * Return the negation of the expected outcome for this assertion when applied to the {@link Object subject}.
     *
     * @return the negated value of the expected outcome for this assertion when applied to the {@link Object subject}.
     * @see #getExpected()
     */
    private boolean getNotExpected() {
      return !getExpected();
    }

    /**
     * Gets a reference to the target {@link Object} that is the {@literal subject} of this assertion.
     *
     * @return a reference to the target {@link Object} that is the {@literal subject} of this assertion.
     * @see java.lang.Object
     */
    private @Nullable T getTarget() {
      return this.obj;
    }

    /**
     * Gets the {@link #getTarget()} {@link Object} as an {@link Object} of {@link Class type}.
     *
     * @param <S> {@link Class type} to cast the {@link #getTarget()} {@link Object} to.
     * @param type {@link Class type} to cast the {@link #getTarget()} {@link Object} to.
     * @return the {@link #getTarget()} {@link Object} cast to {@link Class type}.
     * @see java.lang.Class
     * @see java.lang.Object
     * @see #getTarget()
     */
    private @Nullable <S> S getTargetAs(@NotNull Class<S> type) {
      return type.cast(getTarget());
    }

    private boolean conditionHolds() {
      return nullSafeCondition(this.condition).evaluate();
    }

    private boolean notEqualToExpected(boolean actual) {
      return actual != getExpected();
    }

    @NullSafe
    private @NotNull Condition nullSafeCondition(@Nullable Condition condition) {
      return condition != null ? condition : DEFAULT_CONDITION;
    }

    /**
     * @inheritDoc
     */
    @SuppressWarnings("rawtypes")
    public @NotNull AssertThatExpression<T> isAssignableTo(Class type) {

      if (conditionHolds()) {
        if (notEqualToExpected(is(getTarget()).assignableTo(type))) {
          throwAssertionException("[%1$s] is %2$sassignable to [%3$s]",
            getTarget(), negate(NOT), ObjectUtils.getName(type));
        }
      }

      return this;
    }

    /**
     * @inheritDoc
     */
    public @NotNull AssertThatExpression<T> isComparableTo(Comparable<T> comparable) {

      if (conditionHolds()) {
        if (notEqualToExpected(is(getTargetAs(Comparable.class)).comparableTo(comparable))) {
          throwAssertionException("[%1$s] is %2$scomparable to [%3$s]", getTarget(), negate(NOT), comparable);
        }
      }

      return this;
    }

    /**
     * @inheritDoc
     */
    public @NotNull AssertThatExpression<T> isNotComparableTo(Comparable<T> comparable) {
      not().isComparableTo(comparable);
      return this;
    }

    /**
     * @inheritDoc
     */
    public @NotNull AssertThatExpression<T> isEqualTo(T obj) {

      if (conditionHolds()) {
        if (notEqualToExpected(is(getTarget()).equalTo(obj))) {
          throwAssertionException("[%1$s] is %2$sequal to [%3$s]", getTarget(), negate(NOT), obj);
        }
      }

      return this;
    }

    /**
     * @inheritDoc
     */
    public @NotNull AssertThatExpression<T> isNotEqualTo(T obj) {
      not().isEqualTo(obj);
      return this;
    }

    /**
     * @inheritDoc
     */
    public @NotNull AssertThatExpression<T> isFalse() {

      if (conditionHolds()) {
        if (notEqualToExpected(is(getTarget()).False())) {
          throwAssertionException("[%1$s] is %2$sfalse", getTarget(), negate(NOT));
        }
      }

      return this;
    }

    /**
     * @inheritDoc
     */
    public @NotNull AssertThatExpression<T> isGreaterThan(T lowerBound) {

      if (conditionHolds()) {
        if (notEqualToExpected(is(getTarget()).greaterThan(lowerBound))) {
          throwAssertionException("[%1$s] is %2$sgreater than [%3$s]", getTarget(), negate(NOT), lowerBound);
        }
      }

      return this;
    }

    /**
     * @inheritDoc
     */
    public @NotNull AssertThatExpression<T> isGreaterThanAndLessThan(T lowerBound, T upperBound) {

      if (conditionHolds()) {
        if (notEqualToExpected(is(getTarget()).greaterThanAndLessThan(lowerBound, upperBound))) {
          throwAssertionException("[%1$s] is %2$sgreater than [%3$s] and less than [%4$s]",
            getTarget(), negate(NOT), lowerBound, upperBound);
        }
      }

      return this;
    }

    /**
     * @inheritDoc
     */
    public @NotNull AssertThatExpression<T> isGreaterThanAndLessThanEqualTo(T lowerBound, T upperBound) {

      if (conditionHolds()) {
        if (notEqualToExpected(is(getTarget()).greaterThanAndLessThanEqualTo(lowerBound, upperBound))) {
          throwAssertionException("[%1$s] is %2$sgreater than [%3$s] and less than equal to [%4$s]",
            getTarget(), negate(NOT), lowerBound, upperBound);
        }
      }

      return this;
    }

    /**
     * @inheritDoc
     */
    public @NotNull AssertThatExpression<T> isGreaterThanEqualTo(T lowerBound) {

      if (conditionHolds()) {
        if (notEqualToExpected(is(getTarget()).greaterThanEqualTo(lowerBound))) {
          throwAssertionException("[%1$s] is %2$sgreater than equal to [%3$s]",
            getTarget(), negate(NOT), lowerBound);
        }
      }

      return this;
    }

    /**
     * @inheritDoc
     */
    public @NotNull AssertThatExpression<T> isGreaterThanEqualToAndLessThan(T lowerBound, T upperBound) {

      if (conditionHolds()) {
        if (notEqualToExpected(is(getTarget()).greaterThanEqualToAndLessThan(lowerBound, upperBound))) {
          throwAssertionException("[%1$s] is %2$sgreater than equal to [%3$s] and less than [%4$s]",
            getTarget(), negate(NOT), lowerBound, upperBound);
        }
      }

      return this;
    }

    /**
     * @inheritDoc
     */
    public @NotNull AssertThatExpression<T> isGreaterThanEqualToAndLessThanEqualTo(T lowerBound, T upperBound) {

      if (conditionHolds()) {
        if (notEqualToExpected(is(getTarget()).greaterThanEqualToAndLessThanEqualTo(lowerBound, upperBound))) {
          throwAssertionException("[%1$s] is %2$sgreater than equal to [%3$s] and less than equal to [%4$s]",
            getTarget(), negate(NOT), lowerBound, upperBound);
        }
      }

      return this;
    }

    /**
     * @inheritDoc
     */
    public @NotNull AssertThatExpression<T> hasText() {
      isNotBlank();
      return this;
    }

    /**
     * @inheritDoc
     */
    public @NotNull AssertThatExpression<T> holdsLock(Object lock) {

      if (conditionHolds()) {
        if (notEqualToExpected(Thread.holdsLock(lock))) {
          throwAssertionException("[%1$s] %2$slock [%3$s]", Thread.currentThread(),
            getExpected() ? "does not hold " : "holds ", lock);
        }
      }

      return this;
    }

    /**
     * @inheritDoc
     */
    @SuppressWarnings("rawtypes")
    public @NotNull AssertThatExpression<T> isInstanceOf(Class type) {

      if (conditionHolds()) {
        if (notEqualToExpected(is(getTarget()).instanceOf(type))) {
          throwAssertionException("[%1$s] is %2$san instance of [%3$s]",
            getTarget(), negate(NOT), ObjectUtils.getName(type));
        }
      }

      return this;
    }

    /**
     * @inheritDoc
     */
    public @NotNull AssertThatExpression<T> isLessThan(T upperBound) {

      if (conditionHolds()) {
        if (notEqualToExpected(is(getTarget()).lessThan(upperBound))) {
          throwAssertionException("[%1$s] is %2$sless than [%3$s]", getTarget(), negate(NOT), upperBound);
        }
      }

      return this;
    }

    /**
     * @inheritDoc
     */
    public @NotNull AssertThatExpression<T> isLessThanOrGreaterThan(T upperBound, T lowerBound) {

      if (conditionHolds()) {
        if (notEqualToExpected(is(getTarget()).lessThanOrGreaterThan(upperBound, lowerBound))) {
          throwAssertionException("[%1$s] is %2$sless than [%3$s] or greater than [%4$s]",
            getTarget(), negate(NOT), upperBound, lowerBound);
        }
      }

      return this;
    }

    /**
     * @inheritDoc
     */
    public @NotNull AssertThatExpression<T> isLessThanOrGreaterThanEqualTo(T upperBound, T lowerBound) {

      if (conditionHolds()) {
        if (notEqualToExpected(is(getTarget()).lessThanOrGreaterThanEqualTo(upperBound, lowerBound))) {
          throwAssertionException("[%1$s] is %2$sless than [%3$s] or greater than equal to [%4$s]",
            getTarget(), negate(NOT), upperBound, lowerBound);
        }
      }

      return this;
    }

    /**
     * @inheritDoc
     */
    public @NotNull AssertThatExpression<T> isLessThanEqualTo(T upperBound) {

      if (conditionHolds()) {
        if (notEqualToExpected(is(getTarget()).lessThanEqualTo(upperBound))) {
          throwAssertionException("[%1$s] is %2$sless than equal to [%3$s]",
            getTarget(), negate(NOT), upperBound);
        }
      }

      return this;
    }

    /**
     * @inheritDoc
     */
    public @NotNull AssertThatExpression<T> isLessThanEqualToOrGreaterThan(T upperBound, T lowerBound) {

      if (conditionHolds()) {
        if (notEqualToExpected(is(getTarget()).lessThanEqualToOrGreaterThan(upperBound, lowerBound))) {
          throwAssertionException("[%1$s] is %2$sless than equal to [%3$s] or greater than [%4$s]",
            getTarget(), negate(NOT), upperBound, lowerBound);
        }
      }

      return this;
    }

    /**
     * @inheritDoc
     */
    public @NotNull AssertThatExpression<T> isLessThanEqualToOrGreaterThanEqualTo(T upperBound, T lowerBound) {

      if (conditionHolds()) {
        if (notEqualToExpected(is(getTarget()).lessThanEqualToOrGreaterThanEqualTo(upperBound, lowerBound))) {
          throwAssertionException("[%1$s] is %2$sless than equal to [%3$s] or greater than equal to [%4$s]",
            getTarget(), negate(NOT), upperBound, lowerBound);
        }
      }

      return this;
    }

    /**
     * @inheritDoc
     */
    public @NotNull AssertThatExpression<T> isNotBlank() {

      if (conditionHolds()) {
        if (notEqualToExpected(is(getTarget()).notBlank())) {
          throwAssertionException("[%1$s] is %2$sblank", getTarget(),
            getExpected() ? StringUtils.EMPTY_STRING : NOT);
        }
      }

      return this;
    }

    /**
     * @inheritDoc
     */
    public @NotNull AssertThatExpression<T> isNotEmpty() {

      if (conditionHolds()) {
        if (notEqualToExpected(is(getTarget()).notEmpty())) {
          throwAssertionException("[%1$s] is %2$sempty", getTarget(),
            getExpected() ? StringUtils.EMPTY_STRING : NOT);
        }
      }

      return this;
    }

    /**
     * @inheritDoc
     */
    public @NotNull AssertThatExpression<T> isNotNull() {
      not().isNull();
      return this;
    }

    /**
     * @inheritDoc
     */
    public @NotNull AssertThatExpression<T> isNull() {

      if (conditionHolds()) {
        if (notEqualToExpected(is(getTarget()).Null())) {
          throwAssertionException("[%1$s] is %2$snull", getTarget(), negate(NOT));
        }
      }

      return this;
    }

    /**
     * @inheritDoc
     */
    public @NotNull AssertThatExpression<T> isSameAs(T obj) {

      if (conditionHolds()) {
        if (notEqualToExpected(is(getTarget()).sameAs(obj))) {
          throwAssertionException("[%1$s] is %2$sthe same as [%3$s]", getTarget(), negate(NOT), obj);
        }
      }

      return this;
    }

    /**
     * @inheritDoc
     */
    public @NotNull AssertThatExpression<T> isNotSameAs(T obj) {
      not().isSameAs(obj);
      return this;
    }

    /**
     * @inheritDoc
     */
    public @NotNull AssertThatExpression<T> isTrue() {

      if (conditionHolds()) {
        if (notEqualToExpected(is(getTarget()).True())) {
          throwAssertionException("[%1$s] is %2$strue", getTarget(), negate(NOT));
        }
      }

      return this;
    }

    /**
     * @inheritDoc
     */
    @Override
    public AssertThat<T> isValid(@NotNull Predicate<T> predicate) {

      Assert.notNull(predicate, "Predicate is required");

      if (conditionHolds()) {
        if (notEqualToExpected(predicate.test(getTarget()))) {
          throwAssertionException("[%1$s] is %2$svalid", getTarget(), negate(NOT));
        }
      }

      return this;
    }

    /**
     * @inheritDoc
     */
    public @NotNull AssertThat<T> not() {

      AssertThat<T> expression = new AssertThatExpression<>(getTarget(), getNotExpected());

      expression = this.transformer != null ? this.transformer.transform(expression) : expression;
      expression = this.message != null ? expression.describedAs(this.message) : expression;
      expression = expression.throwing(this.cause);
      expression = expression.when(this.condition);

      return expression;
    }

    /**
     * @inheritDoc
     */
    public @NotNull AssertThat<T> stating(@NotNull String message, @NotNull Object... args) {
      return stating(() -> format(message, args));
    }

    /**
     * @inheritDoc
     */
    public @NotNull AssertThat<T> stating(@Nullable Supplier<String> message) {
      this.message = message;
      return this;
    }

    /**
     * @inheritDoc
     */
    public @NotNull AssertThat<T> throwing(@Nullable RuntimeException cause) {
      this.cause = cause;
      return this;
    }

    /**
     * @inheritDoc
     */
    @Override
    public @NotNull AssertThat<T> transform(@NotNull Transformer<AssertThat<T>> assertionTransformer) {
      Assert.notNull(assertionTransformer, "The Transformer used to transform this assertion is required");
      this.transformer = assertionTransformer;
      return assertionTransformer.transform(this);
    }

    /**
     * @inheritDoc
     */
    public @NotNull AssertThat<T> when(@Nullable Condition condition) {
      this.condition = condition != null ? condition : DEFAULT_CONDITION;
      return this;
    }

    private String format(String message, Object... args) {
      return stringFormat(messageFormat(message, args), args);
    }

    private String messageFormat(String message, Object... args) {
      return MessageFormat.format(message, args);
    }

    private String stringFormat(String message, Object... args) {
      return String.format(message, args);
    }

    private String negate(String value) {
      return getExpected() ? value : EMPTY_STRING;
    }

    private void throwAssertionException(String defaultMessage, Object... args) {
      throw ObjectUtils.returnValueOrDefaultIfNull(this.cause,
        () -> new AssertionException(withMessage(defaultMessage, args)));
    }

    @SuppressWarnings("all")
    private String withMessage(String defaultMessage, Object... args) {

      Supplier<String> message = this.message;

      String suppliedMessage = message != null ? message.get() : null;
      String resolvedMessage = is(suppliedMessage).notBlank() ? suppliedMessage : format(defaultMessage, args);

      return resolvedMessage;
    }
  }

  /**
   * The {@link AssertThatWrapper} class is a {@literal Decorator} used to decorate or modify the existing behavior
   * and/or functionality of an existing assertion ({@link AssertThat} instance).
   *
   * This class makes it easier to extend and customize any existing assertion in the {@code transform(..)} operation.
   *
   * @param <T> {@link Class type} of {@link Object} to evaluate and make an assertion.
   * @see AssertThat
   */
  public static class AssertThatWrapper<T> implements AssertThat<T> {

    /**
     * Factory method used to construct a new instance of {@link AssertThatWrapper} initialized with
     * the given {@link AssertThat} object.
     *
     * @param <T> {@link Class type} of the {@link Object} to evaluate and make an assertion.
     * @param delegate {@link AssertThat} object instance used as the delegate for this wrapper.
     * @return a new {@link AssertThatWrapper} instance wrapping the given {@link AssertThat} object.
     * @throws IllegalArgumentException if the {@link AssertThat} object is {@literal null}.
     * @see AssertThat
     */
    public static <T> AssertThat<T> wrap(@NotNull AssertThat<T> delegate) {
      return new AssertThatWrapper<>(delegate);
    }

    private final AssertThat<T> delegate;

    /**
     * Constructs a new instance of {@link AssertThatWrapper} initialized with the given {@link AssertThat} object.
     *
     * @param delegate {@link AssertThat} object instance used as the delegate for this wrapper.
     * @throws IllegalArgumentException if the {@link AssertThat} object is {@literal null}.
     */
    public AssertThatWrapper(@NotNull AssertThat<T> delegate) {

      Assert.notNull(delegate, "AssertThat delegate is required");

      this.delegate = delegate;
    }

    /**
     * Returns a reference to the configured {@link AssertThat} object wrapped by this wrapper.
     *
     * @return a reference to the configured {@link AssertThat} object wrapped by this wrapper.
     * @see AssertThat
     */
    protected AssertThat<T> getDelegate() {
      return this.delegate;
    }

    /**
     * @inheritDoc
     */
    @Override
    @SuppressWarnings("rawtypes")
    public @NotNull AssertThat<T> isAssignableTo(Class type) {
      return getDelegate().isAssignableTo(type);
    }

    /**
     * @inheritDoc
     */
    @Override
    public @NotNull AssertThat<T> isComparableTo(Comparable<T> obj) {
      return getDelegate().isComparableTo(obj);
    }

    /**
     * @inheritDoc
     */
    @Override
    public @NotNull AssertThat<T> isNotComparableTo(Comparable<T> obj) {
      return getDelegate().isNotComparableTo(obj);
    }

    /**
     * @inheritDoc
     */
    @Override
    public @NotNull AssertThat<T> isEqualTo(T obj) {
      return getDelegate().isEqualTo(obj);
    }

    /**
     * @inheritDoc
     */
    @Override
    public @NotNull AssertThat<T> isNotEqualTo(T obj) {
      return getDelegate().isNotEqualTo(obj);
    }

    /**
     * @inheritDoc
     */
    @Override
    public @NotNull AssertThat<T> isFalse() {
      return getDelegate().isFalse();
    }

    /**
     * @inheritDoc
     */
    @Override
    public @NotNull AssertThat<T> isGreaterThan(T lowerBound) {
      return getDelegate().isGreaterThan(lowerBound);
    }

    /**
     * @inheritDoc
     */
    @Override
    public @NotNull AssertThat<T> isGreaterThanAndLessThan(T lowerBound, T upperBound) {
      return getDelegate().isGreaterThanAndLessThan(lowerBound, upperBound);
    }

    /**
     * @inheritDoc
     */
    @Override
    public @NotNull AssertThat<T> isGreaterThanAndLessThanEqualTo(T lowerBound, T upperBound) {
      return getDelegate().isGreaterThanAndLessThanEqualTo(lowerBound, upperBound);
    }

    /**
     * @inheritDoc
     */
    @Override
    public @NotNull AssertThat<T> isGreaterThanEqualTo(T lowerBound) {
      return getDelegate().isGreaterThanEqualTo(lowerBound);
    }

    /**
     * @inheritDoc
     */
    @Override
    public @NotNull AssertThat<T> isGreaterThanEqualToAndLessThan(T lowerBound, T upperBound) {
      return getDelegate().isGreaterThanEqualToAndLessThan(lowerBound, upperBound);
    }

    /**
     * @inheritDoc
     */
    @Override
    public @NotNull AssertThat<T> isGreaterThanEqualToAndLessThanEqualTo(T lowerBound, T upperBound) {
      return getDelegate().isGreaterThanEqualToAndLessThanEqualTo(lowerBound, upperBound);
    }

    /**
     * @inheritDoc
     */
    @Override
    public @NotNull AssertThat<T> hasText() {
      return getDelegate().hasText();
    }

    /**
     * @inheritDoc
     */
    @Override
    public @NotNull AssertThat<T> holdsLock(Object lock) {
      return getDelegate().holdsLock(lock);
    }

    /**
     * @inheritDoc
     */
    @Override
    @SuppressWarnings("rawtypes")
    public @NotNull AssertThat<T> isInstanceOf(Class type) {
      return getDelegate().isInstanceOf(type);
    }

    /**
     * @inheritDoc
     */
    @Override
    public @NotNull AssertThat<T> isLessThan(T upperBound) {
      return getDelegate().isLessThan(upperBound);
    }

    /**
     * @inheritDoc
     */
    @Override
    public @NotNull AssertThat<T> isLessThanOrGreaterThan(T upperBound, T lowerBound) {
      return getDelegate().isLessThanOrGreaterThan(upperBound, lowerBound);
    }

    /**
     * @inheritDoc
     */
    @Override
    public @NotNull AssertThat<T> isLessThanOrGreaterThanEqualTo(T upperBound, T lowerBound) {
      return getDelegate().isLessThanOrGreaterThanEqualTo(upperBound, lowerBound);
    }

    /**
     * @inheritDoc
     */
    @Override
    public @NotNull AssertThat<T> isLessThanEqualTo(T upperBound) {
      return getDelegate().isLessThanEqualTo(upperBound);
    }

    /**
     * @inheritDoc
     */
    @Override
    public @NotNull AssertThat<T> isLessThanEqualToOrGreaterThan(T upperBound, T lowerBound) {
      return getDelegate().isLessThanEqualToOrGreaterThan(upperBound, lowerBound);
    }

    /**
     * @inheritDoc
     */
    @Override
    public @NotNull AssertThat<T> isLessThanEqualToOrGreaterThanEqualTo(T upperBound, T lowerBound) {
      return getDelegate().isLessThanEqualToOrGreaterThanEqualTo(upperBound, lowerBound);
    }

    /**
     * @inheritDoc
     */
    @Override
    public @NotNull AssertThat<T> isNotBlank() {
      return getDelegate().isNotBlank();
    }

    /**
     * @inheritDoc
     */
    @Override
    public @NotNull AssertThat<T> isNotEmpty() {
      return getDelegate().isNotEmpty();
    }

    /**
     * @inheritDoc
     */
    @Override
    public @NotNull AssertThat<T> isNotNull() {
      return getDelegate().isNotNull();
    }

    /**
     * @inheritDoc
     */
    @Override
    public @NotNull AssertThat<T> isNull() {
      return getDelegate().isNull();
    }

    /**
     * @inheritDoc
     */
    @Override
    public @NotNull AssertThat<T> isSameAs(T obj) {
      return getDelegate().isSameAs(obj);
    }

    /**
     * @inheritDoc
     */
    @Override
    public @NotNull AssertThat<T> isNotSameAs(T obj) {
      return getDelegate().isNotSameAs(obj);
    }

    /**
     * @inheritDoc
     */
    @Override
    public @NotNull AssertThat<T> isTrue() {
      return getDelegate().isTrue();
    }

    /**
     * @inheritDoc
     */
    @Override
    public AssertThat<T> isValid(Predicate<T> predicate) {
      return getDelegate().isValid(predicate);
    }

    /**
     * @inheritDoc
     */
    @Override
    public AssertThat<T> not() {
      return new AssertThatWrapper<>(getDelegate().not());
    }

    /**
     * @inheritDoc
     */
    @Override
    public AssertThat<T> stating(String message, Object... args) {
      getDelegate().stating(message, args);
      return this;
    }

    /**
     * @inheritDoc
     */
    @Override
    public AssertThat<T> stating(Supplier<String> message) {
      getDelegate().stating(message);
      return null;
    }

    /**
     * @inheritDoc
     */
    @Override
    public AssertThat<T> throwing(RuntimeException cause) {
      getDelegate().throwing(cause);
      return this;
    }

    /**
     * @inheritDoc
     */
    @Override
    public AssertThat<T> transform(Transformer<AssertThat<T>> assertionTransformer) {
      return new AssertThatWrapper<>(assertionTransformer.transform(getDelegate()));
    }

    /**
     * @inheritDoc
     */
    @Override
    public AssertThat<T> when(Condition condition) {
      getDelegate().when(condition);
      return this;
    }
  }

  /**
   * The {@literal is} operator can be used to make logical determinations about an {@link Object} such as boolean,
   * equality, identity, relational or type comparisons with other {@link Object Objects}, and so on.
   *
   * @param <T> {@link Class type} of {@link Object} as the {@literal subject} of the {@literal is} operator.
   * @param obj {@link Object} that is the subject of the operation.
   * @return a new instance of the {@literal is} operator.
   * @see org.cp.elements.lang.LangExtensions.IsExpression
   * @see org.cp.elements.lang.annotation.FluentApi
   * @see org.cp.elements.lang.annotation.Dsl
   */
  @Dsl
  @FluentApi
  public static <T> Is<T> is(T obj) {
    return new IsExpression<>(obj);
  }

  /**
   * The {@link Is} interface defines operations to classify a single {@link Object} based on its equality, identity,
   * state, type or relationship to another {@link Object}.
   *
   * @param <T> {@link Class type} of {@link Object} as the {@literal subject} of the {@literal is} operator.
   * @see org.cp.elements.lang.FluentApiExtension
   * @see org.cp.elements.lang.DslExtension
   */
  public interface Is<T> extends DslExtension, FluentApiExtension {

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
    @SuppressWarnings("rawtypes")
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
   * The {@link IsExpression} class is an implementation of the Is interface, is operator.
   *
   * Note, this implementation is Thread-safe, although it is very unlikely that a {@link Thread}
   * will share an instance of this class since every invocation of the {@link #is(Object)} operator factory method
   * will return a new instance of this class, at least for the time being.
   *
   * @param <T> {@link Class type} of the the {@link Object} subject.
   * @see org.cp.elements.lang.LangExtensions.Is
   */
  private static final class IsExpression<T> implements Is<T> {

    private static final boolean DEFAULT_EXPECTED = true;

    private final boolean expected;

    private final T obj;

    private IsExpression(T obj) {
      this(obj, DEFAULT_EXPECTED);
    }

    private IsExpression(T obj, boolean expected) {
      this.obj = obj;
      this.expected = expected;
    }

    private boolean equalToExpected(boolean actualOutcome) {
      return (actualOutcome == expected);
    }

    private LogicalOperator getOp(LogicalOperator op) {
      return (expected ? op : op.getOpposite());
    }

    private Class<?> toClass(Object obj) {
      return (obj instanceof Class ? (Class<?>) obj : obj.getClass());
    }

    @SuppressWarnings("unchecked")
    private Comparable<T> toComparable(T obj) {
      return (Comparable<T>) obj;
    }

    public boolean assignableTo(Class<?> type) {
      return equalToExpected(obj != null && type != null && type.isAssignableFrom(toClass(obj)));
    }

    public boolean comparableTo(T obj) {
      return equalToExpected(toComparable(this.obj).compareTo(obj) == 0);
    }

    public boolean equalTo(T obj) {
      return equalToExpected(this.obj != null && this.obj.equals(obj));
    }

    public boolean notEqualTo(T obj) {
      return not().equalTo(obj);
    }

    public boolean False() {
      return equalToExpected(Boolean.FALSE.equals(this.obj));
    }

    public boolean greaterThan(T lowerBound) {
      return equalToExpected(toComparable(this.obj).compareTo(lowerBound) > 0);
    }

    public boolean greaterThanAndLessThan(T lowerBound, T upperBound) {
      return getOp(LogicalOperator.AND).evaluate(greaterThan(lowerBound), lessThan(upperBound));
    }

    public boolean greaterThanAndLessThanEqualTo(T lowerBound, T upperBound) {
      return getOp(LogicalOperator.AND).evaluate(greaterThan(lowerBound), lessThanEqualTo(upperBound));
    }

    public boolean greaterThanEqualTo(T lowerBound) {
      return equalToExpected(toComparable(this.obj).compareTo(lowerBound) >= 0);
    }

    public boolean greaterThanEqualToAndLessThan(T lowerBound, T upperBound) {
      return getOp(LogicalOperator.AND).evaluate(greaterThanEqualTo(lowerBound), lessThan(upperBound));
    }

    public boolean greaterThanEqualToAndLessThanEqualTo(T lowerBound, T upperBound) {
      return getOp(LogicalOperator.AND).evaluate(greaterThanEqualTo(lowerBound), lessThanEqualTo(upperBound));
    }

    @SuppressWarnings("rawtypes")
    public boolean instanceOf(Class type) {
      return equalToExpected(type != null && type.isInstance(this.obj));
    }

    public boolean lessThan(T upperBound) {
      return equalToExpected(toComparable(this.obj).compareTo(upperBound) < 0);
    }

    public boolean lessThanOrGreaterThan(T upperBound, T lowerBound) {
      return getOp(LogicalOperator.OR).evaluate(lessThan(upperBound), greaterThan(lowerBound));
    }

    public boolean lessThanOrGreaterThanEqualTo(T upperBound, T lowerBound) {
      return getOp(LogicalOperator.OR).evaluate(lessThan(upperBound), greaterThanEqualTo(lowerBound));
    }

    public boolean lessThanEqualTo(T upperBound) {
      return equalToExpected(toComparable(this.obj).compareTo(upperBound) <= 0);
    }

    public boolean lessThanEqualToOrGreaterThan(T upperBound, T lowerBound) {
      return getOp(LogicalOperator.OR).evaluate(lessThanEqualTo(upperBound), greaterThan(lowerBound));
    }

    public boolean lessThanEqualToOrGreaterThanEqualTo(T upperBound, T lowerBound) {
      return getOp(LogicalOperator.OR).evaluate(lessThanEqualTo(upperBound), greaterThanEqualTo(lowerBound));
    }

    public boolean notBlank() {
      return StringUtils.hasText(ObjectUtils.toString(this.obj));
    }

    public boolean notEmpty() {

      boolean result = (this.obj instanceof Object[] && ((Object[]) this.obj).length != 0);

      result |= (this.obj instanceof Collection && !((Collection<?>) this.obj).isEmpty());
      result |= (this.obj instanceof Map && !((Map<?, ?>) this.obj).isEmpty());
      result |= (this.obj instanceof String && !this.obj.toString().isEmpty());

      return result;
    }

    public boolean notNull() {
      return not().Null();
    }

    public boolean Null() {
      return equalToExpected(this.obj == null);
    }

    public boolean notSameAs(T obj) {
      return not().sameAs(obj);
    }

    public boolean sameAs(T obj) {
      return equalToExpected(this.obj == obj);
    }

    public boolean True() {
      return equalToExpected(Boolean.TRUE.equals(this.obj));
    }

    public Is<T> not() {
      return new IsExpression<>(this.obj, !expected);
    }
  }
}
