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
package org.cp.elements.lang.reflect.support;

import static org.cp.elements.lang.ElementsExceptionsFactory.newUnhandledMethodInvocationException;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.lang.reflect.UnhandledMethodInvocationException;
import org.cp.elements.util.ArrayUtils;

/**
 * Implementation of the {@link InvocationHandler} interface composed of an array or a collection of
 * {@link InvocationHandler InvocationHandlers} forming a {@literal Composite} object serving as
 * a single instance of {@link InvocationHandler}.
 *
 * @author John Blum
 * @see java.lang.Iterable
 * @see java.lang.reflect.InvocationHandler
 * @see java.lang.reflect.Method
 * @see <a href="https://en.wikipedia.org/wiki/Composite_pattern">Composite Software Design Pattern</a>
 * @since 1.0.0
 */
public class ComposableInvocationHandler implements InvocationHandler, Iterable<InvocationHandler> {

  /**
   * Factory method used to compose an array of {@link InvocationHandler InvocationHandlers} into a Composite object.
   *
   * @param <T> {@link Class} type of the {@link InvocationHandler}.
   * @param invocationHandlers array of {@link InvocationHandler InvocationHandlers} to compose.
   * @return a new instance of {@link ComposableInvocationHandler} composed of
   * the array of {@link InvocationHandler InvocationHandlers}.
   * @see #ComposableInvocationHandler(InvocationHandler...)
   * @see java.lang.reflect.InvocationHandler
   */
  @SafeVarargs
  public static @NotNull <T extends InvocationHandler> ComposableInvocationHandler compose(T... invocationHandlers) {
    return new ComposableInvocationHandler(invocationHandlers);
  }

  /**
   * Factory method used to compose an {@link Iterable} of {@link InvocationHandler InvocationHandlers}
   * into a Composite object.
   *
   * @param invocationHandlers {@link Iterable} of {@link InvocationHandler InvocationHandlers} to compose.
   * @return a new instance of {@link ComposableInvocationHandler} composed of
   * the {@link Iterable} of {@link InvocationHandler InvocationHandlers}.
   * @see #ComposableInvocationHandler(InvocationHandler...)
   * @see java.lang.reflect.InvocationHandler
   */
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public static @NotNull ComposableInvocationHandler compose(Iterable<? extends InvocationHandler> invocationHandlers) {
    return new ComposableInvocationHandler(ArrayUtils.asArray((Iterable) invocationHandlers, InvocationHandler.class));
  }

  private final List<InvocationHandler> invocationHandlers;

  /**
   * Constructs a new {@link ComposableInvocationHandler} initialized with the given
   * array of {@link InvocationHandler} objects.
   *
   * @param invocationHandlers array of {@link InvocationHandler InvocationHandlers} to compose as a single,
   * indivisible {@link InvocationHandler}.
   * @see java.lang.reflect.InvocationHandler
   */
  @NullSafe
  protected ComposableInvocationHandler(InvocationHandler... invocationHandlers) {
    this.invocationHandlers = Arrays.asList(ArrayUtils.nullSafeArray(invocationHandlers, InvocationHandler.class));
  }

  /**
   * Returns the {@link InvocationHandler InvocationHandlers} that make up this composition.
   *
   * @return an {@link Iterable} of {@link InvocationHandler InvocationHandlers} that make up this composition.
   * @see java.lang.reflect.InvocationHandler
   */
  @SuppressWarnings("all")
  protected @NotNull Iterable<InvocationHandler> getInvocationHandlers() {
    return Collections.unmodifiableList(this.invocationHandlers);
  }

  /**
   * Returns an {@link Iterator} over the collection of {@link InvocationHandler} objects contained in this Composite.
   *
   * @return an {@link Iterator} over the collection of {@link InvocationHandler} objects contained in this Composite.
   * @see java.lang.reflect.InvocationHandler
   * @see #getInvocationHandlers()
   * @see java.util.Iterator
   */
  @Override
  public @NotNull Iterator<InvocationHandler> iterator() {
    return getInvocationHandlers().iterator();
  }

  /**
   * Composite operation that delegates the invocation of the given {@link Method} on the {@code Proxy}
   * to each {@link InvocationHandler} contained in this {@literal Composite} returning the value
   * of the first {@link InvocationHandler} in the collection to handle the {@link Method} invocation.
   * <p>
   * If no {@link InvocationHandler} can successfully handle the {@link Method} invocation,
   * then a {@link UnhandledMethodInvocationException} will be thrown.
   *
   * @param proxy {@link Object proxy} on which the {@link Method} was invoked.
   * @param method {@link Method} that was called.
   * @param args array of {@link Object arguments} passed to the {@link Method}.
   * @return the {@link Object return value} of the {@link Method} or the {@link Object value} provided by
   * the {@link InvocationHandler} as a result of handling the proxy of the {@link Method}.
   * @throws UnhandledMethodInvocationException if all {@link Method} invocation handlers fail
   * to handle the {@link Method}.
   * @see java.lang.reflect.InvocationHandler#invoke(Object, Method, Object[])
   */
  @Override
  public @Nullable Object invoke(Object proxy, Method method, Object[] args) throws Throwable {

    for (InvocationHandler invocationHandler : this) {
      try {
        return invocationHandler.invoke(proxy, method, args);
      }
      catch (UnhandledMethodInvocationException ignore) { }
    }

    throw newUnhandledMethodInvocationException("Method [%s] was not handled", method.getName());
  }
}
