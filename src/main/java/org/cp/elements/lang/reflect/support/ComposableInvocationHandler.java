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

package org.cp.elements.lang.reflect.support;

import static java.util.Arrays.asList;
import static org.cp.elements.util.ArrayUtils.asArray;
import static org.cp.elements.util.ArrayUtils.nullSafeArray;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.reflect.UnhandledMethodInvocationException;

/**
 * The {@link ComposableInvocationHandler} class is an implementation of the {@link InvocationHandler}
 * {@link Class interface} composed of a collection of {@link InvocationHandler InvocationHandlers}
 * forming a Composite object serving as a single instance of {@link InvocationHandler}.
 *
 * @author John Blum
 * @see java.lang.Iterable
 * @see java.lang.reflect.InvocationHandler
 * @see java.lang.reflect.Method
 * @since 1.0.0
 */
public class ComposableInvocationHandler implements InvocationHandler, Iterable<InvocationHandler> {

  private final List<InvocationHandler> invocationHandlers;

  /**
   * Factory method to compose the array of {@link InvocationHandler InvocationHandlers} into a Composite object.
   *
   * @param <T> {@link Class} type of the {@link InvocationHandler}.
   * @param invocationHandlers array of {@link InvocationHandler InvocationHandlers} to compose.
   * @return a new instance of {@link ComposableInvocationHandler} composed of
   * the array of {@link InvocationHandler InvocationHandlers}.
   * @see #ComposableInvocationHandler(InvocationHandler...)
   * @see java.lang.reflect.InvocationHandler
   */
  @SafeVarargs
  public static <T extends InvocationHandler> ComposableInvocationHandler compose(T... invocationHandlers) {
    return new ComposableInvocationHandler(invocationHandlers);
  }

  /**
   * Factory method to compose the {@link Iterable} of {@link InvocationHandler InvocationHandlers}
   * into a Composite object.
   *
   * @param invocationHandlers {@link Iterable} of {@link InvocationHandler InvocationHandlers} to compose.
   * @return a new instance of {@link ComposableInvocationHandler} composed of
   * the {@link Iterable} of {@link InvocationHandler InvocationHandlers}.
   * @see #ComposableInvocationHandler(InvocationHandler...)
   * @see java.lang.reflect.InvocationHandler
   */
  @SuppressWarnings("unchecked")
  public static ComposableInvocationHandler compose(Iterable<? extends InvocationHandler> invocationHandlers) {
    return new ComposableInvocationHandler(asArray((Iterable) invocationHandlers, InvocationHandler.class));
  }

  /**
   * Constructs an instance of {@link ComposableInvocationHandler} initialized with the given
   * array of {@link InvocationHandler} objects.
   *
   * @param invocationHandlers array of {@link InvocationHandler InvocationHandlers} to compose as a single,
   * indivisible {@link InvocationHandler}.
   * @see java.lang.reflect.InvocationHandler
   */
  @NullSafe
  protected ComposableInvocationHandler(InvocationHandler... invocationHandlers) {
    this.invocationHandlers = asList(nullSafeArray(invocationHandlers, InvocationHandler.class));
  }

  /**
   * Returns the {@link InvocationHandler InvocationHandlers} that make up this composition.
   *
   * @return an {@link Iterable} of {@link InvocationHandler InvocationHandlers} that make up this composition.
   * @see java.lang.reflect.InvocationHandler
   */
  @SuppressWarnings("all")
  protected Iterable<InvocationHandler> getInvocationHandlers() {
    return Collections.unmodifiableList(this.invocationHandlers);
  }

  /**
   * Returns an {@link Iterator} over the collection of {@link InvocationHandler} objects contained in this Composite.
   *
   * @return an {@link Iterator} over the collection of {@link InvocationHandler} objects contained in this Composite.
   * @see java.lang.reflect.InvocationHandler
   * @see java.util.Iterator
   */
  @Override
  public Iterator<InvocationHandler> iterator() {
    return Collections.unmodifiableList(this.invocationHandlers).iterator();
  }

  /**
   * Composite operation that delegates the invocation of the given {@link Method} on the {@code proxy}
   * to each {@link InvocationHandler} contained in this Composite returning the value
   * of the first {@link InvocationHandler} in the collection to handle the {@link Method} invocation.
   *
   * If no {@link InvocationHandler} can successfully handle the {@link Method} invocation,
   * then a {@link UnhandledMethodInvocationException} will be thrown.
   *
   * @param proxy {@link Object proxy} on which the {@link Method} was invoked.
   * @param method {@link Method} that was called.
   * @param args array of {@link Object arguments} passed to the {@link Method}.
   * @return the return value of the {@link Method} or the value provided by the {@link InvocationHandler}
   * as a result of handling the proxied {@link Method}.
   * @throws UnhandledMethodInvocationException if all {@link Method} invocation handlers fail
   * to handle the {@link Method}.
   * @see java.lang.reflect.InvocationHandler#invoke(Object, Method, Object[])
   */
  @Override
  public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
    for (InvocationHandler invocationHandler : this) {
      try {
        return invocationHandler.invoke(proxy, method, args);
      }
      catch (UnhandledMethodInvocationException ignore) {
      }
    }

    throw new UnhandledMethodInvocationException(String.format("Method [%s] was not handled", method.getName()));
  }
}
