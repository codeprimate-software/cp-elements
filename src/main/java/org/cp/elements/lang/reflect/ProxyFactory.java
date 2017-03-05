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

package org.cp.elements.lang.reflect;

import static org.cp.elements.lang.ClassUtils.getInterfaces;
import static org.cp.elements.util.ArrayUtils.nullSafeArray;
import static org.cp.elements.util.stream.StreamUtils.stream;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.cp.elements.lang.ClassUtils;
import org.cp.elements.lang.NullSafe;

/**
 * The {@link ProxyFactory} class is a factory object used to create a proxy for a given target {@link Object}
 * that will implement the given set of {@link Class interfaces}.
 *
 * @author John Blum
 * @param <T> {@link Class} type of the {@link Object} to proxy.
 * @see java.lang.reflect.Proxy
 * @see org.cp.elements.lang.reflect.ProxyService
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class ProxyFactory<T> {

  private Class<?>[] proxyInterfaces;

  private ClassLoader proxyClassLoader;

  private List<MethodInterceptor> methodInterceptors = new ArrayList<>();

  private Object target;

  /**
   * Returns a reference to a {@link ProxyFactory} service provider implementation provided by Elements that delegates
   * all proxy operations to any/all application configured {@link ProxyFactory} service provider implementations.
   *
   * @param <T> {@link Class} type of the {@link Object} to proxy.
   * @return a reference to the Elements-defined {@link ProxyFactory} service provider implementation.
   * @see org.cp.elements.lang.reflect.ProxyFactory
   * @see org.cp.elements.lang.reflect.ProxyService
   */
  @SuppressWarnings({ "unchecked", "unused" })
  protected static <T> ProxyFactory<T> newProxyFactory() {

    return new ProxyFactory<T>() {

      @Override
      public boolean canProxy(Object target, Class[] proxyInterfaces) {
        return ProxyService.<T>newProxyService().canProxy(target, proxyInterfaces);
      }

      @Override
      public Object newProxy() {
        return stream(ProxyService.<T>newProxyService())
          .filter(proxyFactory -> proxyFactory.canProxy(getTarget(), getProxyInterfaces())).findFirst()
            .flatMap(proxyFactory -> Optional.of(proxyFactory.newProxy())).orElse(getTarget());
      }
    };
  }

  /**
   * Factory method used to constract a new instance of {@link ProxyFactory} that will be used to create a proxy
   * for the given {@link Object} implementing the given array of {@link Class interfaces}.
   *
   * @param <T> {@link Class} type of the {@link Object} to proxy.
   * @param target {@link Object} to proxy.
   * @param proxyInterfaces array of {@link Class interfaces} to be implemented by the Proxy.
   * @return a new instance of the Elements-defined {@link ProxyFactory} service provider implementation
   * used to create a proxy for the given target {@link Object}.
   * @see #newProxyFactory()
   * @see #proxy(Object)
   * @see #implementing(Class[])
   */
  public static <T> ProxyFactory<T> newProxyFactory(T target, Class<?>... proxyInterfaces) {
    return ProxyFactory.<T>newProxyFactory().proxy(target).implementing(proxyInterfaces);
  }

  /**
   * Resolves all the {@link Class interfaces} implemented by the given {@link Object object's} {@link Class}
   * including all the given {@link Class interfaces} in the {@link Class} array.
   *
   * @param obj {@link Object} to evaluate.
   * @param interfaces array of {@link Class interfaces} to includes in the result.
   * @return an array of {@link Class interfaces} implemented by the given {@link Object object's} {@link Class}
   * including all {@link Class interfaces} from the given {@link Class} array.
   * @see org.cp.elements.lang.ClassUtils#getInterfaces(Object)
   * @see java.lang.Class
   * @see java.lang.Object
   */
  @NullSafe
  protected static Class<?>[] resolveInterfaces(Object obj, Class<?>... interfaces) {
    Set<Class<?>> allImplementedInterfaces = new HashSet<>(getInterfaces(obj));

    allImplementedInterfaces.addAll(stream(nullSafeArray(interfaces, Class.class))
      .filter(ClassUtils::isInterface).collect(Collectors.toList()));

    return allImplementedInterfaces.toArray(new Class<?>[allImplementedInterfaces.size()]);
  }

  /**
   * Determines whether this {@link ProxyFactory} can proxy the given {@link Object}
   * with the specified {@link Class interfaces}.
   *
   * @param target {@link Object} to proxy.
   * @param proxyInterfaces array of {@link Class interfaces} to be implemented by the created Proxy {@link Object}.
   * @return a boolean value indicating whether this {@link ProxyFactory} can proxy the given {@link Object}
   * with the specified array of {@link Class interface}.
   * @see java.lang.Class
   * @see java.lang.Object
   */
  public abstract boolean canProxy(Object target, Class<?>... proxyInterfaces);

  /**
   * Returns an {@link Iterable} of {@link MethodInterceptor MethodInterceptors} used to advise
   * the {@link MethodInvocation method invocations} on the Proxy.
   *
   * @return an {@link Iterable} of {@link MethodInterceptor MethodInterceptors} used to advise
   * the {@link MethodInvocation method invocations} on the Proxy.
   * @see org.cp.elements.lang.reflect.MethodInterceptor
   * @see java.lang.Iterable
   */
  @NullSafe
  protected Iterable<MethodInterceptor> getMethodInterceptors() {
    return Collections.unmodifiableList(this.methodInterceptors);
  }

  /**
   * Gets the Java {@link ClassLoader} used to resolve and define the Proxy {@link Class}.
   *
   * If the {@link ClassLoader} was not configured then the current {@link Thread Thread's} {@link ClassLoader}
   * will be used.
   *
   * @return the Java {@link ClassLoader} used to resolve and define the Proxy {@link Class}.
   * @see java.lang.Thread#currentThread().getContextClassLoader()
   * @see java.lang.ClassLoader
   */
  @NullSafe
  protected ClassLoader getProxyClassLoader() {
    return Optional.ofNullable(this.proxyClassLoader).orElseGet(() -> Thread.currentThread().getContextClassLoader());
  }

  /**
   * Returns an array of {@link Class interfaces} implemented by the Proxy.
   *
   * @return an array of {@link Class interfaces} implemented by the Proxy.
   * Returns an empty array if no {@link Class interfaces} were configured.
   * @see java.lang.Class
   */
  @NullSafe
  protected Class<?>[] getProxyInterfaces() {
    return nullSafeArray(this.proxyInterfaces, Class.class);
  }

  /**
   * Returns the {@link Object} to Proxy.
   *
   * @return the {@link Object} to Proxy.
   * @see java.lang.Object
   */
  protected Object getTarget() {
    return this.target;
  }

  /**
   * Adds the array of {@link MethodInterceptor MethodInterceptors} to the Advice used to intercept
   * the {@link java.lang.reflect.Method} invocation and apply AOP-style advice
   * to the Proxy {@link java.lang.reflect.Method}.
   *
   * @param methodInterceptors array of {@link MethodInterceptor MethodInterceptors} used to advise the Proxy object
   * {@link java.lang.reflect.Method} invocation.
   * @return this {@link ProxyFactory}.
   * @see org.cp.elements.lang.reflect.MethodInterceptor
   * @see org.cp.elements.lang.reflect.ProxyFactory
   * @see #getMethodInterceptors()
   */
  @NullSafe
  public ProxyFactory<T> adviseWith(MethodInterceptor... methodInterceptors) {
    Collections.addAll(this.methodInterceptors, nullSafeArray(methodInterceptors));
    return this;
  }

  /**
   * Configures the {@link Class interfaces} to be implemented by the Proxy.
   *
   * @param proxyInterfaces array of {@link Class interfaces} to be implemented by the Proxy.
   * @return this {@link ProxyFactory}.
   * @see org.cp.elements.lang.reflect.ProxyFactory
   * @see java.lang.Class
   * @see #getProxyInterfaces()
   */
  public ProxyFactory<T> implementing(Class<?>... proxyInterfaces) {
    this.proxyInterfaces = proxyInterfaces;
    return this;
  }

  /**
   * Sets the {@link Object} to Proxy.
   *
   * @param target {@link Object} to Proxy.
   * @return this {@link ProxyFactory}
   * @see org.cp.elements.lang.reflect.ProxyFactory
   * @see java.lang.Object
   * @see #getTarget()
   */
  public ProxyFactory<T> proxy(Object target) {
    this.target = target;
    return this;
  }

  /**
   * Configures the Java {@link ClassLoader} used to resolve and define the Proxy {@link Class}.
   *
   * @param proxyClassLoader Java {@link ClassLoader} used to resolve and define the Proxy {@link Class}.
   * @return this {@link ProxyFactory}.
   * @see org.cp.elements.lang.reflect.ProxyFactory
   * @see java.lang.ClassLoader
   * @see #getProxyClassLoader()
   */
  public ProxyFactory<T> using(ClassLoader proxyClassLoader) {
    this.proxyClassLoader = proxyClassLoader;
    return this;
  }

  /**
   * Constructs, configures and initializes a new Proxy.
   *
   * @param <T> {@link Class} type of the new Proxy object.
   * @return a new Proxy object of the given {@link Class} type T.
   */
  public abstract<T> T newProxy();

}
