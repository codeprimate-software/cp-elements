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

package org.cp.elements.lang.reflect;

import static org.assertj.core.api.Assertions.assertThat;
import static org.cp.elements.lang.reflect.ProxyFactory.newProxyFactory;
import static org.mockito.Mockito.mock;

import java.io.Serializable;

import org.cp.elements.lang.Auditable;
import org.cp.elements.lang.Identifiable;
import org.junit.Test;

import lombok.Data;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;

/**
 * Unit tests for {@link ProxyFactory}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.cp.elements.lang.reflect.ProxyFactory
 * @since 1.0.0
 */
public class ProxyFactoryTests {

  private final Object target = new Object();

  @Test
  public void newProxyFactoryIsNotNull() {
    ProxyFactory<Object> proxyFactory = newProxyFactory();

    assertThat(proxyFactory).isNotNull();
    assertThat(proxyFactory.getMethodInterceptors()).isEmpty();
    assertThat(proxyFactory.getProxyClassLoader()).isEqualTo(Thread.currentThread().getContextClassLoader());
    assertThat(proxyFactory.getProxyInterfaces()).isEmpty();
    assertThat(proxyFactory.getTarget()).isNull();
  }

  @Test
  public void newProxyFactoryForTargetObjectAndInterfaces() {
    Contact johnBlum = Contact.newContact("John Blum");

    ProxyFactory<Object> proxyFactory = newProxyFactory(johnBlum, Auditable.class, Identifiable.class, Runnable.class);

    assertThat(proxyFactory).isNotNull();
    assertThat(proxyFactory.getMethodInterceptors()).isEmpty();
    assertThat(proxyFactory.getProxyClassLoader()).isEqualTo(Thread.currentThread().getContextClassLoader());
    assertThat(proxyFactory.getProxyInterfaces()).contains(Auditable.class, Identifiable.class, Runnable.class);
    assertThat(proxyFactory.getTarget()).isEqualTo(johnBlum);
  }

  @Test
  public void resolvesGivenInterfaces() {
    assertThat(ProxyFactory.resolveInterfaces(Contact.newContact("John Blum"), Auditable.class, Identifiable.class))
      .contains(Auditable.class, Identifiable.class);
  }

  @Test
  public void resolvesImplementingInterfaces() {
    assertThat(ProxyFactory.resolveInterfaces(mock(Golfer.class)))
      .contains(Auditable.class, Comparable.class, Identifiable.class, Serializable.class);
  }

  @Test
  public void resolvesImplementingAndProvidedInterfaces() {
    assertThat(ProxyFactory.resolveInterfaces(mock(Golfer.class), Runnable.class))
      .contains(Auditable.class, Comparable.class, Identifiable.class, Runnable.class, Serializable.class);
  }

  @Test
  public void resolvesNoInterfaces() {
    assertThat(ProxyFactory.resolveInterfaces(Contact.newContact("John Blum"))).isEmpty();
    assertThat(ProxyFactory.resolveInterfaces(new Object())).isEmpty();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void adviseWithMethodInterceptors() {
    MethodInterceptor mockMethodInterceptorOne = mock(MethodInterceptor.class);
    MethodInterceptor mockMethodInterceptorTwo = mock(MethodInterceptor.class);

    ProxyFactory<Object> proxyFactory = newProxyFactory();

    assertThat(proxyFactory).isNotNull();
    assertThat(proxyFactory.getMethodInterceptors()).isEmpty();
    assertThat(proxyFactory.adviseWith(mockMethodInterceptorOne, mockMethodInterceptorTwo)).isSameAs(proxyFactory);
    assertThat(proxyFactory.getMethodInterceptors()).contains(mockMethodInterceptorOne, mockMethodInterceptorTwo);
  }

  @Test
  public void adviseWithNullMethodInterceptors() {
    ProxyFactory<Object> proxyFactory = newProxyFactory();

    assertThat(proxyFactory).isNotNull();
    assertThat(proxyFactory.getMethodInterceptors()).isEmpty();
    assertThat(proxyFactory.adviseWith((MethodInterceptor<Object>[]) null)).isSameAs(proxyFactory);
    assertThat(proxyFactory.getMethodInterceptors()).isEmpty();
  }

  @Test
  public void implementingInterfaces() {
    ProxyFactory<Object> proxyFactory = newProxyFactory();

    assertThat(proxyFactory).isNotNull();
    assertThat(proxyFactory.getProxyInterfaces()).isEmpty();
    assertThat(proxyFactory.implementing(Auditable.class, Identifiable.class)).isSameAs(proxyFactory);
    assertThat(proxyFactory.getProxyInterfaces()).contains(Auditable.class, Identifiable.class);
  }

  @Test
  public void implementingNoInterfaces() {
    ProxyFactory<Object> proxyFactory = newProxyFactory(target, Auditable.class, Identifiable.class);

    assertThat(proxyFactory).isNotNull();
    assertThat(proxyFactory.getProxyInterfaces()).contains(Auditable.class, Identifiable.class);
    assertThat(proxyFactory.getTarget()).isSameAs(target);
    assertThat(proxyFactory.implementing((Class[]) null)).isSameAs(proxyFactory);
    assertThat(proxyFactory.getProxyInterfaces()).isEmpty();
    assertThat(proxyFactory.getTarget()).isSameAs(target);
  }

  @Test
  public void proxyTarget() {
    ProxyFactory<Object> proxyFactory = newProxyFactory();

    assertThat(proxyFactory).isNotNull();
    assertThat(proxyFactory.getTarget()).isNull();
    assertThat(proxyFactory.proxy(target)).isSameAs(proxyFactory);
    assertThat(proxyFactory.getTarget()).isSameAs(target);
  }

  @Test
  public void proxyNull() {
    ProxyFactory<Object> proxyFactory = newProxyFactory(target);

    assertThat(proxyFactory).isNotNull();
    assertThat(proxyFactory.getTarget()).isSameAs(target);
    assertThat(proxyFactory.proxy(null)).isSameAs(proxyFactory);
    assertThat(proxyFactory.getTarget()).isNull();
  }

  @Test
  public void usingSystemClassLoader() {
    ProxyFactory<Object> proxyFactory = newProxyFactory();

    assertThat(proxyFactory).isNotNull();
    assertThat(proxyFactory.getProxyClassLoader()).isEqualTo(Thread.currentThread().getContextClassLoader());
    assertThat(proxyFactory.using(ClassLoader.getSystemClassLoader())).isSameAs(proxyFactory);
    assertThat(proxyFactory.getProxyClassLoader()).isEqualTo(ClassLoader.getSystemClassLoader());
  }

  @Test
  public void usingThreadContextClassLoader() {
    ProxyFactory<Object> proxyFactory = newProxyFactory();

    assertThat(proxyFactory).isNotNull();
    assertThat(proxyFactory.using(null)).isSameAs(proxyFactory);
    assertThat(proxyFactory.getProxyClassLoader()).isEqualTo(Thread.currentThread().getContextClassLoader());
  }

  abstract class Entity implements Auditable {
  }

  abstract class Person extends Entity implements Comparable<Person>, Serializable {
  }

  abstract class Golfer extends Person {
  }

  @Data
  @RequiredArgsConstructor(staticName = "newContact")
  static class Contact {
    @NonNull String name;
  }
}
