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
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verifyNoInteractions;

import java.io.Serializable;

import org.junit.Test;

import org.cp.elements.lang.Auditable;
import org.cp.elements.lang.Describable;
import org.cp.elements.lang.Identifiable;
import org.cp.elements.lang.Nameable;
import org.cp.elements.test.annotation.SubjectUnderTest;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.ToString;

/**
 * Unit Tests for {@link ProxyFactory}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.reflect.ProxyFactory
 * @since 1.0.0
 */
public class ProxyFactoryUnitTests {

  @SubjectUnderTest
  private final Object target = new Object();

  @Test
  public void newProxyFactoryIsCorrect() {

    ProxyFactory<Object> proxyFactory = ProxyFactory.newProxyFactory();

    assertThat(proxyFactory).isNotNull();
    assertThat(proxyFactory.getMethodInterceptors()).isEmpty();
    assertThat(proxyFactory.getProxyClassLoader()).isEqualTo(Thread.currentThread().getContextClassLoader());
    assertThat(proxyFactory.getProxyInterfaces()).isEmpty();
    assertThat(proxyFactory.getTarget()).isNull();
  }

  @Test
  public void newProxyFactoryForTargetObjectAndInterfaces() {

    Contact johnBlum = Contact.newContact("John Blum");

    ProxyFactory<Object> proxyFactory = ProxyFactory.newProxyFactory(johnBlum,
      Auditable.class, Describable.class, Identifiable.class, Nameable.class, Runnable.class);

    assertThat(proxyFactory).isNotNull();
    assertThat(proxyFactory.getMethodInterceptors()).isEmpty();
    assertThat(proxyFactory.getProxyClassLoader()).isEqualTo(Thread.currentThread().getContextClassLoader());
    assertThat(proxyFactory.getProxyInterfaces())
      .containsExactlyInAnyOrder(Auditable.class, Describable.class, Identifiable.class, Nameable.class, Runnable.class);
    assertThat(proxyFactory.getTarget()).isEqualTo(johnBlum);
  }

  @Test
  public void resolvesGivenInterfaces() {
    assertThat(ProxyFactory.resolveInterfaces(Contact.newContact("John Blum"), Auditable.class, Identifiable.class))
      .containsExactlyInAnyOrder(Auditable.class, Identifiable.class);
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

    MethodInterceptor<Object> mockMethodInterceptorOne = mock(MethodInterceptor.class);
    MethodInterceptor<Object> mockMethodInterceptorTwo = mock(MethodInterceptor.class);

    ProxyFactory<Object> proxyFactory = ProxyFactory.newProxyFactory();

    assertThat(proxyFactory).isNotNull();
    assertThat(proxyFactory.getMethodInterceptors()).isEmpty();
    assertThat(proxyFactory.adviseWith(mockMethodInterceptorOne, mockMethodInterceptorTwo)).isSameAs(proxyFactory);
    assertThat(proxyFactory.getMethodInterceptors())
      .containsExactlyInAnyOrder(mockMethodInterceptorOne, mockMethodInterceptorTwo);

    verifyNoInteractions(mockMethodInterceptorOne, mockMethodInterceptorTwo);
  }

  @Test
  public void adviseWithNullMethodInterceptors() {

    ProxyFactory<Object> proxyFactory = ProxyFactory.newProxyFactory();

    assertThat(proxyFactory).isNotNull();
    assertThat(proxyFactory.getMethodInterceptors()).isEmpty();
    assertThat(proxyFactory.adviseWith((MethodInterceptor<Object>[]) null)).isSameAs(proxyFactory);
    assertThat(proxyFactory.getMethodInterceptors()).isEmpty();
  }

  @Test
  public void implementingInterfaces() {

    ProxyFactory<Object> proxyFactory = ProxyFactory.newProxyFactory();

    assertThat(proxyFactory).isNotNull();
    assertThat(proxyFactory.getProxyInterfaces()).isEmpty();
    assertThat(proxyFactory.implementing(Auditable.class, Nameable.class)).isSameAs(proxyFactory);
    assertThat(proxyFactory.getProxyInterfaces()).containsExactlyInAnyOrder(Auditable.class, Nameable.class);
  }

  @Test
  @SuppressWarnings("rawtypes")
  public void implementingNoInterfaces() {

    ProxyFactory<Object> proxyFactory = ProxyFactory.newProxyFactory(this.target, Auditable.class, Nameable.class);

    assertThat(proxyFactory).isNotNull();
    assertThat(proxyFactory.getProxyInterfaces()).containsExactlyInAnyOrder(Auditable.class, Nameable.class);
    assertThat(proxyFactory.getTarget()).isSameAs(this.target);
    assertThat(proxyFactory.implementing((Class[]) null)).isSameAs(proxyFactory);
    assertThat(proxyFactory.getProxyInterfaces()).isEmpty();
    assertThat(proxyFactory.getTarget()).isSameAs(this.target);
  }

  @Test
  public void proxyTarget() {

    ProxyFactory<Object> proxyFactory = ProxyFactory.newProxyFactory();

    assertThat(proxyFactory).isNotNull();
    assertThat(proxyFactory.getTarget()).isNull();
    assertThat(proxyFactory.proxy(this.target)).isSameAs(proxyFactory);
    assertThat(proxyFactory.getTarget()).isSameAs(this.target);
  }

  @Test
  public void proxyNull() {

    ProxyFactory<Object> proxyFactory = ProxyFactory.newProxyFactory(this.target);

    assertThat(proxyFactory).isNotNull();
    assertThat(proxyFactory.getTarget()).isSameAs(this.target);
    assertThat(proxyFactory.proxy(null)).isSameAs(proxyFactory);
    assertThat(proxyFactory.getTarget()).isNull();
  }

  @Test
  public void usingSystemClassLoader() {

    ProxyFactory<Object> proxyFactory = ProxyFactory.newProxyFactory();

    assertThat(proxyFactory).isNotNull();
    assertThat(proxyFactory.getProxyClassLoader()).isEqualTo(Thread.currentThread().getContextClassLoader());
    assertThat(proxyFactory.using(ClassLoader.getSystemClassLoader())).isSameAs(proxyFactory);
    assertThat(proxyFactory.getProxyClassLoader()).isEqualTo(ClassLoader.getSystemClassLoader());
  }

  @Test
  public void usingThreadContextClassLoader() {

    ProxyFactory<Object> proxyFactory = ProxyFactory.newProxyFactory();

    assertThat(proxyFactory).isNotNull();
    assertThat(proxyFactory.getProxyClassLoader()).isEqualTo(Thread.currentThread().getContextClassLoader());
    assertThat(proxyFactory.using(null)).isSameAs(proxyFactory);
    assertThat(proxyFactory.getProxyClassLoader()).isEqualTo(Thread.currentThread().getContextClassLoader());
  }

  @SuppressWarnings("rawtypes")
  static abstract class Entity implements Auditable { }

  static abstract class Person extends Entity implements Comparable<Person>, Serializable { }

  static abstract class Golfer extends Person { }

  @Getter
  @ToString(of = "name")
  @EqualsAndHashCode(of = "name")
  @RequiredArgsConstructor(staticName = "newContact")
  static class Contact {

    @lombok.NonNull
    private final String name;

  }
}
