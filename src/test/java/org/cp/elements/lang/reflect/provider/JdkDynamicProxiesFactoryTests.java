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

package org.cp.elements.lang.reflect.provider;

import static org.assertj.core.api.Assertions.assertThat;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newUnsupportedOperationException;
import static org.cp.elements.lang.reflect.provider.JdkDynamicProxiesFactory.newJdkDynamicProxiesFactory;
import static org.cp.elements.lang.reflect.support.MethodInvokingMethodInterceptor.newMethodInvokingMethodInterceptor;

import java.io.Serializable;
import java.math.BigDecimal;
import java.math.BigInteger;

import org.cp.elements.lang.Constants;
import org.cp.elements.lang.Identifiable;
import org.cp.elements.lang.support.IdentifiableAdapter;
import org.junit.Test;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;

/**
 * Unit tests for {@link JdkDynamicProxiesFactory}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see lombok
 * @see org.cp.elements.lang.reflect.provider.JdkDynamicProxiesFactory
 * @since 1.0.0
 */
public class JdkDynamicProxiesFactoryTests {

  @SuppressWarnings("all")
  protected Golfer newGolfer(Long id) {
    Golfer golfer = Golfer.newGolfer();
    golfer.setId(id);
    return golfer;
  }

  @Test
  public void canProxyClassExtendingClassImplementingInterfaceIsTrue() {
    assertThat(newJdkDynamicProxiesFactory().canProxy(new Golfer(), Serializable.class)).isTrue();
  }

  @Test
  public void canProxyClassImplementingInterfaceIsTrue() {
    assertThat(newJdkDynamicProxiesFactory().canProxy(new ContactRepositorySupport() {})).isTrue();
  }

  @Test
  public void canProxyClassWithInterfaceIsTrue() {
    assertThat(newJdkDynamicProxiesFactory().canProxy(Contact.newContact("John Blum"), Comparable.class)).isTrue();
  }

  @Test
  public void canProxyInterfaceIsTrue() {
    assertThat(newJdkDynamicProxiesFactory().canProxy(new Object(), ContactRepository.class)).isTrue();
  }

  @Test
  public void cannotProxyClass() {
    assertThat(newJdkDynamicProxiesFactory().canProxy(Contact.newContact("John Blum"))).isFalse();
  }

  @Test
  public void cannotProxyClassOnlyImplementingSerializableInterface() {
    assertThat(newJdkDynamicProxiesFactory().canProxy(Contact.newContact("John Blum"), Serializable.class)).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void cannotProxyJavaTypes() {
    JdkDynamicProxiesFactory<?> proxyFactory = newJdkDynamicProxiesFactory();

    assertThat(proxyFactory.canProxy(new BigDecimal(3.14159d), Identifiable.class)).isFalse();
    assertThat(proxyFactory.canProxy(new BigInteger("42"), Identifiable.class)).isFalse();
    assertThat(proxyFactory.canProxy(true, Identifiable.class)).isFalse();
    assertThat(proxyFactory.canProxy(Boolean.TRUE, Identifiable.class)).isFalse();
    assertThat(proxyFactory.canProxy((byte) 8, Identifiable.class)).isFalse();
    assertThat(proxyFactory.canProxy(Byte.valueOf((byte) 16), Identifiable.class)).isFalse();
    assertThat(proxyFactory.canProxy('x', Identifiable.class)).isFalse();
    assertThat(proxyFactory.canProxy(Character.valueOf('X'), Identifiable.class)).isFalse();
    assertThat(proxyFactory.canProxy(Math.PI, Identifiable.class)).isFalse();
    assertThat(proxyFactory.canProxy(Double.valueOf(Math.PI), Identifiable.class)).isFalse();
    assertThat(proxyFactory.canProxy(3.14159f, Identifiable.class)).isFalse();
    assertThat(proxyFactory.canProxy(Float.valueOf(3.14159f), Identifiable.class)).isFalse();
    assertThat(proxyFactory.canProxy(2, Identifiable.class)).isFalse();
    assertThat(proxyFactory.canProxy(1234567890L, Identifiable.class)).isFalse();
    assertThat(proxyFactory.canProxy(Long.valueOf(4500250125L), Identifiable.class)).isFalse();
    assertThat(proxyFactory.canProxy((short) 8192, Identifiable.class)).isFalse();
    assertThat(proxyFactory.canProxy(Short.valueOf((short) 16384), Identifiable.class)).isFalse();
    assertThat(proxyFactory.canProxy("test", Identifiable.class)).isFalse();
    assertThat(proxyFactory.canProxy(new Thread("test"), Identifiable.class)).isFalse();
    assertThat(proxyFactory.canProxy(new Throwable("test"), Identifiable.class)).isFalse();
  }

  @Test
  public void cannotProxyNull() {
    assertThat(newJdkDynamicProxiesFactory().canProxy(null, Comparable.class)).isTrue();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void newProxyIsSuccessful() {
    Golfer golfer = newGolfer(1L);

    Identifiable<Long> golferProxy = (Identifiable<Long>) newJdkDynamicProxiesFactory().proxy(golfer)
      .adviseWith(newMethodInvokingMethodInterceptor(golfer))
        .newProxy();

    assertThat(golferProxy).isNotNull();
    assertThat(golferProxy.getId()).isEqualTo(1L);
  }

  @Data
  @EqualsAndHashCode(callSuper = false)
  static class Person extends IdentifiableAdapter<Long> {
    Long id;
  }

  @RequiredArgsConstructor(staticName = "newGolfer")
  @SuppressWarnings("all")
  static class Golfer extends Person {
  }

  @Data
  @RequiredArgsConstructor(staticName = "newContact")
  static class Contact {
    @NonNull String name;
  }

  @SuppressWarnings("unused")
  interface ContactRepository {
    Contact findBy(Long id);
    Iterable<Contact> findBy(String name);
  }

  abstract class ContactRepositorySupport implements ContactRepository {

    @Override
    public Contact findBy(Long id) {
      throw newUnsupportedOperationException(Constants.NOT_IMPLEMENTED);
    }

    @Override
    public Iterable<Contact> findBy(String name) {
      throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
    }
  }
}
