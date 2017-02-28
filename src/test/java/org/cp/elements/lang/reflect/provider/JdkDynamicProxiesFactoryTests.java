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

package org.cp.elements.lang.reflect.provider;

import static org.assertj.core.api.Assertions.assertThat;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newUnsupportedOperationException;
import static org.cp.elements.lang.reflect.provider.JdkDynamicProxiesFactory.newJdkDynamicProxiesFactory;
import static org.cp.elements.lang.reflect.support.MethodInvokingMethodInterceptor.newMethodInvokingMethodInterceptor;

import java.io.Serializable;

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
    assertThat(newJdkDynamicProxiesFactory().canProxy(null, ContactRepository.class)).isTrue();
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
  public void newProxyIsSuccessful() {
    Golfer golfer = newGolfer(1L);

    Identifiable<Long> golferProxy = newJdkDynamicProxiesFactory().proxy(golfer)
      .adviseWith(newMethodInvokingMethodInterceptor(golfer))
        .newProxy();

    assertThat(golferProxy).isNotNull();
    assertThat(golferProxy.getId()).isEqualTo(1l);
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
