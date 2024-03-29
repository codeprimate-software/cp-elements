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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Iterator;
import java.util.Optional;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.reflect.provider.JdkDynamicProxiesFactory;
import org.cp.elements.util.ArrayUtils;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.ToString;

/**
 * Unit Tests for {@link ProxyService}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.reflect.ProxyFactory
 * @see org.cp.elements.lang.reflect.ProxyService
 * @see org.cp.elements.lang.reflect.provider.JdkDynamicProxiesFactory
 * @since 1.0.0
 */
public class ProxyServiceUnitTests {

  @Test
  @SuppressWarnings("rawtypes")
  public void newProxyServiceIsSuccessful() {

    ProxyService<?> proxyService = ProxyService.newProxyService();

    assertThat(proxyService).isNotNull();
    assertThat(ProxyService.newProxyService()).isSameAs(proxyService);

    Iterator<ProxyFactory> iterator = proxyService.iterator();

    assertThat(iterator).isNotNull();
    assertThat(iterator.hasNext()).isTrue();
    assertThat(iterator.next()).isInstanceOf(JdkDynamicProxiesFactory.class);
  }

  @Test
  public void cannotProxyClass() {

    Contact johnBlum = Contact.newContact("John Blum");

    assertThat(ProxyService.newProxyService().canProxy(johnBlum)).isFalse();
  }

  @Test
  public void canProxyInterface() {
    assertThat(ProxyService.newProxyService().canProxy(new Object(), ContactRepository.class)).isTrue();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void findFirstProxyFactoryIsSuccessful() {

    ProxyFactory<Object> mockProxyFactoryOne = mock(ProxyFactory.class);
    ProxyFactory<Object> mockProxyFactoryTwo = mock(ProxyFactory.class);

    doReturn(false).when(mockProxyFactoryOne).canProxy(any(), any());
    doReturn(true).when(mockProxyFactoryTwo).canProxy(any(), any());

    ProxyService<Object> proxyService = spy(ProxyService.newProxyService());

    doReturn(ArrayUtils.asIterable(mockProxyFactoryOne, mockProxyFactoryTwo).spliterator())
      .when(proxyService).spliterator();

    Optional<ProxyFactory<Object>> proxyFactory = proxyService.findFirstProxyFactory(null, Runnable.class);

    assertThat(proxyFactory).isNotNull();
    assertThat(proxyFactory).isNotEmpty();
    assertThat(proxyFactory.orElse(null)).isEqualTo(mockProxyFactoryTwo);

    verify(proxyService, times(1)).findFirstProxyFactory(eq(null), eq(Runnable.class));
    verify(proxyService, times(1)).spliterator();
    verify(mockProxyFactoryOne, times(1)).canProxy(eq(null), eq(Runnable.class));
    verify(mockProxyFactoryTwo, times(1)).canProxy(eq(null), eq(Runnable.class));
    verifyNoMoreInteractions(mockProxyFactoryOne, mockProxyFactoryTwo, proxyService);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void findsNoProxyFactory() {

    ProxyFactory<Object> mockProxyFactoryOne = mock(ProxyFactory.class);
    ProxyFactory<Object> mockProxyFactoryTwo = mock(ProxyFactory.class);

    doReturn(false).when(mockProxyFactoryOne).canProxy(any(), any());
    doReturn(false).when(mockProxyFactoryTwo).canProxy(any(), any());

    ProxyService<Object> proxyService = spy(ProxyService.newProxyService());

    doReturn(ArrayUtils.asIterable(mockProxyFactoryOne, mockProxyFactoryTwo).spliterator())
      .when(proxyService).spliterator();

    Optional<ProxyFactory<Object>> proxyFactory = proxyService.findFirstProxyFactory(null, Runnable.class);

    assertThat(proxyFactory).isNotNull();
    assertThat(proxyFactory).isEmpty();

    verify(proxyService, times(1)).findFirstProxyFactory(eq(null), eq(Runnable.class));
    verify(proxyService, times(1)).spliterator();
    verify(mockProxyFactoryOne, times(1)).canProxy(eq(null), eq(Runnable.class));
    verify(mockProxyFactoryTwo, times(1)).canProxy(eq(null), eq(Runnable.class));
    verifyNoMoreInteractions(mockProxyFactoryOne, mockProxyFactoryTwo, proxyService);
  }

  @Test
  public void reloadIsSuccessful() {
    ProxyService.newProxyService().reload();
  }

  @Getter
  @ToString(of = "name")
  @EqualsAndHashCode(of = "name")
  @RequiredArgsConstructor(staticName = "newContact")
  static class Contact {

    @lombok.NonNull
    private final String name;

  }

  @SuppressWarnings("unused")
  interface ContactRepository {
    Contact findBy(Long id);
    Iterable<Contact> findBy(String name);
  }
}
