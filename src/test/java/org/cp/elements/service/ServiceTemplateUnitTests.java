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
package org.cp.elements.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Optional;

import org.junit.Test;

import org.cp.elements.context.configure.Configuration;
import org.cp.elements.context.configure.ConfigurationService;
import org.cp.elements.context.configure.provider.ElementsConfigurationService;
import org.cp.elements.context.container.DependencyInjection;
import org.cp.elements.context.container.provider.Syringe;
import org.cp.elements.data.caching.Cache;
import org.cp.elements.data.caching.CacheNotFoundException;
import org.cp.elements.data.caching.support.CachingTemplate;
import org.cp.elements.data.conversion.ConversionService;
import org.cp.elements.data.conversion.provider.SimpleConversionService;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.factory.ObjectFactory;
import org.cp.elements.lang.factory.provider.PrototypeObjectFactory;
import org.cp.elements.util.ArrayUtils;
import org.cp.elements.util.CollectionUtils;

import org.assertj.core.api.InstanceOfAssertFactories;

/**
 * Unit Tests for {@link ServiceTemplate}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.service.ServiceTemplate
 * @since 1.0.0
 */
public class ServiceTemplateUnitTests {

  @Test
  public void loadGenericCache() {

    ServiceTemplate<?> serviceTemplate = mock(ServiceTemplate.class);

    doCallRealMethod().when(serviceTemplate).getCache(any());

    Cache<String, Object> cache = serviceTemplate.getCache(null);

    assertThat(cache).isNotNull();
    assertThat(cache.getName()).isEqualTo(null);

    verify(serviceTemplate, times(1)).getCache(isNull());
    verifyNoMoreInteractions(serviceTemplate);
  }

  @Test
  public void loadMockCache() {

    ServiceTemplate<?> serviceTemplate = mock(ServiceTemplate.class);

    doCallRealMethod().when(serviceTemplate).getCache(anyString());

    Cache<Integer, Object> cache = serviceTemplate.getCache("MockCache");

    assertThat(cache).isNotNull();
    assertThat(cache.getName()).isEqualTo("MockCache");

    verify(serviceTemplate, times(1)).getCache(eq("MockCache"));
    verifyNoMoreInteractions(serviceTemplate);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void loadMockCachingTemplate() {

    Cache<Integer, Object> mockCache = mock(Cache.class);

    ServiceTemplate<?> serviceTemplate = mock(ServiceTemplate.class);

    doReturn(mockCache).when(serviceTemplate).getCache(anyString());
    doCallRealMethod().when(serviceTemplate).getCachingTemplate(anyString());

    CachingTemplate<Integer, Object> cacheTemplate = serviceTemplate.getCachingTemplate("MockCache");

    assertThat(cacheTemplate).isNotNull();
    assertThat(ObjectUtils.invoke(cacheTemplate, "getCache", Cache.class)).isEqualTo(mockCache);

    verify(serviceTemplate, times(1)).getCachingTemplate(eq("MockCache"));
    verify(serviceTemplate, times(1)).getCache(eq("MockCache"));
    verifyNoMoreInteractions(serviceTemplate);
    verifyNoInteractions(mockCache);
  }

  @Test
  public void loadNonExistingNamedCache() {

    ServiceTemplate<?> serviceTemplate = mock(ServiceTemplate.class);

    doCallRealMethod().when(serviceTemplate).getCache(any());

    assertThatExceptionOfType(CacheNotFoundException.class)
      .isThrownBy(() -> serviceTemplate.getCache("NonExistingCache"))
      .withMessage("Cache with name [NonExistingCache] not found")
      .withNoCause();
  }

  @Test
  public void loadConfigurationByName() {

    Configuration mockConfiguration = mock(Configuration.class);

    ConfigurationService mockConfigurationService = mock(ConfigurationService.class);

    ServiceTemplate<?> serviceTemplate = mock(ServiceTemplate.class);

    doCallRealMethod().when(serviceTemplate).getConfiguration(anyString());
    doReturn(Optional.of(mockConfigurationService)).when(serviceTemplate).getConfigurationService();
    doReturn(ArrayUtils.asIterable(null, null, mockConfiguration, null).spliterator())
      .when(mockConfigurationService).spliterator();
    doReturn("MockConfiguration").when(mockConfiguration).getName();

    Configuration configuration = serviceTemplate.getConfiguration("MockConfiguration").orElse(null);

    assertThat(configuration).isEqualTo(mockConfiguration);

    verify(serviceTemplate, times(1)).getConfiguration(eq("MockConfiguration"));
    verify(serviceTemplate, times(1)).getConfigurationService();
    verify(mockConfigurationService, times(1)).spliterator();
    verify(mockConfiguration, times(1)).getName();
    verifyNoMoreInteractions(serviceTemplate, mockConfigurationService, mockConfiguration);
  }

  @Test
  public void loadNonExistingConfigurationByName() {

    ConfigurationService mockConfigurationService = mock(ConfigurationService.class);

    ServiceTemplate<?> serviceTemplate = mock(ServiceTemplate.class);

    doCallRealMethod().when(serviceTemplate).getConfiguration(anyString());
    doReturn(Optional.of(mockConfigurationService)).when(serviceTemplate).getConfigurationService();
    doReturn(CollectionUtils.emptyIterable().spliterator()).when(mockConfigurationService).spliterator();

    Configuration configuration = serviceTemplate.getConfiguration("TestConfiguration").orElse(null);

    assertThat(configuration).isNull();

    verify(serviceTemplate, times(1)).getConfiguration(eq("TestConfiguration"));
    verify(serviceTemplate, times(1)).getConfigurationService();
    verify(mockConfigurationService, times(1)).spliterator();
    verifyNoMoreInteractions(serviceTemplate, mockConfigurationService);
  }

  @Test
  public void loadsConfigurationService() {

    ServiceTemplate<?> serviceTemplate = mock(ServiceTemplate.class);

    doCallRealMethod().when(serviceTemplate).getConfigurationService();

    ConfigurationService configurationService = serviceTemplate.getConfigurationService().orElse(null);

    assertThat(configurationService).isInstanceOf(ElementsConfigurationService.class);

    verify(serviceTemplate, times(1)).getConfigurationService();
    verifyNoMoreInteractions(serviceTemplate);
  }

  @Test
  public void loadsConversionService() {

    ServiceTemplate<?> serviceTemplate = mock(ServiceTemplate.class);

    doCallRealMethod().when(serviceTemplate).getConversionService();

    ConversionService conversionService = serviceTemplate.getConversionService().orElse(null);

    assertThat(conversionService).isInstanceOf(SimpleConversionService.class);

    verify(serviceTemplate, times(1)).getConversionService();
    verifyNoMoreInteractions(serviceTemplate);
  }

  @Test
  public void loadsDependencyInjectionContainer() {

    ServiceTemplate<?> serviceTemplate = mock(ServiceTemplate.class);

    doCallRealMethod().when(serviceTemplate).getDependencyInjectionContainer();

    DependencyInjection dependencyInjectionContainer =
      serviceTemplate.getDependencyInjectionContainer().orElse(null);

    assertThat(dependencyInjectionContainer).isInstanceOf(Syringe.class);

    verify(serviceTemplate, times(1)).getDependencyInjectionContainer();
    verifyNoMoreInteractions(serviceTemplate);
  }

  @Test
  public void loadObjectFactory() {

    Configuration mockConfiguration = mock(Configuration.class);

    ConfigurationService mockConfigurationService = mock(ConfigurationService.class);

    doReturn(ArrayUtils.asIterator(mockConfiguration)).when(mockConfigurationService).iterator();
    doCallRealMethod().when(mockConfigurationService).asConfiguration();

    ConversionService mockConversionService = mock(ConversionService.class);

    ServiceTemplate<?> serviceTemplate = mock(ServiceTemplate.class);

    doReturn(Optional.of(mockConfigurationService)).when(serviceTemplate).getConfigurationService();
    doReturn(Optional.of(mockConversionService)).when(serviceTemplate).getConversionService();
    doCallRealMethod().when(serviceTemplate).getObjectFactory();

    Optional<ObjectFactory> objectFactory = serviceTemplate.getObjectFactory();

    assertThat(objectFactory).isNotNull();
    assertThat(objectFactory).isPresent();

    assertThat(objectFactory.orElse(null)).isInstanceOf(PrototypeObjectFactory.class);

    assertThat(objectFactory.orElse(null))
      .asInstanceOf(InstanceOfAssertFactories.type(PrototypeObjectFactory.class))
      .extracting(prototypeObjectFactory ->
        ObjectUtils.invoke(prototypeObjectFactory, "getConfiguration", Configuration.class))
      .isSameAs(mockConfiguration);

    assertThat(objectFactory.orElse(null))
      .asInstanceOf(InstanceOfAssertFactories.type(PrototypeObjectFactory.class))
      .extracting(prototypeObjectFactory ->
        ObjectUtils.invoke(prototypeObjectFactory, "getConversionService", ConversionService.class))
      .isSameAs(mockConversionService);

    verify(mockConfigurationService, times(1)).asConfiguration();
    verify(mockConfigurationService, times(1)).iterator();
    verifyNoMoreInteractions(mockConfigurationService);
    verifyNoInteractions(mockConfiguration, mockConversionService);
  }
}
