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
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.mock;

import org.cp.elements.data.caching.Cache;
import org.cp.elements.data.caching.CacheNotFoundException;
import org.cp.elements.data.conversion.ConversionService;

import org.junit.Test;

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
  }

  @Test
  public void loadNamedCache() {

    ServiceTemplate<?> serviceTemplate = mock(ServiceTemplate.class);

    doCallRealMethod().when(serviceTemplate).getCache(any());

    assertThatExceptionOfType(CacheNotFoundException.class)
      .isThrownBy(() -> serviceTemplate.getCache("nonExistingCache"))
      .withMessage("Cache with name [nonExistingCache] not found")
      .withNoCause();
  }

  @Test
  public void loadsConversionService() {

    ServiceTemplate<?> serviceTemplate = mock(ServiceTemplate.class);

    doCallRealMethod().when(serviceTemplate).getConversionService();

    ConversionService conversionService = serviceTemplate.getConversionService().orElse(null);

    assertThat(conversionService).isNotNull();
  }
}
