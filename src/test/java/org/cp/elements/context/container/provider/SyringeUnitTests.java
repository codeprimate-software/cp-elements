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
package org.cp.elements.context.container.provider;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import org.junit.Test;

import org.cp.elements.context.configure.ConfigurationService;
import org.cp.elements.context.configure.ConfigurationServiceAware;
import org.cp.elements.data.conversion.ConversionService;
import org.cp.elements.data.conversion.ConversionServiceAware;
import org.cp.elements.lang.factory.AbstractObjectFactory;
import org.cp.elements.lang.factory.ObjectFactoryAware;

/**
 * Unit Tests for {@link Syringe}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.context.container.DependencyInjection
 * @see org.cp.elements.context.container.provider.Syringe
 * @since 1.0.0
 */
public class SyringeUnitTests {

  @Test
  public void injectsAllDependencies() {

    TestBean testBean = mock(TestBean.class);

    Syringe syringe = new Syringe();

    assertThat(syringe.inject(testBean)).isEqualTo(testBean);

    verify(testBean, times(1)).setConfigurationService(isA(ConfigurationService.class));
    verify(testBean, times(1)).setConversionService(isA(ConversionService.class));
    verify(testBean, times(1)).setObjectFactory(isA(AbstractObjectFactory.class));
    verifyNoMoreInteractions(testBean);
  }

  @Test
  public void injectsPartialDependencies() {

    TestBeanTwo testBean = mock(TestBeanTwo.class);

    Syringe syringe = new Syringe();

    assertThat(syringe.inject(testBean)).isEqualTo(testBean);

    verify(testBean, times(1)).setObjectFactory(isA(AbstractObjectFactory.class));
    verifyNoMoreInteractions(testBean);
  }

  @Test
  public void injectsZeroDependencies() {

    Object pojo = spy(new Object());

    Syringe syringe = new Syringe();

    assertThat(syringe.inject(pojo)).isEqualTo(pojo);

    verifyNoInteractions(pojo);
  }

  @Test
  public void injectIsNullSafe() {
    assertThat(new Syringe().<Object>inject(null)).isNull();
  }

  interface TestBean extends ConfigurationServiceAware, ConversionServiceAware, ObjectFactoryAware { }

  interface TestBeanTwo extends ObjectFactoryAware { }

}
