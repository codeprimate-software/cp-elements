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
package org.cp.elements.context.configure.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import org.junit.Test;

import org.cp.elements.context.annotation.Profile;
import org.cp.elements.context.configure.Configuration;
import org.cp.elements.util.ArrayUtils;

/**
 * Unit Tests for {@link DelegatingConfiguration}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.context.configure.Configuration
 * @see org.cp.elements.context.configure.support.DelegatingConfiguration
 * @since 1.0.0
 */
public class DelegatingConfigurationUnitTests {

  @Test
  public void constructDelegatingConfiguration() {

    Configuration mockConfiguration = mock(Configuration.class);

    DelegatingConfiguration delegatingConfiguration = new DelegatingConfiguration(mockConfiguration);

    assertThat(delegatingConfiguration).isNotNull();
    assertThat(delegatingConfiguration.getDelegate()).isSameAs(mockConfiguration);

    verifyNoInteractions(mockConfiguration);
  }

  @Test
  public void constructDelegatingConfigurationWithNull() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new DelegatingConfiguration(null))
      .withMessage("Configuration to be used as the delegate is required")
      .withNoCause();
  }

  @Test
  public void getDescriptorCallsDelegate() {

    Configuration mockConfiguration = mock(Configuration.class);

    Configuration.Descriptor<?> mockConfigurationDescriptor = mock(Configuration.Descriptor.class);

    doReturn(mockConfigurationDescriptor).when(mockConfiguration).getDescriptor();

    DelegatingConfiguration delegatingConfiguration = new DelegatingConfiguration(mockConfiguration);

    assertThat(delegatingConfiguration.getDelegate()).isSameAs(mockConfiguration);
    assertThat(delegatingConfiguration.getDescriptor()).isEqualTo(mockConfigurationDescriptor);

    verify(mockConfiguration, times(1)).getDescriptor();
    verifyNoMoreInteractions(mockConfiguration);
    verifyNoInteractions(mockConfigurationDescriptor);
  }

  @Test
  public void getNameCallsDelegate() {

    TestConfiguration testConfiguration = spy(new TestConfiguration());

    DelegatingConfiguration delegatingConfiguration = new DelegatingConfiguration(testConfiguration);

    assertThat(delegatingConfiguration.getDelegate()).isSameAs(testConfiguration);
    assertThat(delegatingConfiguration.getName())
      .isEqualTo(testConfiguration.getName().concat(DelegatingConfiguration.DELEGATE_NAME_SUFFIX));

    verify(testConfiguration, atLeastOnce()).getName();
    verifyNoMoreInteractions(testConfiguration);
  }

  @Test
  public void getProfilesCallsDelegate() {

    TestConfiguration testConfiguration = spy(new TestConfiguration());

    DelegatingConfiguration delegatingConfiguration = new DelegatingConfiguration(testConfiguration);

    assertThat(delegatingConfiguration.getDelegate()).isSameAs(testConfiguration);
    assertThat(delegatingConfiguration.getProfiles()).containsExactly("DEV", "QA");

    verify(testConfiguration, times(1)).getProfiles();
    verifyNoMoreInteractions(testConfiguration);
  }

  @Test
  public void getPropertyValueCallsDelegate() {

    Configuration mockConfiguration = mock(Configuration.class);

    doReturn("test").when(mockConfiguration).getPropertyValue(anyString(), anyBoolean());

    DelegatingConfiguration delegatingConfiguration = new DelegatingConfiguration(mockConfiguration);

    assertThat(delegatingConfiguration.getDelegate()).isEqualTo(mockConfiguration);
    assertThat(delegatingConfiguration.getPropertyValue("mockProperty")).isEqualTo("test");

    verify(mockConfiguration, times(1))
      .getPropertyValue(eq("mockProperty"), eq(Configuration.REQUIRED));
    verifyNoMoreInteractions(mockConfiguration);
  }

  @Test
  public void iteratorCallsDelegate() {

    Configuration mockConfiguration = mock(Configuration.class);

    doReturn(ArrayUtils.asIterator("propertyOne", "propertyTwo")).when(mockConfiguration).iterator();

    DelegatingConfiguration delegatingConfiguration = new DelegatingConfiguration(mockConfiguration);

    assertThat(delegatingConfiguration.getDelegate()).isEqualTo(mockConfiguration);
    assertThat(ArrayUtils.asArray(delegatingConfiguration.iterator(), String.class))
      .containsExactly("propertyOne", "propertyTwo");

    verify(mockConfiguration, times(1)).iterator();
    verifyNoMoreInteractions(mockConfiguration);
  }

  @Profile(names = { "DEV", "QA" })
  static class TestConfiguration implements Configuration {

    @Override
    public String getPropertyValue(String propertyName, boolean required) {
      return null;
    }
  }
}
