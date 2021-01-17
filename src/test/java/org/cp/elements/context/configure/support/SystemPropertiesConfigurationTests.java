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
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.HashSet;
import java.util.Set;

import org.cp.elements.context.configure.Configuration;
import org.junit.Test;

/**
 * Unit Tests for {@link SystemPropertiesConfiguration}.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.context.configure.Configuration
 * @see org.cp.elements.context.configure.support.SystemPropertiesConfiguration
 * @since 1.0.0
 */
public class SystemPropertiesConfigurationTests {

  private final SystemPropertiesConfiguration configuration = new SystemPropertiesConfiguration();

  @Test
  public void isPresent() {

    assertThat(this.configuration.isPresent("java.class.path")).isTrue();
    assertThat(this.configuration.isPresent("java.home")).isTrue();
    assertThat(this.configuration.isPresent("java.version")).isTrue();
    assertThat(this.configuration.isPresent("user.dir")).isTrue();
    assertThat(this.configuration.isPresent("user.home")).isTrue();
    assertThat(this.configuration.isPresent("user.name")).isTrue();
    assertThat(this.configuration.isPresent("unset.system.property")).isFalse();
  }

  @Test
  public void doGetPropertyValue() {

    assertThat(this.configuration.doGetPropertyValue("java.class.path")).isEqualTo(System.getProperty("java.class.path"));
    assertThat(this.configuration.doGetPropertyValue("java.home")).isEqualTo(System.getProperty("java.home"));
    assertThat(this.configuration.doGetPropertyValue("java.version")).isEqualTo(System.getProperty("java.version"));
    assertThat(this.configuration.doGetPropertyValue("user.dir")).isEqualTo(System.getProperty("user.dir"));
    assertThat(this.configuration.doGetPropertyValue("user.home")).isEqualTo(System.getProperty("user.home"));
    assertThat(this.configuration.doGetPropertyValue("user.name")).isEqualTo(System.getProperty("user.name"));
  }

  @Test
  public void getParentPropertyValue() {

    Configuration mockParentConfiguration = mock(Configuration.class);

    when(mockParentConfiguration.getPropertyValue(eq("custom.system.property"), anyBoolean())).thenReturn("test");

    SystemPropertiesConfiguration configuration = new SystemPropertiesConfiguration(mockParentConfiguration);

    assertThat(configuration.getPropertyValue("java.version")).isEqualTo(System.getProperty("java.version"));
    assertThat(configuration.getPropertyValue("custom.system.property")).isEqualTo("test");
    assertThat(configuration.getPropertyValue("unset.system.property", false)).isNull();

    verify(mockParentConfiguration, times(1)).getPropertyValue(eq("custom.system.property"), eq(true));
    verify(mockParentConfiguration, times(1)).getPropertyValue(eq("unset.system.property"), anyBoolean());
  }

  @Test
  public void iterator() {

    Set<String> expectedSystemPropertyNames = new HashSet<>(System.getProperties().stringPropertyNames());

    assertThat(expectedSystemPropertyNames.isEmpty()).isFalse();

    for (String actualSystemPropertyName : this.configuration) {
      assertThat(expectedSystemPropertyNames.remove(actualSystemPropertyName)).isTrue();
    }

    assertThat(expectedSystemPropertyNames.isEmpty()).isTrue();
  }
}
