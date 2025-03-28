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
package org.cp.elements.context.configure.provider;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

import org.cp.elements.context.annotation.ActiveProfiles;

/**
 * Integration Tests for {@link SimpleConfigurationService}.
 *
 * @author John Blum
 * @see SimpleConfigurationService
 * @see org.junit.jupiter.api.Test
 * @since 1.0.0
 */
public class SimpleConfigurationServiceIntegrationTests {

  @Test
  public void usingDefaultNonProfiledConfiguration() {

    SimpleConfigurationService configurationService = new SimpleConfigurationService();

    assertThat(configurationService).isNotNull();
    assertThat(configurationService).hasSize(3);
    assertThat(configurationService.getActiveProfiles()).isEmpty();
    assertThat(configurationService.getPropertyValue("test.properties.mock-property")).isEqualTo("test");
    assertThat(configurationService.getPropertyValue("test.properties.overridden-property")).isEqualTo("base");
  }

  @Test
  public void usingDevProfiledConfiguration() {

    SimpleConfigurationService configurationService = new DevSimpleConfigurationService();

    assertThat(configurationService).isNotNull();
    assertThat(configurationService).hasSize(4);
    assertThat(configurationService.getActiveProfiles()).containsExactly("DEV");
    assertThat(configurationService.getPropertyValue("test.properties.mock-property")).isEqualTo("test");
    assertThat(configurationService.getPropertyValue("test.properties.overridden-property")).isEqualTo("dev");
  }

  @Test
  public void usingDevAndQaProfiledConfiguration() {

    SimpleConfigurationService configurationService = new DevAndQaSimpleConfigurationService();

    assertThat(configurationService).isNotNull();
    assertThat(configurationService).hasSize(5);
    assertThat(configurationService.getActiveProfiles()).containsExactly("DEV", "QA");
    assertThat(configurationService.getPropertyValue("test.properties.mock-property")).isEqualTo("test");
    assertThat(configurationService.getPropertyValue("test.properties.overridden-property")).isEqualTo("qa");
  }

  @ActiveProfiles(names = "DEV")
  static class DevSimpleConfigurationService extends SimpleConfigurationService { }

  @ActiveProfiles(names = { "DEV", "QA" })
  static class DevAndQaSimpleConfigurationService extends SimpleConfigurationService { }

}
