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
package org.cp.elements.data.struct;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Optional;

import org.junit.Test;

/**
 * Unit Tests for {@link SystemPropertyValue}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.cp.elements.data.struct.SystemPropertyValue
 * @since 1.0.0
 */
public class SystemPropertyValueUnitTests {

  @Test
  public void newSystemPropertyValueIsSuccessful() {

    SystemPropertyValue systemPropertyValue = SystemPropertyValue.newSystemPropertyValue("user.name");

    assertThat(systemPropertyValue).isNotNull();
    assertThat(systemPropertyValue.getKey()).isEqualTo("user.name");
    assertThat(systemPropertyValue.getValue()).isInstanceOf(Optional.class);
    assertThat(systemPropertyValue.getValue()).isPresent();
    assertThat(systemPropertyValue.getValue("default")).isNotEqualTo("default");
  }

  @Test
  public void getValueForExistingSystemProperty() {

    System.getProperties().stringPropertyNames().forEach(propertyName ->
      assertThat(SystemPropertyValue.newSystemPropertyValue(propertyName).getValue("default"))
        .isEqualTo(System.getProperty(propertyName)));
  }

  @Test
  public void getValueForNonExistingSystemProperty() {

    SystemPropertyValue systemPropertyValue = SystemPropertyValue.newSystemPropertyValue("non.existing.property");

    assertThat(systemPropertyValue.getValue()).isInstanceOf(Optional.class);
    assertThat(systemPropertyValue.getValue("default"))
      .isEqualTo(System.getProperty("non.existing.property", "default"));
  }
}
