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

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link EnvironmentVariableValue}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.data.struct.EnvironmentVariableValue
 * @since 1.0.0
 */
public class EnvironmentVariableValueUnitTests {

  @Test
  public void newEnvironmentVariableValueIsSuccessful() {

    EnvironmentVariableValue environmentVariableValue = EnvironmentVariableValue.newEnvironmentVariableValue("USER");

    assertThat(environmentVariableValue).isNotNull();
    assertThat(environmentVariableValue.getKey()).isEqualTo("USER");
    assertThat(environmentVariableValue.getValue()).isInstanceOf(Optional.class);
    assertThat(environmentVariableValue.getValue()).isPresent();
    assertThat(environmentVariableValue.getValue("default")).isNotEqualTo("default");
  }

  @Test
  public void getValueForExistingEnvironmentVariable() {

    System.getenv().keySet().forEach(environmentVariable ->
      assertThat(EnvironmentVariableValue.newEnvironmentVariableValue(environmentVariable).getValue("default"))
        .isEqualTo(System.getenv(environmentVariable)));
  }

  @Test
  public void getValueForNonExistingEnvironmentVariable() {

    EnvironmentVariableValue environmentVariableValue =
      EnvironmentVariableValue.newEnvironmentVariableValue("non.existing.variable");

    assertThat(environmentVariableValue.getValue()).isInstanceOf(Optional.class);
    assertThat(environmentVariableValue.getValue()).isNotPresent();
    assertThat(environmentVariableValue.getValue("default")).isEqualTo("default");
  }
}
