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
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.util.HashSet;
import java.util.Set;
import java.util.stream.StreamSupport;

import org.junit.Test;

import org.cp.elements.context.configure.Configuration;

/**
 * Unit Tests for {@link EnvironmentVariablesConfiguration}.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.cp.elements.context.configure.Configuration
 * @see org.cp.elements.context.configure.support.EnvironmentVariablesConfiguration
 * @since 1.0.0
 */
public class EnvironmentVariablesConfigurationUnitTests {

  private final EnvironmentVariablesConfiguration configuration = new EnvironmentVariablesConfiguration();

  @Test
  public void isPresentReturnsTrue() {

    assertThat(System.getenv()).containsKey("PATH");
    assertThat(System.getenv()).containsKey("USER");

    assertThat(this.configuration.isPresent("PATH")).isTrue();
    assertThat(this.configuration.isPresent("USER")).isTrue();
  }

  @Test
  public void isPresentReturnsFalse() {

    assertThat(System.getenv()).doesNotContainKey("NON_EXISTING_ENVIRONMENT_VARIABLE");
    assertThat(this.configuration.isPresent("NON_EXISTING_ENVIRONMENT_VARIABLE")).isFalse();
  }

  @Test
  public void doGetPropertyValueIsCorrect() {

    assertThat(this.configuration.doGetPropertyValue("HOST")).isEqualTo(System.getenv("HOST"));
    assertThat(this.configuration.doGetPropertyValue("PATH")).isEqualTo(System.getenv("PATH"));
    assertThat(this.configuration.doGetPropertyValue("USER")).isEqualTo(System.getenv("USER"));
  }

  @Test
  public void doGetNonExistingPropertyValueReturnsNull() {
    assertThat(this.configuration.doGetPropertyValue("NON_EXISTING_PROPERTY")).isNull();
  }

  @Test
  public void getParentPropertyValue() {

    Configuration mockParentConfiguration = mock(Configuration.class);

    when(mockParentConfiguration.getPropertyValue(eq("MOCK_ENVIRONMENT_VARIABLE"), eq(true))).thenReturn("test");

    EnvironmentVariablesConfiguration configuration = new EnvironmentVariablesConfiguration(mockParentConfiguration);

    assertThat(configuration.getPropertyValue("USER")).isEqualTo(System.getenv("USER"));
    assertThat(configuration.getPropertyValue("MOCK_ENVIRONMENT_VARIABLE")).isEqualTo("test");
    assertThat(configuration.getPropertyValue("TEST_ENVIRONMENT_VARIABLE", false)).isNull();

    verify(mockParentConfiguration, times(1))
      .getPropertyValue(eq("MOCK_ENVIRONMENT_VARIABLE"), eq(true));
    verify(mockParentConfiguration, times(1))
      .getPropertyValue(eq("TEST_ENVIRONMENT_VARIABLE"), eq(false));

    verifyNoMoreInteractions(mockParentConfiguration);
  }

  @Test
  public void iteratorIsCorrect() {

    Set<String> expectedEnvironmentVariableNames = new HashSet<>(System.getenv().keySet());

    assertThat(expectedEnvironmentVariableNames).isNotEmpty();

    StreamSupport.stream(this.configuration.spliterator(), false).forEach(actualEnvironmentVariableName ->
        assertThat(expectedEnvironmentVariableNames.remove(actualEnvironmentVariableName)).isTrue());

    assertThat(expectedEnvironmentVariableNames).isEmpty();
  }
}
