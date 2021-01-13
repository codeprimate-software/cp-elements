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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assume.assumeTrue;
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
 * Unit tests for {@link EnvironmentVariablesConfiguration}.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.cp.elements.context.configure.Configuration
 * @see org.cp.elements.context.configure.support.EnvironmentVariablesConfiguration
 * @since 1.0.0
 */
public class EnvironmentVariablesConfigurationTest {

  private final EnvironmentVariablesConfiguration configuration = new EnvironmentVariablesConfiguration();

  @Test
  public void testIsPresent() {

    assertTrue(System.getenv().containsKey("PATH"));
    assumeTrue(System.getenv().containsKey("USER"));
    assertTrue(configuration.isPresent("PATH"));
    assertTrue(configuration.isPresent("USER"));
    assertFalse(configuration.isPresent("UNSET_ENVIRONMENT_VARIABLE_NAME"));
  }

  @Test
  public void testDoGetPropertyValue() {

    assertEquals(System.getenv("HOST"), configuration.doGetPropertyValue("HOST"));
    assertEquals(System.getenv("PATH"), configuration.doGetPropertyValue("PATH"));
    assertEquals(System.getenv("USER"), configuration.doGetPropertyValue("USER"));
  }

  @Test
  public void getParentPropertyValue() {

    Configuration mockParentConfiguration = mock(Configuration.class);

    when(mockParentConfiguration.getPropertyValue(eq("CUSTOM_ENVIRONMENT_VARIABLE"), eq(true))).thenReturn("test");

    EnvironmentVariablesConfiguration configuration = new EnvironmentVariablesConfiguration(mockParentConfiguration);

    assertEquals(System.getenv("USER"), configuration.getPropertyValue("USER"));
    assertEquals("test", configuration.getPropertyValue("CUSTOM_ENVIRONMENT_VARIABLE"));
    assertNull(configuration.getPropertyValue("UNSET_ENVIRONMENT_VARIABLE", false));

    verify(mockParentConfiguration, times(1)).getPropertyValue(eq("CUSTOM_ENVIRONMENT_VARIABLE"), eq(true));
  }

  @Test
  public void testIterator() {

    Set<String> expectedEnvironmentVariableNames = new HashSet<>(System.getenv().keySet());

    assertFalse(expectedEnvironmentVariableNames.isEmpty());

    for (String actualEnvironmentVariableName : configuration) {
      assertTrue(expectedEnvironmentVariableNames.remove(actualEnvironmentVariableName));
    }

    assertTrue(expectedEnvironmentVariableNames.isEmpty());
  }
}
