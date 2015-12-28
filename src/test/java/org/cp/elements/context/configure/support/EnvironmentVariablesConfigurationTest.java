/*
 * Copyright 2016 Author or Authors.
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

import java.util.HashSet;
import java.util.Set;

import org.cp.elements.context.configure.Configuration;
import org.cp.elements.test.AbstractMockingTestSuite;
import org.jmock.Expectations;
import org.junit.Test;

/**
 * The EnvironmentVariablesConfigurationTest class is a test suite of test cases testing the contract and functionality
 * of the EnvironmentVariablesConfiguration class.
 *
 * @author John J. Blum
 * @see org.cp.elements.context.configure.Configuration
 * @see org.cp.elements.context.configure.support.EnvironmentVariablesConfiguration
 * @see org.cp.elements.test.AbstractMockingTestSuite
 * @see org.junit.Test
 * @since 1.0.0
 */
public class EnvironmentVariablesConfigurationTest extends AbstractMockingTestSuite {

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
  public void testGetParentPropertyValue() {
    final Configuration mockParentConfiguration = mockContext.mock(Configuration.class);

    mockContext.checking(new Expectations() {{
      allowing(mockParentConfiguration).getPropertyValue(with(equal("CUSTOM_ENVIRONMENT_VARIABLE")), with(equal(true)));
      will(returnValue("test"));
      allowing(mockParentConfiguration).getPropertyValue(with(any(String.class)), with(any(Boolean.class)));
      will(returnValue(null));
    }});

    EnvironmentVariablesConfiguration configuration = new EnvironmentVariablesConfiguration(mockParentConfiguration);

    assertEquals(System.getenv("USER"), configuration.getPropertyValue("USER"));
    assertEquals("test", configuration.getPropertyValue("CUSTOM_ENVIRONMENT_VARIABLE"));
    assertNull(configuration.getPropertyValue("UNSET_ENVIRONMENT_VARIABLE", false));
  }

  @Test
  public void testIterator() {
    Set<String> expectedEnvironmentVariableNames = new HashSet<String>(System.getenv().keySet());

    assertFalse(expectedEnvironmentVariableNames.isEmpty());

    for (String actualEnvironmentVariableName : configuration) {
      assertTrue(expectedEnvironmentVariableNames.remove(actualEnvironmentVariableName));
    }

    assertTrue(expectedEnvironmentVariableNames.isEmpty());
  }

}
