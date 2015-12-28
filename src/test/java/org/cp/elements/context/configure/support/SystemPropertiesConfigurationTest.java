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

import java.util.HashSet;
import java.util.Set;

import org.cp.elements.context.configure.Configuration;
import org.cp.elements.test.AbstractMockingTestSuite;
import org.jmock.Expectations;
import org.junit.Test;

/**
 * The SystemPropertiesConfigurationTest class is a test suite of test cases testing the contract and functionality
 * of the SystemPropertiesConfiguration class.
 *
 * @author John J. Blum
 * @see org.cp.elements.context.configure.Configuration
 * @see org.cp.elements.context.configure.support.SystemPropertiesConfiguration
 * @see org.cp.elements.test.AbstractMockingTestSuite
 * @see org.junit.Test
 * @since 1.0.0
 */
public class SystemPropertiesConfigurationTest extends AbstractMockingTestSuite {

  private final SystemPropertiesConfiguration configuration = new SystemPropertiesConfiguration();

  @Test
  public void testIsPresent() {
    assertTrue(configuration.isPresent("java.class.path"));
    assertTrue(configuration.isPresent("java.home"));
    assertTrue(configuration.isPresent("java.version"));
    assertTrue(configuration.isPresent("user.dir"));
    assertTrue(configuration.isPresent("user.home"));
    assertTrue(configuration.isPresent("user.name"));
    assertFalse(configuration.isPresent("unset.system.property"));
  }

  @Test
  public void testDoGetPropertyValue() {
    assertEquals(System.getProperty("java.class.path"), configuration.doGetPropertyValue("java.class.path"));
    assertEquals(System.getProperty("java.home"), configuration.doGetPropertyValue("java.home"));
    assertEquals(System.getProperty("java.version"), configuration.doGetPropertyValue("java.version"));
    assertEquals(System.getProperty("user.dir"), configuration.doGetPropertyValue("user.dir"));
    assertEquals(System.getProperty("user.home"), configuration.doGetPropertyValue("user.home"));
    assertEquals(System.getProperty("user.name"), configuration.doGetPropertyValue("user.name"));
  }

  @Test
  public void testGetParentPropertyValue() {
    final Configuration mockParentConfiguration = mockContext.mock(Configuration.class);

    mockContext.checking(new Expectations() {{
      allowing(mockParentConfiguration).getPropertyValue(with(equal("custom.system.property")), with(equal(true)));
      will(returnValue("test"));
      allowing(mockParentConfiguration).getPropertyValue(with(any(String.class)), with(any(Boolean.class)));
    }});

    SystemPropertiesConfiguration configuration = new SystemPropertiesConfiguration(mockParentConfiguration);

    assertEquals(System.getProperty("java.version"), configuration.getPropertyValue("java.version"));
    assertEquals("test", configuration.getPropertyValue("custom.system.property"));
    assertNull(configuration.getPropertyValue("unset.system.property", false));
  }

  @Test
  public void testIterator() {
    Set<String> expectedSystemPropertyNames = new HashSet<String>(System.getProperties().stringPropertyNames());

    assertFalse(expectedSystemPropertyNames.isEmpty());

    for (String actualSystemPropertyName : configuration) {
      assertTrue(expectedSystemPropertyNames.remove(actualSystemPropertyName));
    }

    assertTrue(expectedSystemPropertyNames.isEmpty());
  }

}
