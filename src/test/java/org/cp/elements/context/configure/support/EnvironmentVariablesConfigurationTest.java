/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.context.configure.support;

import static org.junit.Assert.*;
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
 * <p/>
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
