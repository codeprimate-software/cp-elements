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

import java.util.HashSet;
import java.util.Set;

import org.cp.elements.context.configure.Configuration;
import org.jmock.Expectations;
import org.jmock.Mockery;
import org.jmock.lib.legacy.ClassImposteriser;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * The SystemPropertiesConfigurationTest class is a test suite of test cases testing the contract and functionality
 * of the SystemPropertiesConfiguration class.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.context.configure.Configuration
 * @see org.cp.elements.context.configure.support.SystemPropertiesConfiguration
 * @see org.jmock.Mockery
 * @see org.jmock.lib.legacy.ClassImposteriser
 * @see org.junit.Test
 * @since 1.0.0
 */
public class SystemPropertiesConfigurationTest {

  private final SystemPropertiesConfiguration configuration = new SystemPropertiesConfiguration();

  private Mockery mockContext;

  @Before
  public void setup() {
    mockContext = new Mockery();
    mockContext.setImposteriser(ClassImposteriser.INSTANCE);
  }

  @After
  public void tearDown() {
    mockContext.assertIsSatisfied();
    mockContext = null;
  }

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
