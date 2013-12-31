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

package org.cp.elements.lang;

import static org.junit.Assert.*;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.jmock.Expectations;
import org.jmock.Mockery;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * The InitializerTest class is a test suite of test cases testing the contract and functionality of the Initializer
 * class.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.lang.Initable
 * @see org.cp.elements.lang.Initializer
 * @see org.cp.elements.lang.ParameterizedInitable
 * @see org.jmock.Mockery
 * @see org.junit.Assert
 * @see org.junit.Test
 */
public class InitializerTest {

  private Mockery mockContext;

  @Before
  public void setUp() {
    mockContext = new Mockery();
  }

  @After
  public void tearDown() {
    mockContext.assertIsSatisfied();
    mockContext = null;
  }

  @Test
  public void testInitUsingInitableObject() {
    final Initable mockInitableObj = mockContext.mock(Initable.class, "testInitWithInitableObject");

    mockContext.checking(new Expectations() {{
      oneOf(mockInitableObj).init();
    }});

    assertTrue(Initializer.init(mockInitableObj));
  }

  @Test
  public void testInitUsingNonInitableObject() {
    assertFalse(Initializer.init(new Object()));
  }

  @Test
  public void testInitWithArgumentsUsingParameterizedInitableObject() {
    final Object[] testArgs = { "arg1", "arg2", "arg3" };
    final ParameterizedInitable mockInitableObj = mockContext.mock(ParameterizedInitable.class, "testInitWithArgumentsUsingParameterizedInitableObject");

    mockContext.checking(new Expectations() {{
      oneOf(mockInitableObj).init(with(same(testArgs)));
    }});

    assertTrue(Initializer.init(mockInitableObj, testArgs));
  }

  @Test
  public void testInitWithArgumentsUsingInitableObject() {
    final Object[] testArgs = { "arg1", "arg2", "arg3" };
    final Initable mockInitableObj = mockContext.mock(Initable.class, "testInitWithArgumentsUsingInitableObject");

    mockContext.checking(new Expectations() {{
      oneOf(mockInitableObj).init();
    }});

    assertTrue(Initializer.init(mockInitableObj, testArgs));
  }

  @Test
  public void testInitWithArgumentsUsingNonInitableObject() {
    assertFalse(Initializer.init(new Object(), "arg1", "arg2", "arg3"));
  }

  @Test
  public void testInitWithParametersUsingParameterizedInitableObject() {
    final Map<String, String> testParameters = new HashMap<String, String>(3);

    testParameters.put("param1", "arg1");
    testParameters.put("param2", "arg2");
    testParameters.put("param3", "arg3");

    final ParameterizedInitable mockInitableObj = mockContext.mock(ParameterizedInitable.class, "testInitWithParametersUsingParameterizedInitableObject");

    mockContext.checking(new Expectations() {{
      oneOf(mockInitableObj).init(with(same(testParameters)));
    }});

    assertTrue(Initializer.init(mockInitableObj, testParameters));
  }

  @Test
  public void testInitWithParametersUsingInitableObject() {
    final Map<String, String> testParameters = new HashMap<String, String>(3);

    testParameters.put("param1", "arg1");
    testParameters.put("param2", "arg2");
    testParameters.put("param3", "arg3");

    final Initable mockInitableObj = mockContext.mock(Initable.class, "testInitWithParametersUsingInitableObject");

    mockContext.checking(new Expectations() {{
      oneOf(mockInitableObj).init();
    }});

    assertTrue(Initializer.init(mockInitableObj, testParameters));
  }

  @Test
  public void testInitWithParametersUsingNonInitableObject() {
    assertFalse(Initializer.init(new Object(), Collections.emptyMap()));
  }

}
