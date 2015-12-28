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

package org.cp.elements.lang;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.cp.elements.test.AbstractMockingTestSuite;
import org.jmock.Expectations;
import org.junit.Test;

/**
 * The InitializerTest class is a test suite of test cases testing the contract and functionality of the Initializer
 * class.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.Initable
 * @see org.cp.elements.lang.Initializer
 * @see org.cp.elements.lang.ParameterizedInitable
 * @see org.cp.elements.test.AbstractMockingTestSuite
 * @see org.junit.Test
 */
public class InitializerTest extends AbstractMockingTestSuite {

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
