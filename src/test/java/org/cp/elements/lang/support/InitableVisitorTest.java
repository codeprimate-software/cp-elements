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

package org.cp.elements.lang.support;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;

import java.util.HashMap;
import java.util.Map;

import org.cp.elements.lang.Initable;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.ParameterizedInitable;
import org.cp.elements.lang.Visitable;
import org.cp.elements.test.AbstractMockingTestSuite;
import org.jmock.Expectations;
import org.junit.Test;

/**
 * The InitableVisitorTest class is a test suite of test cases testing the contract and functionality of the
 * InitableVisitor class.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.support.InitableVisitor
 * @see org.cp.elements.test.AbstractMockingTestSuite
 * @see org.junit.Test
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class InitableVisitorTest extends AbstractMockingTestSuite {

  @Test
  public void testConstruct() {
    InitableVisitor visitor = new InitableVisitor();

    assertNotNull(visitor);
    assertNull(visitor.getArguments());
    assertNull(visitor.getParameters());
  }

  @Test
  public void testConstructWithArguments() {
    Object[] expectedArguments = { true, 'x', 2, Math.PI, "test" };
    InitableVisitor visitor = new InitableVisitor(expectedArguments);

    assertNotNull(visitor);
    assertSame(expectedArguments, visitor.getArguments());
    assertNull(visitor.getParameters());
  }

  @Test
  public void testConstructWithParameters() {
    Map<String, String> expectedParameters = new HashMap<>(3);

    expectedParameters.put("param1", "value1");
    expectedParameters.put("param2", "value2");
    expectedParameters.put("param3", "value3");

    InitableVisitor visitor = new InitableVisitor(expectedParameters);

    assertNotNull(visitor);
    assertNull(visitor.getArguments());
    assertSame(expectedParameters, visitor.getParameters());
  }

  @Test
  public void testVisitParameterizedInitableWithParameters() {
    final Map<String, String> expectedParameters = new HashMap<>(3);

    expectedParameters.put("param1", "value1");
    expectedParameters.put("param2", "value2");
    expectedParameters.put("param3", "value3");

    final VisitiableParameterizedInitiable mockInitable = mockContext.mock(VisitiableParameterizedInitiable.class);

    mockContext.checking(new Expectations() {{
      oneOf(mockInitable).init(with(equal(expectedParameters)));
      never(mockInitable).init(with(any(Object[].class)));
      never(mockInitable).init();
    }});

    InitableVisitor visitor = new InitableVisitor(expectedParameters);

    assertNotNull(visitor);
    assertNull(visitor.getArguments());
    assertSame(expectedParameters, visitor.getParameters());

    visitor.visit(mockInitable);
  }

  @Test
  public void testVisitParameterizedInitableWithArguments() {
    final Object[] expectedArguments = { true, 'x', 1, Math.PI, "test" };
    final VisitiableParameterizedInitiable mockInitable = mockContext.mock(VisitiableParameterizedInitiable.class);

    mockContext.checking(new Expectations() {{
      oneOf(mockInitable).init(with(equal(expectedArguments)));
      never(mockInitable).init(with(any(Map.class)));
      never(mockInitable).init();
    }});

    InitableVisitor visitor = new InitableVisitor(expectedArguments);

    assertNotNull(visitor);
    assertSame(expectedArguments, visitor.getArguments());
    assertNull(visitor.getParameters());

    visitor.visit(mockInitable);
  }

  @Test
  public void testVisitParameterizedInitable() {
    final VisitiableParameterizedInitiable mockInitable = mockContext.mock(VisitiableParameterizedInitiable.class);

    mockContext.checking(new Expectations() {{
      oneOf(mockInitable).init();
      never(mockInitable).init(with(any(Object[].class)));
      never(mockInitable).init(with(any(Map.class)));
    }});

    InitableVisitor visitor = new InitableVisitor();

    assertNotNull(visitor);
    assertNull(visitor.getArguments());
    assertNull(visitor.getParameters());

    visitor.visit(mockInitable);
  }

  @Test
  public void testVisitInitable() {
    final VisitableInitable mockInitable = mockContext.mock(VisitableInitable.class);

    mockContext.checking(new Expectations() {{
      oneOf(mockInitable).init();
    }});

    InitableVisitor visitor = new InitableVisitor(ObjectUtils.EMPTY_OBJECT_ARRAY);

    assertNotNull(visitor);
    assertSame(ObjectUtils.EMPTY_OBJECT_ARRAY, visitor.getArguments());
    assertNull(visitor.getParameters());

    visitor.visit(mockInitable);
  }

  @Test
  public void testVisitNonInitableVisitable() {
    new InitableVisitor().visit(mockContext.mock(Visitable.class));
  }

  protected interface VisitiableParameterizedInitiable extends ParameterizedInitable, Visitable {
  }

  protected interface VisitableInitable extends Initable, Visitable {
  }

}
