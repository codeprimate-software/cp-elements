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

package org.cp.elements.lang.support;

import static org.junit.Assert.*;

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
 * <p/>
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
