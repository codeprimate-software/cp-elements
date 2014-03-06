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

import static org.junit.Assert.assertEquals;

import org.cp.elements.context.configure.Configuration;
import org.cp.elements.lang.Configurable;
import org.cp.elements.lang.Visitable;
import org.cp.elements.test.AbstractMockingTestSuite;
import org.jmock.Expectations;
import org.junit.Test;

/**
 * The ConfigurableVisitorTest class is a test suite of test cases testing the contract and functionality of the
 * ConfigurableVisitor class.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.lang.support.ConfigurableVisitor
 * @see org.cp.elements.test.AbstractMockingTestSuite
 * @see org.junit.Test
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ConfigurableVisitorTest extends AbstractMockingTestSuite {

  @Test
  public void testConstruct() {
    new ConfigurableVisitor<Configuration>(mockContext.mock(Configuration.class));
  }

  @Test(expected = NullPointerException.class)
  public void testConstructWithNullConfiguration() {
    try {
      new ConfigurableVisitor<Configuration>(null);
    }
    catch (NullPointerException expected) {
      assertEquals("The configuration object cannot be null!", expected.getMessage());
      throw expected;
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testVisit() {
    final Configuration mockConfiguration = mockContext.mock(Configuration.class);
    final VisitableConfigurable<Configuration> mockConfigurable = mockContext.mock(VisitableConfigurable.class);

    mockContext.checking(new Expectations() {{
      oneOf(mockConfigurable).configure(with(equal(mockConfiguration)));
    }});

    ConfigurableVisitor<Configuration> visitor = new ConfigurableVisitor<Configuration>(mockConfiguration);

    visitor.visit(mockConfigurable);
  }

  @Test
  public void testVisitWithNonConfigurableVisitable() {
    new ConfigurableVisitor<Configuration>(mockContext.mock(Configuration.class))
      .visit(mockContext.mock(Visitable.class));
  }

  protected static interface VisitableConfigurable<T> extends Configurable<T>, Visitable {
  }

}
