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
 *
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
    new ConfigurableVisitor<>(mockContext.mock(Configuration.class));
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

    ConfigurableVisitor<Configuration> visitor = new ConfigurableVisitor<>(mockConfiguration);

    visitor.visit(mockConfigurable);
  }

  @Test
  public void testVisitWithNonConfigurableVisitable() {
    new ConfigurableVisitor<>(mockContext.mock(Configuration.class))
      .visit(mockContext.mock(Visitable.class));
  }

  protected interface VisitableConfigurable<T> extends Configurable<T>, Visitable {
  }

}
