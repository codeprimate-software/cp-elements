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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import org.cp.elements.lang.Filter;
import org.cp.elements.test.AbstractMockingTestSuite;
import org.jmock.Expectations;
import org.junit.Test;

/**
 * The InverseFilterTest class is a test suite of test cases testing the contract and functionality
 * of the InverseFilter class.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.Filter
 * @see org.cp.elements.lang.support.InverseFilter
 * @see org.cp.elements.test.AbstractMockingTestSuite
 * @see org.junit.Test
 * @since 1.0.0
 */
@SuppressWarnings("unchecked")
public class InverseFilterTest extends AbstractMockingTestSuite {

  @Test
  public void testCreateInverseFilter() {
    final Filter<Object> mockFilter = mockContext.mock(Filter.class);
    final InverseFilter<?> inverseFilter = new InverseFilter<Object>(mockFilter);

    assertNotNull(inverseFilter);
    assertSame(mockFilter, inverseFilter.getFilter());
  }

  @Test(expected = IllegalArgumentException.class)
  public void testCreateInverseFilterWithNullFilter() {
    new InverseFilter<Object>(null);
  }

  @Test
  public void testAcceptReturnsFalse() {
    final Object value = new Object();
    final Filter<Object> mockFilter = mockContext.mock(Filter.class);

    mockContext.checking(new Expectations() {{
      oneOf(mockFilter).accept(with(same(value)));
      will(returnValue(true));
    }});

    final InverseFilter<Object> inverseFilter = new InverseFilter<Object>(mockFilter);

    assertNotNull(inverseFilter);
    assertSame(mockFilter, inverseFilter.getFilter());
    assertFalse(inverseFilter.accept(value));
  }

  @Test
  public void testAcceptReturnsTrue() {
    final Object value = new Object();
    final Filter<Object> mockFilter = mockContext.mock(Filter.class);

    mockContext.checking(new Expectations() {{
      oneOf(mockFilter).accept(with(same(value)));
      will(returnValue(false));
    }});

    final InverseFilter<Object> inverseFilter = new InverseFilter<Object>(mockFilter);

    assertNotNull(inverseFilter);
    assertSame(mockFilter, inverseFilter.getFilter());
    assertTrue(inverseFilter.accept(value));
  }
}
