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

import org.cp.elements.lang.Filter;
import org.jmock.Expectations;
import org.jmock.Mockery;
import org.junit.After;
import org.junit.Test;

/**
 * The InverseFilterTest class is a test suite of test cases testing the contract and functionality
 * of the InverseFilter class.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.lang.Filter
 * @see org.cp.elements.lang.support.InverseFilter
 * @see org.jmock.Mockery
 * @see org.junit.Assert
 * @see org.junit.Test
 * @since 1.0.0
 */
public class InverseFilterTest {

  private final Mockery mockContext = new Mockery();

  @After
  public void tearDown() {
    mockContext.assertIsSatisfied();
  }

  @Test
  public void testCreateInverseFilter() {
    final Filter<Object> mockFilter = mockContext.mock(Filter.class);
    final InverseFilter<?> inverseFilter = new InverseFilter<Object>(mockFilter);

    assertNotNull(inverseFilter);
    assertSame(mockFilter, inverseFilter.getFilter());
  }

  @Test(expected = NullPointerException.class)
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
