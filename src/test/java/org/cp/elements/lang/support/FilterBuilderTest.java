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
import org.cp.elements.test.AbstractMockingTestSuite;
import org.junit.Test;

/**
 * The FilterBuilderTest class is a test suite of test cases testing the contract and functionality of the FilterBuilder
 * class.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.lang.Filter
 * @see org.cp.elements.test.AbstractMockingTestSuite
 * @see org.junit.Test
 */
public class FilterBuilderTest extends AbstractMockingTestSuite {

  @Test
  @SuppressWarnings("unchecked")
  public void testAddWithAndThenBuild() {
    final FilterBuilder<Object> builder = new FilterBuilder<Object>();

    final Filter<Object> leftFilter = mockContext.mock(Filter.class, "testAddWithAndThenBuild.Filter.1");
    final Filter<Object> rightFilter = mockContext.mock(Filter.class, "testAddWithAndThenBuild.Filter.2");

    assertSame(builder, builder.addWithAnd(leftFilter));
    assertSame(leftFilter, builder.build());
    assertSame(builder, builder.addWithAnd(rightFilter));

    final Filter<Object> leftRightFilter = builder.build();

    assertNotNull(leftRightFilter);
    assertNotSame(leftFilter, leftRightFilter);
    assertNotSame(rightFilter, leftRightFilter);
    assertTrue(leftRightFilter instanceof ComposableFilter);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testAddWithOrThenBuild() {
    final FilterBuilder<Object> builder = new FilterBuilder<Object>();

    final Filter<Object> leftFilter = mockContext.mock(Filter.class, "testAddWithAndThenBuild.Filter.1");
    final Filter<Object> rightFilter = mockContext.mock(Filter.class, "testAddWithAndThenBuild.Filter.2");

    assertSame(builder, builder.addWithOr(leftFilter));
    assertSame(leftFilter, builder.build());
    assertSame(builder, builder.addWithOr(rightFilter));

    final Filter<Object> leftRightFilter = builder.build();

    assertNotNull(leftRightFilter);
    assertNotSame(leftFilter, leftRightFilter);
    assertNotSame(rightFilter, leftRightFilter);
    assertTrue(leftRightFilter instanceof ComposableFilter);
  }

}
