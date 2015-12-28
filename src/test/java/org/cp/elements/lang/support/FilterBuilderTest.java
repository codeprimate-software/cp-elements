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
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import org.cp.elements.lang.Filter;
import org.cp.elements.test.AbstractMockingTestSuite;
import org.junit.Test;

/**
 * The FilterBuilderTest class is a test suite of test cases testing the contract and functionality of the FilterBuilder
 * class.
 *
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
