/*
 * Copyright 2011-Present Author or Authors.
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
import static org.mockito.Mockito.mock;

import org.cp.elements.lang.Filter;
import org.junit.Test;

/**
 * Test suite of test cases testing the contract and functionality of the {@link FilterBuilder} class.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.cp.elements.lang.Filter
 * @see org.cp.elements.lang.support.FilterBuilder
 * @since 1.0.0
 */
public class FilterBuilderTests {

  @Test
  @SuppressWarnings("unchecked")
  public void addWithAndThenBuild() {
    FilterBuilder<Object> filterBuilder = new FilterBuilder<>();

    Filter<Object> mockFilterLeft = mock(Filter.class, "MockFilterLeft");
    Filter<Object> mockFilterRight = mock(Filter.class, "MockFilterRight");

    assertSame(filterBuilder, filterBuilder.addWithAnd(mockFilterLeft));
    assertSame(mockFilterLeft, filterBuilder.build());
    assertSame(filterBuilder, filterBuilder.addWithAnd(mockFilterRight));

    Filter<Object> leftRightFilter = filterBuilder.build();

    assertNotNull(leftRightFilter);
    assertNotSame(mockFilterLeft, leftRightFilter);
    assertNotSame(mockFilterRight, leftRightFilter);
    assertTrue(leftRightFilter instanceof ComposableFilter);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void addWithOrThenBuild() {
    FilterBuilder<Object> filterBuilder = new FilterBuilder<>();

    Filter<Object> mockFilterLeft = mock(Filter.class, "MockFilterLeft");
    Filter<Object> mockFilterRight = mock(Filter.class, "MockFilterRight");

    assertSame(filterBuilder, filterBuilder.addWithOr(mockFilterLeft));
    assertSame(mockFilterLeft, filterBuilder.build());
    assertSame(filterBuilder, filterBuilder.addWithOr(mockFilterRight));

    Filter<Object> leftRightFilter = filterBuilder.build();

    assertNotNull(leftRightFilter);
    assertNotSame(mockFilterLeft, leftRightFilter);
    assertNotSame(mockFilterRight, leftRightFilter);
    assertTrue(leftRightFilter instanceof ComposableFilter);
  }
}
