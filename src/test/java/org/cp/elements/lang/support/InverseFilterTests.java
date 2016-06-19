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
import static org.mockito.Matchers.anyObject;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.same;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.cp.elements.lang.Filter;
import org.junit.Test;

/**
 * Test suite of test cases testing the contract and functionality of the {@link InverseFilter} class.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.Filter
 * @see org.cp.elements.lang.support.InverseFilter
 * @since 1.0.0
 */
@SuppressWarnings("unchecked")
public class InverseFilterTests {

  @Test
  public void createInverseFilter() {
    Filter<Object> mockFilter = mock(Filter.class);
    InverseFilter<?> inverseFilter = new InverseFilter<>(mockFilter);

    assertNotNull(inverseFilter);
    assertSame(mockFilter, inverseFilter.getFilter());
  }

  @Test(expected = IllegalArgumentException.class)
  public void createInverseFilterWithNullFilter() {
    new InverseFilter<>(null);
  }

  @Test
  public void acceptReturnsFalse() {
    Object value = new Object();
    Filter<Object> mockFilter = mock(Filter.class);

    when(mockFilter.accept(same(value))).thenReturn(true);

    InverseFilter<Object> inverseFilter = new InverseFilter<>(mockFilter);

    assertNotNull(inverseFilter);
    assertSame(mockFilter, inverseFilter.getFilter());
    assertFalse(inverseFilter.accept(value));

    verify(mockFilter, times(1)).accept(eq(value));
  }

  @Test
  public void acceptReturnsTrue() {
    Object value = new Object();
    Filter<Object> mockFilter = mock(Filter.class);

    when(mockFilter.accept(anyObject())).thenReturn(false);

    InverseFilter<Object> inverseFilter = new InverseFilter<>(mockFilter);

    assertNotNull(inverseFilter);
    assertSame(mockFilter, inverseFilter.getFilter());
    assertTrue(inverseFilter.accept(value));

    verify(mockFilter, times(1)).accept(eq(value));
  }
}
