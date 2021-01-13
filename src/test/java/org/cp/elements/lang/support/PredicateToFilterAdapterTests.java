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

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.function.Predicate;

import org.junit.Test;

/**
 * Unit tests for {@link PredicateToFilterAdapter}.
 *
 * @author John Blum
 * @see java.util.function.Predicate
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.Filter
 * @since 1.0.0
 */
public class PredicateToFilterAdapterTests {

  @Test
  public void constructPredicateToFilterAdapter() {

    Predicate<?> mockPredicate = mock(Predicate.class);

    PredicateToFilterAdapter<?> filter = new PredicateToFilterAdapter<>(mockPredicate);

    assertThat(filter).isNotNull();
    assertThat(filter.getPredicate()).isEqualTo(mockPredicate);
  }

  @Test(expected = IllegalArgumentException.class)
  public void constructWithNullPredicateThrowsIllegalArgumentException() {

    try {
      new PredicateToFilterAdapter<>(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Predicate is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void ofNonNullPredicate() {

    Predicate<?> mockPredicate = mock(Predicate.class);

    PredicateToFilterAdapter<?> filter = PredicateToFilterAdapter.of(mockPredicate);

    assertThat(filter).isNotNull();
    assertThat(filter.getPredicate()).isEqualTo(mockPredicate);
  }

  @Test(expected = IllegalArgumentException.class)
  public void ofNullPredicateThrowsIllegalArgumentException() {

    try {
      PredicateToFilterAdapter.of(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Predicate is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void filterAcceptCallsPredicateTest() {

    Predicate<Object> mockPredicate = mock(Predicate.class);

    when(mockPredicate.test(any())).thenReturn(true);

    assertThat(PredicateToFilterAdapter.of(mockPredicate).test("test")).isTrue();

    verify(mockPredicate, times(1)).test(eq("test"));
  }
}
