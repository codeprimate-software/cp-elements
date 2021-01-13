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

package org.cp.elements.lang;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Test;

/**
 * Unit tests for the {@link Filter} interface.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.Filter
 * @since 1.0.0
 */
public class FilterTests {

  @Test
  public void acceptingFilterAcceptsAll() {

    assertThat(Filter.accepting().accept(false)).isTrue();
    assertThat(Filter.accepting().accept('X')).isTrue();
    assertThat(Filter.accepting().accept(Math.PI)).isTrue();
    assertThat(Filter.accepting().accept(-1)).isTrue();
    assertThat(Filter.accepting().accept("test")).isTrue();
  }

  @Test
  public void acceptingAndAcceptingReturnsTrue() {
    assertThat(Filter.accepting().and(Filter.accepting()).test("test")).isTrue();
  }

  @Test
  public void acceptingAndRejectingReturnsFalse() {
    assertThat(Filter.accepting().and(Filter.rejecting()).test("test")).isFalse();
  }

  @Test
  public void acceptingNegatedReturnsFalse() {
    assertThat(Filter.accepting().negate().test("test")).isFalse();
  }

  @Test
  public void acceptingOrAcceptingReturnsTrue() {
    assertThat(Filter.accepting().or(Filter.accepting()).test("test")).isTrue();
  }

  @Test
  public void acceptingOrRejectingReturnsTrue() {
    assertThat(Filter.accepting().or(Filter.rejecting()).test("test")).isTrue();
  }

  @Test
  public void rejectingFilterRejectsAll() {

    assertThat(Filter.rejecting().accept(true)).isFalse();
    assertThat(Filter.rejecting().accept('O')).isFalse();
    assertThat(Filter.rejecting().accept(Math.PI)).isFalse();
    assertThat(Filter.rejecting().accept(1)).isFalse();
    assertThat(Filter.rejecting().accept("test")).isFalse();
  }

  @Test
  public void rejectingAndAcceptingReturnsFalse() {
    assertThat(Filter.rejecting().and(Filter.accepting()).test("test")).isFalse();
  }

  @Test
  public void rejectingAndRejectingReturnsFalse() {
    assertThat(Filter.rejecting().and(Filter.rejecting()).test("test")).isFalse();
  }

  @Test
  public void rejectingNegatedReturnsTrue() {
    assertThat(Filter.rejecting().negate().test("test")).isTrue();
  }

  @Test
  public void rejectingOrAcceptingReturnsTrue() {
    assertThat(Filter.rejecting().or(Filter.accepting()).test("test")).isTrue();
  }

  @Test
  public void rejectingOrRejectingReturnsFalse() {
    assertThat(Filter.rejecting().or(Filter.rejecting()).test("test")).isFalse();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testCallsAcceptReturnsTrue() {

    Filter<Object> mockFilter = mock(Filter.class);

    when(mockFilter.test(any())).thenCallRealMethod();
    when(mockFilter.accept(any())).thenReturn(true);

    assertThat(mockFilter.test("test")).isTrue();

    verify(mockFilter, times(1)).accept(eq("test"));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testCallsAcceptReturnsFalse() {

    Filter<Object> mockFilter = mock(Filter.class);

    when(mockFilter.test(any())).thenCallRealMethod();
    when(mockFilter.accept(any())).thenReturn(false);

    assertThat(mockFilter.test("test")).isFalse();

    verify(mockFilter, times(1)).accept(eq("test"));
  }
}
