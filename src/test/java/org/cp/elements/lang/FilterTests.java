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

    assertThat(Filter.ACCEPTING.accept(false)).isTrue();
    assertThat(Filter.ACCEPTING.accept('X')).isTrue();
    assertThat(Filter.ACCEPTING.accept(Math.PI)).isTrue();
    assertThat(Filter.ACCEPTING.accept(-1)).isTrue();
    assertThat(Filter.ACCEPTING.accept("test")).isTrue();
  }

  @Test
  public void acceptingAndAcceptingReturnsTrue() {
    assertThat(Filter.ACCEPTING.and(Filter.ACCEPTING).test("test")).isTrue();
  }

  @Test
  public void acceptingAndRejectingReturnsFalse() {
    assertThat(Filter.ACCEPTING.and(Filter.REJECTING).test("test")).isFalse();
  }

  @Test
  public void acceptingNegatedReturnsFalse() {
    assertThat(Filter.ACCEPTING.negate().test("test")).isFalse();
  }

  @Test
  public void acceptingOrAcceptingReturnsTrue() {
    assertThat(Filter.ACCEPTING.or(Filter.ACCEPTING).test("test")).isTrue();
  }

  @Test
  public void acceptingOrRejectingReturnsTrue() {
    assertThat(Filter.ACCEPTING.or(Filter.REJECTING).test("test")).isTrue();
  }

  @Test
  public void rejectingFilterRejectsAll() {

    assertThat(Filter.REJECTING.accept(true)).isFalse();
    assertThat(Filter.REJECTING.accept('O')).isFalse();
    assertThat(Filter.REJECTING.accept(Math.PI)).isFalse();
    assertThat(Filter.REJECTING.accept(1)).isFalse();
    assertThat(Filter.REJECTING.accept("test")).isFalse();
  }

  @Test
  public void rejectingAndAcceptingReturnsFalse() {
    assertThat(Filter.REJECTING.and(Filter.ACCEPTING).test("test")).isFalse();
  }

  @Test
  public void rejectingAndRejectingReturnsFalse() {
    assertThat(Filter.REJECTING.and(Filter.REJECTING).test("test")).isFalse();
  }

  @Test
  public void rejectingNegatedReturnsTrue() {
    assertThat(Filter.REJECTING.negate().test("test")).isTrue();
  }

  @Test
  public void rejectingOrAcceptingReturnsTrue() {
    assertThat(Filter.REJECTING.or(Filter.ACCEPTING).test("test")).isTrue();
  }

  @Test
  public void rejectingOrRejectingReturnsFalse() {
    assertThat(Filter.REJECTING.or(Filter.REJECTING).test("test")).isFalse();
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
