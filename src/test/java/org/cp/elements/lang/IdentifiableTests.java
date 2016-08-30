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

import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.anyLong;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

/**
 * Test suite of test cases testing the contract and functionality of the {@link Identifiable} interface.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.runners.MockitoJUnitRunner
 * @see org.cp.elements.lang.Identifiable
 */
@RunWith(MockitoJUnitRunner.class)
public class IdentifiableTests {

  @Mock
  private AbstractIdentifiable<Long> mockIdentifiable;

  @Test
  public void isNewIsTrueWhenGetIdReturnsNull() {
    when(mockIdentifiable.getId()).thenReturn(null);
    when(mockIdentifiable.isNew()).thenCallRealMethod();
    assertThat(mockIdentifiable.isNew(), is(true));
    verify(mockIdentifiable, times(1)).getId();
  }

  @Test
  public void isNewIsFalseWhenGetIdReturnsNonNullValue() {
    when(mockIdentifiable.getId()).thenReturn(1L);
    when(mockIdentifiable.isNew()).thenCallRealMethod();
    assertThat(mockIdentifiable.isNew(), is(false));
    verify(mockIdentifiable, times(1)).getId();
  }

  @Test
  public void isNotNewIsTrueWhenGetIdReturnsNonNullValue() {
    when(mockIdentifiable.getId()).thenReturn(1L);
    when(mockIdentifiable.isNew()).thenCallRealMethod();
    when(mockIdentifiable.isNotNew()).thenCallRealMethod();
    assertThat(mockIdentifiable.isNotNew(), is(true));
    verify(mockIdentifiable, times(1)).isNew();
    verify(mockIdentifiable, times(1)).getId();
  }

  @Test
  public void isNotNewIsFalseWhenGetIdReturnsNull() {
    when(mockIdentifiable.getId()).thenReturn(null);
    when(mockIdentifiable.isNew()).thenCallRealMethod();
    when(mockIdentifiable.isNotNew()).thenCallRealMethod();
    assertThat(mockIdentifiable.isNotNew(), is(false));
    verify(mockIdentifiable, times(1)).isNew();
    verify(mockIdentifiable, times(1)).getId();
  }

  @Test
  public void identifiedByReturnsThis() {
    when(mockIdentifiable.identifiedBy(anyLong())).thenCallRealMethod();
    assertThat(mockIdentifiable.identifiedBy(1L), is(sameInstance(mockIdentifiable)));
    verify(mockIdentifiable, times(1)).setId(eq(1L));
  }

  abstract class AbstractIdentifiable<T extends Comparable<T>> implements Identifiable<T> {
  }
}
