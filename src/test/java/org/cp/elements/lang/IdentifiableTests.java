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
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit Tests for the {@link Identifiable} interface.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.cp.elements.lang.Identifiable
 */
@RunWith(MockitoJUnitRunner.class)
public class IdentifiableTests {

  @Mock
  private AbstractIdentifiable<Long> mockIdentifiable;

  @Test
  public void isNewWhenGetIdReturnsNullReurnsTrue() {

    when(this.mockIdentifiable.getId()).thenReturn(null);
    when(this.mockIdentifiable.isNew()).thenCallRealMethod();

    assertThat(this.mockIdentifiable.isNew()).isTrue();

    verify(this.mockIdentifiable, times(1)).getId();
  }

  @Test
  public void isNewWhenGetIdReturnsNonNullValueReturnsFalse() {

    when(this.mockIdentifiable.getId()).thenReturn(1L);
    when(this.mockIdentifiable.isNew()).thenCallRealMethod();

    assertThat(this.mockIdentifiable.isNew()).isFalse();

    verify(this.mockIdentifiable, times(1)).getId();
  }

  @Test
  public void isNotNewWhenGetIdReturnsNonNullValueReturnsTrue() {

    when(this.mockIdentifiable.getId()).thenReturn(1L);
    when(this.mockIdentifiable.isNew()).thenCallRealMethod();
    when(this.mockIdentifiable.isNotNew()).thenCallRealMethod();

    assertThat(this.mockIdentifiable.isNotNew()).isTrue();

    verify(this.mockIdentifiable, times(1)).isNew();
    verify(this.mockIdentifiable, times(1)).getId();
  }

  @Test
  public void isNotNewWhenGetIdReturnsNullReturnsFalse() {

    when(this.mockIdentifiable.getId()).thenReturn(null);
    when(this.mockIdentifiable.isNew()).thenCallRealMethod();
    when(this.mockIdentifiable.isNotNew()).thenCallRealMethod();

    assertThat(this.mockIdentifiable.isNotNew()).isFalse();

    verify(this.mockIdentifiable, times(1)).isNew();
    verify(this.mockIdentifiable, times(1)).getId();
  }

  @Test
  public void identifiedByReturnsThis() {

    when(this.mockIdentifiable.identifiedBy(anyLong())).thenCallRealMethod();

    assertThat((Identifiable<Long>) this.mockIdentifiable.identifiedBy(1L)).isSameAs(this.mockIdentifiable);

    verify(this.mockIdentifiable, times(1)).setId(eq(1L));
  }

  static abstract class AbstractIdentifiable<T extends Comparable<T>> implements Identifiable<T> { }

}
