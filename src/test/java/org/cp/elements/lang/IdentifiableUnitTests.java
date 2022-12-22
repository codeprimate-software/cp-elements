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
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

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
public class IdentifiableUnitTests {

  @Mock
  private AbstractIdentifiable<Integer> mockIdentifiable;

  @Test
  public void isNewWhenGetIdReturnsNullReturnsTrue() {

    doReturn(null).when(this.mockIdentifiable).getId();
    doCallRealMethod().when(this.mockIdentifiable).isNew();

    assertThat(this.mockIdentifiable.isNew()).isTrue();

    verify(this.mockIdentifiable, times(1)).isNew();
    verify(this.mockIdentifiable, times(1)).getId();
    verifyNoMoreInteractions(this.mockIdentifiable);
  }

  @Test
  public void isNewWhenGetIdReturnsNonNullValueReturnsFalse() {

    doReturn(1).when(this.mockIdentifiable).getId();
    doCallRealMethod().when(this.mockIdentifiable).isNew();

    assertThat(this.mockIdentifiable.isNew()).isFalse();

    verify(this.mockIdentifiable, times(1)).isNew();
    verify(this.mockIdentifiable, times(1)).getId();
    verifyNoMoreInteractions(this.mockIdentifiable);
  }

  @Test
  public void isNotNewWhenGetIdReturnsNonNullValueReturnsTrue() {

    doReturn(1).when(this.mockIdentifiable).getId();
    doCallRealMethod().when(this.mockIdentifiable).isNotNew();
    doCallRealMethod().when(this.mockIdentifiable).isNew();

    assertThat(this.mockIdentifiable.isNotNew()).isTrue();

    verify(this.mockIdentifiable, times(1)).isNotNew();
    verify(this.mockIdentifiable, times(1)).isNew();
    verify(this.mockIdentifiable, times(1)).getId();
    verifyNoMoreInteractions(this.mockIdentifiable);
  }

  @Test
  public void isNotNewWhenGetIdReturnsNullReturnsFalse() {

    doReturn(null).when(this.mockIdentifiable).getId();
    doCallRealMethod().when(this.mockIdentifiable).isNotNew();
    doCallRealMethod().when(this.mockIdentifiable).isNew();

    assertThat(this.mockIdentifiable.isNotNew()).isFalse();

    verify(this.mockIdentifiable, times(1)).isNotNew();
    verify(this.mockIdentifiable, times(1)).isNew();
    verify(this.mockIdentifiable, times(1)).getId();
    verifyNoMoreInteractions(this.mockIdentifiable);
  }

  @Test
  public void identifiedByReturnsThis() {

    doCallRealMethod().when(this.mockIdentifiable).identifiedBy(anyInt());

    assertThat((Identifiable<Integer>) this.mockIdentifiable.identifiedBy(1)).isSameAs(this.mockIdentifiable);

    verify(this.mockIdentifiable, times(1)).identifiedBy(eq(1));
    verify(this.mockIdentifiable, times(1)).setId(eq(1));
    verifyNoMoreInteractions(this.mockIdentifiable);
  }

  @Test
  public void setIdByDefaultThrowsUnsupportedOperationException() {

    doCallRealMethod().when(this.mockIdentifiable).setId(anyInt());

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(() -> this.mockIdentifiable.setId(1))
      .withMessage("Setting ID is not supported")
      .withNoCause();

    verify(this.mockIdentifiable, times(1)).setId(eq(1));
    verifyNoMoreInteractions(this.mockIdentifiable);
  }

  static abstract class AbstractIdentifiable<T extends Comparable<T>> implements Identifiable<T> { }

}
