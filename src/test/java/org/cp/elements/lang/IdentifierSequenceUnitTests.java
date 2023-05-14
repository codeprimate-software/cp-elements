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
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.withSettings;

import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Test;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

import org.mockito.quality.Strictness;

import lombok.NonNull;

/**
 * Unit Tests for the {@link IdentifierSequence}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.IdentifierSequence
 * @since 1.0.0
 */
public class IdentifierSequenceUnitTests {

  private final IdentifierSequence<Integer> identifierSequence = new TestIdentifierSequence();

  private @NotNull IdentifierSequence<Integer> getIdentifierSequence() {
    return this.identifierSequence;
  }

  @SuppressWarnings("unchecked")
  private @NonNull Identifiable<Integer> mockIdentifiable(@Nullable Integer id) {

    Identifiable<Integer> mockIdentifiable = mock(Identifiable.class, withSettings().strictness(Strictness.LENIENT));

    doCallRealMethod().when(mockIdentifiable).isNew();
    doCallRealMethod().when(mockIdentifiable).identifiedBy(anyInt());
    doReturn(id).when(mockIdentifiable).getId();

    return mockIdentifiable;
  }

  @Test
  public void identifyIsNullSafe() {
    assertThat(getIdentifierSequence().<Identifiable<Integer>>identify(null)).isNull();
  }

  @Test
  public void setsIdOnNewIdentifiableObject() {

    Identifiable<Integer> mockIdentifiable = mockIdentifiable(null);

    assertThat(getIdentifierSequence().identify(mockIdentifiable)).isSameAs(mockIdentifiable);

    verify(mockIdentifiable, times(1)).isNew();
    verify(mockIdentifiable, times(1)).getId();
    verify(mockIdentifiable, times(1)).identifiedBy(eq(1));
    verify(mockIdentifiable, times(1)).setId(eq(1));
    verifyNoMoreInteractions(mockIdentifiable);
  }

  @Test
  public void willNotSetIdOnNonNewIdentifiableObject() {

    Identifiable<Integer> mockIdentifiable = mockIdentifiable(-1);

    assertThat(getIdentifierSequence().identify(mockIdentifiable)).isSameAs(mockIdentifiable);

    verify(mockIdentifiable, times(1)).isNew();
    verify(mockIdentifiable, times(1)).getId();
    verify(mockIdentifiable, never()).identifiedBy(anyInt());
    verify(mockIdentifiable, never()).setId(anyInt());
    verifyNoMoreInteractions(mockIdentifiable);
  }

  static class TestIdentifierSequence implements IdentifierSequence<Integer> {

    private static final AtomicInteger identifier = new AtomicInteger(0);

    @Override
    public Integer nextId() {
      return identifier.incrementAndGet();
    }
  }
}
