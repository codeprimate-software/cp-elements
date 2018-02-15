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
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.concurrent.atomic.AtomicLong;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit tests for the {@link IdentifierSequence}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.IdentifierSequence
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class IdentifierSequenceTests {

  @Mock
  private Identifiable<Long> mockIdentifiable;

  private final IdentifierSequence<Long> identifierSequence = new TestIdentifierSequence();

  @Test
  public void setsIdOnNewIdentifiableObject() {

    when(this.mockIdentifiable.getId()).thenReturn(null);
    when(this.mockIdentifiable.isNew()).thenCallRealMethod();
    when(this.mockIdentifiable.identifiedBy(anyLong())).thenCallRealMethod();

    assertThat(this.identifierSequence.identify(this.mockIdentifiable)).isSameAs(this.mockIdentifiable);

    verify(this.mockIdentifiable, times(1)).isNew();
    verify(this.mockIdentifiable, times(1)).getId();
    verify(this.mockIdentifiable, times(1)).identifiedBy(eq(1L));
    verify(this.mockIdentifiable, times(1)).setId(eq(1L));
  }

  @Test
  public void doesNotSetIdOnNonNewIdentifiableObject() {

    when(this.mockIdentifiable.getId()).thenReturn(-1L);
    when(this.mockIdentifiable.isNew()).thenCallRealMethod();

    assertThat(this.identifierSequence.identify(this.mockIdentifiable)).isSameAs(this.mockIdentifiable);

    verify(this.mockIdentifiable, times(1)).isNew();
    verify(this.mockIdentifiable, times(1)).getId();
    verify(this.mockIdentifiable, never()).identifiedBy(anyLong());
    verify(this.mockIdentifiable, never()).setId(anyLong());
  }

  @Test
  public void identifyIsNullSafe() {
    assertThat(this.identifierSequence.<Identifiable<Long>>identify(null)).isNull();
  }

  static class TestIdentifierSequence implements IdentifierSequence<Long> {

    private static final AtomicLong identifier = new AtomicLong(0L);

    @Override
    public Long nextId() {
      return identifier.incrementAndGet();
    }
  }
}
