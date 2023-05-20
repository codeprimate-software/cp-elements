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
package org.cp.elements.beans.event;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;

import org.junit.jupiter.api.Test;

import org.mockito.InOrder;
import org.mockito.stubbing.Answer;

/**
 * Unit Tests for {@link ChangeSupport}.
 *
 * @author John J. Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.mockito.Spy
 * @see org.cp.elements.beans.event.ChangeSupport
 * @since 1.0.0
 */
public class ChangeSupportUnitTests {

  @Test
  public void constructNewChangeSupportObject() {

    Object source = new Object();

    ChangeSupport changeSupport = new ChangeSupport(source);

    assertThat(changeSupport).isNotNull();
    assertThat(changeSupport.getChangeListeners()).isEmpty();
    assertThat(changeSupport.getSource()).isSameAs(source);
  }

  @Test
  public void constructNewChangeSupportWithNullSource() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new ChangeSupport(null))
      .withMessage("A source object is required")
      .withNoCause();
  }

  @Test
  public void containsNullChangeListenerIsNullSafeAndReturnsFalse() {

    ChangeSupport changeSupport = spy(new ChangeSupport(new Object()));

    assertThat(changeSupport.contains(null)).isFalse();

    verify(changeSupport, times(1)).contains(isNull());
    verifyNoMoreInteractions(changeSupport);
  }

  @Test
  public void containsRegisteredChangeListenerReturnsTrue() {

    ChangeListener mockChangeListener = mock(ChangeListener.class);

    ChangeSupport changeSupport = spy(new ChangeSupport(new Object()));

    doReturn(Collections.singletonList(mockChangeListener)).when(changeSupport).getChangeListeners();

    assertThat(changeSupport.contains(mockChangeListener)).isTrue();

    verify(changeSupport, times(1)).contains(eq(mockChangeListener));
    verify(changeSupport, times(1)).getChangeListeners();
    verifyNoMoreInteractions(changeSupport);
    verifyNoInteractions(mockChangeListener);
  }

  @Test
  public void containsUnregisteredChangeListenerReturnsFalse() {

    ChangeListener mockChangeListener = mock(ChangeListener.class);

    ChangeSupport changeSupport = spy(new ChangeSupport(new Object()));

    assertThat(changeSupport.contains(mockChangeListener)).isFalse();

    verify(changeSupport, times(1)).contains(eq(mockChangeListener));
    verify(changeSupport, times(1)).getChangeListeners();
    verifyNoMoreInteractions(changeSupport);
    verifyNoInteractions(mockChangeListener);
  }

  @Test
  public void countReturnsZero() {

    ChangeSupport changeSupport = spy(new ChangeSupport(new Object()));

    assertThat(changeSupport.count()).isZero();

    verify(changeSupport, times(1)).count();
    verify(changeSupport, times(1)).getChangeListeners();
    verifyNoMoreInteractions(changeSupport);
  }

  @Test
  public void countReturnsOne() {

    ChangeListener mockChangeListener = mock(ChangeListener.class);

    ChangeSupport changeSupport = spy(new ChangeSupport(new Object()));

    doReturn(Collections.singletonList(mockChangeListener)).when(changeSupport).getChangeListeners();

    assertThat(changeSupport.count()).isOne();

    verify(changeSupport, times(1)).count();
    verify(changeSupport, times(1)).getChangeListeners();
    verifyNoMoreInteractions(changeSupport);
    verifyNoInteractions(mockChangeListener);
  }

  @Test
  public void countReturnsTwo() {

    ChangeListener mockChangeListenerOne = mock(ChangeListener.class);
    ChangeListener mockChangeListenerTwo = mock(ChangeListener.class);

    ChangeSupport changeSupport = spy(new ChangeSupport(new Object()));

    doReturn(Arrays.asList(mockChangeListenerOne, mockChangeListenerTwo))
      .when(changeSupport).getChangeListeners();

    assertThat(changeSupport.count()).isEqualTo(2);

    verifyNoInteractions(mockChangeListenerOne, mockChangeListenerTwo);
    verify(changeSupport, times(1)).count();
    verify(changeSupport, times(1)).getChangeListeners();
    verifyNoMoreInteractions(changeSupport);
  }

  @Test
  public void constructNewChangeEvent() {

    Object source = new Object();
    Object target = new Object();

    ChangeSupport changeSupport = new ChangeSupport(source);

    assertThat(changeSupport).isNotNull();
    assertThat(changeSupport.getSource()).isEqualTo(source);

    ChangeEvent event = changeSupport.newChangeEvent(target);

    assertThat(event).isNotNull();
    assertThat(event.getSource()).isEqualTo(target);
  }

  @Test
  public void hasListenersReturnsTrue() {

    ChangeSupport changeSupport = spy(new ChangeSupport(new Object()));

    doReturn(1).doReturn(2).when(changeSupport).count();

    assertThat(changeSupport.hasListeners()).isTrue();
    assertThat(changeSupport.hasListeners()).isTrue();

    verify(changeSupport, times(2)).hasListeners();
    verify(changeSupport, times(2)).count();
    verifyNoMoreInteractions(changeSupport);
  }

  @Test
  public void hasListenersReturnsFalse() {

    ChangeSupport changeSupport = spy(new ChangeSupport(new Object()));

    doReturn(0).doReturn(-1).when(changeSupport).count();

    assertThat(changeSupport.hasListeners()).isFalse();
    assertThat(changeSupport.hasListeners()).isFalse();

    verify(changeSupport, times(2)).hasListeners();
    verify(changeSupport, times(2)).count();
    verifyNoMoreInteractions(changeSupport);
  }

  @Test
  public void iteratesOverChangeListeners() {

    ChangeListener mockChangeListenerOne = mock(ChangeListener.class);
    ChangeListener mockChangeListenerTwo = mock(ChangeListener.class);

    ChangeSupport changeSupport = spy(new ChangeSupport(new Object()));

    doReturn(Arrays.asList(mockChangeListenerOne, mockChangeListenerTwo))
      .when(changeSupport).getChangeListeners();

    assertThat(changeSupport).containsExactly(mockChangeListenerOne, mockChangeListenerTwo);

    verifyNoInteractions(mockChangeListenerOne, mockChangeListenerTwo);
    verify(changeSupport, times(1)).iterator();
  }

  @Test
  public void registerMockChangeListenerIsSuccessful() {

    ChangeListener mockChangeListener = mock(ChangeListener.class);

    ChangeSupport changeSupport = spy(new ChangeSupport(new Object()));

    assertThat(changeSupport.register(mockChangeListener)).isSameAs(changeSupport);
    assertThat(changeSupport).containsExactly(mockChangeListener);

    verify(changeSupport, times(1)).register(eq(mockChangeListener));
    verify(changeSupport, times(2)).getChangeListeners();
    verify(changeSupport, times(1)).iterator();
    verifyNoInteractions(mockChangeListener);
  }

  @Test
  public void registerNullChangeListenerIsNullSafe() {

    ChangeSupport changeSupport = spy(new ChangeSupport(new Object()));

    assertThat(changeSupport.register(null)).isSameAs(changeSupport);
    assertThat(changeSupport).isEmpty();

    verify(changeSupport, times(1)).register(isNull());
    verify(changeSupport, times(1)).getChangeListeners();
    verify(changeSupport, times(1)).iterator();
    verifyNoMoreInteractions(changeSupport);
  }

  @Test
  public void unregisterMockChangeListenerIsSuccessful() {

    ChangeListener mockChangeListener = mock(ChangeListener.class);

    ChangeSupport changeSupport = spy(new ChangeSupport(new Object()));

    doReturn(new ArrayList<>(Collections.singletonList(mockChangeListener)))
      .when(changeSupport).getChangeListeners();

    assertThat(changeSupport.unregister(mockChangeListener)).isSameAs(changeSupport);
    assertThat(changeSupport).isEmpty();

    verify(changeSupport, times(1)).unregister(eq(mockChangeListener));
    verify(changeSupport, times(2)).getChangeListeners();
    verify(changeSupport, times(1)).iterator();
    verifyNoMoreInteractions(changeSupport);
    verifyNoInteractions(mockChangeListener);
  }

  @Test
  public void unregisterNullChangeListenerIsNullSafe() {

    ChangeSupport changeSupport = spy(new ChangeSupport(new Object()));

    assertThat(changeSupport.unregister(null)).isSameAs(changeSupport);
    assertThat(changeSupport).isEmpty();

    verify(changeSupport, times(1)).unregister(isNull());
    verify(changeSupport, times(1)).getChangeListeners();
    verify(changeSupport, times(1)).iterator();
    verifyNoMoreInteractions(changeSupport);
  }

  @Test
  public void unregisterNonRegisteredChangeListenerHasNoEffect() {

    ChangeListener mockChangeListener = mock(ChangeListener.class);

    ChangeSupport changeSupport = spy(new ChangeSupport(new Object()));

    assertThat(changeSupport.unregister(mockChangeListener)).isSameAs(changeSupport);
    assertThat(changeSupport).isEmpty();

    verify(changeSupport, times(1)).unregister(eq(mockChangeListener));
    verify(changeSupport, times(2)).getChangeListeners();
    verify(changeSupport, times(1)).iterator();
    verifyNoMoreInteractions(changeSupport);
    verifyNoInteractions(mockChangeListener);
  }

  @Test
  @SuppressWarnings("all")
  public void fireChangeEventNotifiesListeners() {

    Object source = new Object();

    Instant now = Instant.now();

    ChangeListener mockChangeListenerOne = mock(ChangeListener.class);
    ChangeListener mockChangeListenerTwo = mock(ChangeListener.class);

    Answer<ChangeEvent> answer = invocation -> {

      ChangeEvent event = invocation.getArgument(0);

      assertThat(event).isNotNull();
      assertThat(event.getSource()).isEqualTo(source);
      assertThat(event.getChangeDateTime()).isAfterOrEqualTo(now);

      return null;
    };

    doAnswer(answer).when(mockChangeListenerOne).stateChanged(isA(ChangeEvent.class));
    doAnswer(answer).when(mockChangeListenerTwo).stateChanged(isA(ChangeEvent.class));

    ChangeSupport changeSupport = spy(new ChangeSupport(source));

    doReturn(Arrays.asList(mockChangeListenerOne, mockChangeListenerTwo))
      .when(changeSupport).getChangeListeners();

    changeSupport.fireChangeEvent();

    InOrder order = inOrder(changeSupport, mockChangeListenerOne, mockChangeListenerTwo);

    order.verify(changeSupport, times(1)).fireChangeEvent();
    order.verify(changeSupport, times(1)).hasListeners();
    order.verify(changeSupport, times(1)).count();
    order.verify(changeSupport, times(1)).getChangeListeners();
    order.verify(changeSupport, times(1)).getSource();
    order.verify(changeSupport, times(1)).newChangeEvent(eq(source));
    order.verify(changeSupport, times(1)).iterator();
    order.verify(changeSupport, times(1)).getChangeListeners();
    order.verify(mockChangeListenerOne, times(1)).stateChanged(isA(ChangeEvent.class));
    order.verify(mockChangeListenerTwo, times(1)).stateChanged(isA(ChangeEvent.class));
    verifyNoMoreInteractions(mockChangeListenerOne, mockChangeListenerTwo);
  }

  @Test
  public void fireChangeEventWhenNoListenersAreRegistered() {

    ChangeSupport changeSupport = spy(new ChangeSupport(new Object()));

    changeSupport.fireChangeEvent();

    verify(changeSupport, times(1)).fireChangeEvent();
    verify(changeSupport, times(1)).hasListeners();
    verify(changeSupport, times(1)).count();
    verify(changeSupport, times(1)).getChangeListeners();
    verify(changeSupport, never()).newChangeEvent(any());
    verifyNoMoreInteractions(changeSupport);
  }
}
