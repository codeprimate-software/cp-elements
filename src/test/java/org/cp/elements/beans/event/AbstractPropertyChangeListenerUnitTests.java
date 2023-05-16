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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.beans.PropertyChangeEvent;
import java.util.Arrays;

import org.cp.elements.lang.Constants;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link AbstractPropertyChangeListener}.
 *
 * @author John Blum
 * @see java.beans.PropertyChangeEvent
 * @see java.beans.PropertyChangeListener
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.beans.event.AbstractPropertyChangeListener
 * @since 1.0.0
 */
public class AbstractPropertyChangeListenerUnitTests {

  @Test
  public void constructAbstractPropertyChangeListenerWithArrayOfPropertyNames() {

    AbstractPropertyChangeListener listener =
      new TestPropertyChangeListener("mockProperty", "testProperty");

    assertThat(listener).isNotNull();
    assertThat(listener).containsExactly("mockProperty", "testProperty");
  }

  @Test
  public void constructAbstractPropertyChangeListenerWithIterableOfPropertyNames() {

    AbstractPropertyChangeListener listener =
      new TestPropertyChangeListener(Arrays.asList("mockProperty", "testProperty"));

    assertThat(listener).isNotNull();
    assertThat(listener).containsExactly("mockProperty", "testProperty");
  }

  @Test
  public void propertyChangeEventCallsHandle() {

    PropertyChangeEvent mockEvent = mock(PropertyChangeEvent.class);

    AbstractPropertyChangeListener listener = spy(new TestPropertyChangeListener("mockProperty"));

    doNothing().when(listener).handle(any());

    listener.propertyChange(mockEvent);

    verify(listener, times(1)).propertyChange(eq(mockEvent));
    verify(listener, times(1)).handle(eq(mockEvent));
    verifyNoMoreInteractions(listener);
    verifyNoInteractions(mockEvent);
  }

  static class TestPropertyChangeListener extends AbstractPropertyChangeListener {

    public TestPropertyChangeListener(String... propertyNames) {
      super(propertyNames);
    }

    public TestPropertyChangeListener(Iterable<String> propertyNames) {
      super(propertyNames);
    }

    @Override
    protected void doHandle(PropertyChangeEvent event) {
      throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
    }
  }
}
