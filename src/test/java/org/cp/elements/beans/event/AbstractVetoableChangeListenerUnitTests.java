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
import static org.cp.elements.lang.ThrowableAssertions.assertThatThrowableOfType;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyVetoException;
import java.util.Arrays;

import org.cp.elements.beans.IllegalPropertyValueException;
import org.cp.elements.lang.Constants;
import org.cp.elements.lang.ThrowableOperation;

import org.junit.Test;

/**
 * Unit Tests for {@link AbstractVetoableChangeListener}.
 *
 * @author John Blum
 * @see java.beans.PropertyChangeEvent
 * @see java.beans.VetoableChangeListener
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.beans.event.AbstractVetoableChangeListener
 * @since 1.0.0
 */
public class AbstractVetoableChangeListenerUnitTests {

  @Test
  public void constructVetoableChangeListenerWithArrayOfPropertyNames() {

    AbstractVetoableChangeListener listener =
      new TestVetoableChangeListener("mockProperty", "testProperty");

    assertThat(listener).isNotNull();
    assertThat(listener).containsExactly("mockProperty", "testProperty");
  }

  @Test
  public void constructVetoableChangeListenerWithIterableOfPropertyNames() {

    AbstractVetoableChangeListener listener =
      new TestVetoableChangeListener(Arrays.asList("mockProperty", "testProperty"));

    assertThat(listener).isNotNull();
    assertThat(listener).containsExactly("mockProperty", "testProperty");
  }

  @Test
  public void vetoableChangeCallsHandle() throws PropertyVetoException {

    PropertyChangeEvent mockEvent = mock(PropertyChangeEvent.class);

    AbstractVetoableChangeListener listener =
      spy(new TestVetoableChangeListener(Arrays.asList("mockProperty", "testProperty")));

    doNothing().when(listener).handle(any(PropertyChangeEvent.class));

    listener.vetoableChange(mockEvent);

    verify(listener, times(1)).vetoableChange(eq(mockEvent));
    verify(listener, times(1)).handle(eq(mockEvent));
    verifyNoInteractions(mockEvent);
  }

  @Test
  public void vetoableChangeHandlesBeansException() throws PropertyVetoException {

    PropertyChangeEvent mockEvent = mock(PropertyChangeEvent.class);

    AbstractVetoableChangeListener listener =
      spy(new TestVetoableChangeListener(Arrays.asList("mockProperty", "testProperty")));

    doThrow(new IllegalPropertyValueException("TEST")).when(listener).handle(any(PropertyChangeEvent.class));

    assertThatThrowableOfType(PropertyVetoException.class)
      .isThrownBy(ThrowableOperation.from(args -> listener.vetoableChange(mockEvent)))
      .havingMessage("Error occurred while processing event [%s]", mockEvent)
      .causedBy(IllegalPropertyValueException.class)
      .havingMessage("TEST")
      .withNoCause();

    verify(listener, times(1)).vetoableChange(eq(mockEvent));
    verify(listener, times(1)).handle(eq(mockEvent));
    verifyNoInteractions(mockEvent);
  }

  static class TestVetoableChangeListener extends AbstractVetoableChangeListener {

    TestVetoableChangeListener(String... propertyNames) {
      super(propertyNames);
    }

    TestVetoableChangeListener(Iterable<String> propertyNames) {
      super(propertyNames);
    }

    @Override
    protected void doHandle(PropertyChangeEvent event) {
      throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
    }
  }
}
