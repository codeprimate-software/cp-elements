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
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.beans.PropertyChangeEvent;
import java.util.Arrays;
import java.util.List;

import org.cp.elements.lang.Constants;

import org.junit.Test;
import org.mockito.ArgumentMatchers;

/**
 * Unit Tests for {@link AbstractListener}.
 *
 * @author John Blum
 * @see java.beans.PropertyChangeEvent
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.beans.event.AbstractListener
 * @since 1.0.0
 */
public class AbstractListenerUnitTests {

  @Test
  public void constructsListenerWithArrayOfPropertyNames() {

    String[] propertyNames = { "propertyOne", "propertyTwo" };

    AbstractListener listener = new TestListener(propertyNames);

    assertThat(listener).isNotNull();
    assertThat(listener).containsExactly(propertyNames);
  }

  @Test
  public void constructsListenerWithIterableOfPropertyNames() {

    List<String> propertyNames = Arrays.asList("propertyOne", "propertyTwo");

    AbstractListener listener = new TestListener(propertyNames);

    assertThat(listener).isNotNull();
    assertThat(listener).containsExactly(propertyNames.toArray(new String[0]));
  }

  @Test
  public void constructListenerWithNoPropertyNames() {

    AbstractListener listener = new TestListener();

    assertThat(listener).isNotNull();
    assertThat(listener).isEmpty();
  }

  @Test
  public void constructListenerWithNullArrayOfPropertyNamesIsNullSafe() {

    AbstractListener listener = new TestListener((String[]) null);

    assertThat(listener).isNotNull();
    assertThat(listener).isEmpty();
  }

  @Test
  public void constructListenerWithNullIterableOfPropertyNamesIsNullSafe() {

    AbstractListener listener = new TestListener((Iterable<String>) null);

    assertThat(listener).isNotNull();
    assertThat(listener).isEmpty();
  }

  @Test
  public void handlesPropertyChangeEvent() {

    PropertyChangeEvent mockEvent = mock(PropertyChangeEvent.class);

    AbstractListener listener = spy(new TestListener());

    doReturn(true).when(listener).canHandle(any(PropertyChangeEvent.class));
    doNothing().when(listener).doHandle(any());

    listener.handle(mockEvent);

    verify(listener, times(1)).handle(eq(mockEvent));
    verify(listener, times(1)).canHandle(eq(mockEvent));
    verify(listener, times(1)).doHandle(mockEvent);
    verifyNoMoreInteractions(listener);
    verifyNoInteractions(mockEvent);
  }

  @Test
  public void doesNotHandlePropertyChangeEvent() {

    PropertyChangeEvent mockEvent = mock(PropertyChangeEvent.class);

    AbstractListener listener = spy(new TestListener());

    doReturn(false).when(listener).canHandle(any(PropertyChangeEvent.class));

    listener.handle(mockEvent);

    verify(listener, times(1)).handle(eq(mockEvent));
    verify(listener, times(1)).canHandle(eq(mockEvent));
    verify(listener, never()).doHandle(any());
    verifyNoMoreInteractions(listener);
    verifyNoInteractions(mockEvent);
  }

  @Test
  public void canHandlePropertyChangeEvent() {

    PropertyChangeEvent mockEvent = mock(PropertyChangeEvent.class);

    doReturn("testProperty").when(mockEvent).getPropertyName();

    AbstractListener listener = spy(new TestListener("mockProperty"));

    doReturn(true).when(listener).canHandle(anyString());

    assertThat(listener.canHandle(mockEvent)).isTrue();

    verify(listener, times(1)).canHandle(eq(mockEvent));
    verify(listener, times(1)).canHandle(eq("testProperty"));
    verify(mockEvent, times(1)).getPropertyName();
    verifyNoMoreInteractions(listener, mockEvent);
  }

  @Test
  public void cannotHandlePropertyChangeEvent() {

    PropertyChangeEvent mockEvent = mock(PropertyChangeEvent.class);

    doReturn("testProperty").when(mockEvent).getPropertyName();

    AbstractListener listener = spy(new TestListener("mockProperty"));

    doReturn(false).when(listener).canHandle(anyString());

    assertThat(listener.canHandle(mockEvent)).isFalse();

    verify(listener, times(1)).canHandle(eq(mockEvent));
    verify(listener, times(1)).canHandle(eq("testProperty"));
    verify(mockEvent, times(1)).getPropertyName();
    verifyNoMoreInteractions(listener, mockEvent);
  }

  @Test
  public void cannotHandleNullPropertyChangeEventIsNullSafe() {

    AbstractListener listener = spy(new TestListener());

    assertThat(listener.canHandle((PropertyChangeEvent) null)).isFalse();

    verify(listener, times(1)).canHandle(ArgumentMatchers.<PropertyChangeEvent>isNull());
    verifyNoMoreInteractions(listener);
  }

  @Test
  public void canHandleAnyNamedProperty() {

    AbstractListener listener = spy(new TestListener());

    assertThat(listener.canHandle("mockProperty")).isTrue();
    assertThat(listener.canHandle("nonExistingProperty")).isTrue();
    assertThat(listener.canHandle("testProperty")).isTrue();

    verify(listener, times(1)).canHandle(eq("mockProperty"));
    verify(listener, times(1)).canHandle(eq("nonExistingProperty"));
    verify(listener, times(1)).canHandle(eq("testProperty"));
    verify(listener, times(3)).getPropertyNames();
    verifyNoMoreInteractions(listener);
  }

  @Test
  public void canHandleNamedProperty() {

    AbstractListener listener = spy(new TestListener("mockProperty"));

    assertThat(listener.canHandle("mockProperty")).isTrue();

    verify(listener, times(1)).canHandle(eq("mockProperty"));
    verify(listener, times(1)).getPropertyNames();
    verifyNoMoreInteractions(listener);
  }

  @Test
  public void cannotHandleNamedProperty() {

    AbstractListener listener = spy(new TestListener("mockProperty"));

    assertThat(listener.canHandle("testProperty")).isFalse();

    verify(listener, times(1)).canHandle(eq("testProperty"));
    verify(listener, times(1)).getPropertyNames();
    verifyNoMoreInteractions(listener);
  }

  public void testHandleInvalidPropertyName(String propertyName) {

    AbstractListener listener = spy(new TestListener("mockProperty"));

    assertThat(listener.canHandle(propertyName)).isFalse();

    verify(listener, times(1)).canHandle(ArgumentMatchers.<String>any());
    verify(listener, times(1)).getPropertyNames();
    verifyNoMoreInteractions(listener);
  }

  @Test
  public void cannotHandleNonExitingProperty() {

    testHandleInvalidPropertyName("mockedProperty");
    testHandleInvalidPropertyName("nonExistingProperty");
    testHandleInvalidPropertyName("testProperty");
  }

  @Test
  public void cannotHandleBlankProperty() {
    testHandleInvalidPropertyName("  ");
  }

  @Test
  public void cannotHandleEmptyProperty() {
    testHandleInvalidPropertyName("");
  }

  @Test
  public void cannotHandleNullProperty() {
    testHandleInvalidPropertyName(null);
  }

  static class TestListener extends AbstractListener {

    TestListener(String... propertyNames) {
      super(propertyNames);
    }

    TestListener(Iterable<String> propertyNames) {
      super(propertyNames);
    }

    @Override
    protected void doHandle(PropertyChangeEvent event) {
      throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
    }
  }
}
