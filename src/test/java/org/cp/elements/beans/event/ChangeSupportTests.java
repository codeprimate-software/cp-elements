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

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertThat;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for {@link ChangeSupport}.
 *
 * @author John J. Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.junit.rules.ExpectedException
 * @see org.cp.elements.beans.event.ChangeSupport
 * @see org.cp.elements.lang.DateTimeUtils
 * @since 1.0.0
 */
public class ChangeSupportTests {

  @Rule
  public ExpectedException exception = ExpectedException.none();

  private Object source = new Object();

  private ChangeSupport changeSupport;

  @Before
  public void setup() {
    this.changeSupport = new ChangeSupport(source);
  }

  @Test
  public void createChangeSupport() {

    ChangeSupport changeSupport = new ChangeSupport(source);

    assertThat(changeSupport, is(notNullValue(ChangeSupport.class)));
    assertThat(changeSupport.getSource(), is(sameInstance(source)));
    assertThat(changeSupport.hasListeners(), is(false));
    assertThat(changeSupport.size(), is(equalTo(0)));
  }

  @Test
  public void createChangeSupportWithNullSource() {

    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Source cannot be null");

    new ChangeSupport(null);
  }

  @Test
  public void addChangeListener() {

    ChangeListener mockChangeListener = mock(ChangeListener.class);

    assertThat(changeSupport.add(mockChangeListener), is(true));
    assertThat(changeSupport.contains(mockChangeListener), is(true));
  }

  @Test
  public void addChangeListenerTwiceIsAllowed() {

    ChangeListener mockChangeListener = mock(ChangeListener.class);

    assertThat(changeSupport.add(mockChangeListener), is(true));
    assertThat(changeSupport.contains(mockChangeListener), is(true));
    assertThat(changeSupport.size(), is(equalTo(1)));
    assertThat(changeSupport.add(mockChangeListener), is(true));
    assertThat(changeSupport.contains(mockChangeListener), is(true));
    assertThat(changeSupport.size(), is(equalTo(2)));
  }

  @Test
  public void addNullChangeListener() {

    assertThat(changeSupport.add(null), is(false));
    assertThat(changeSupport.contains(null), is(false));
    assertThat(changeSupport.hasListeners(), is(false));
    assertThat(changeSupport.size(), is(equalTo(0)));
  }

  @Test
  public void containsChangeListener()  {

    ChangeListener mockChangeListener = mock(ChangeListener.class);

    assertThat(changeSupport.contains(mockChangeListener), is(false));
    assertThat(changeSupport.add(mockChangeListener), is(true));
    assertThat(changeSupport.contains(mockChangeListener), is(true));
    assertThat(changeSupport.remove(mockChangeListener), is(true));
    assertThat(changeSupport.contains(mockChangeListener), is(false));
  }

  @Test
  public void createChangeEvent() {

    Object alternateSource = new Object();

    ChangeEvent changeEvent = changeSupport.createChangeEvent(alternateSource);

    assertThat(changeEvent, is(notNullValue(ChangeEvent.class)));
    assertThat(changeEvent.getSource(), is(sameInstance(alternateSource)));
    assertThat(changeEvent.getSource(), is(not(equalTo(changeSupport.getSource()))));
  }

  @Test
  public void fireChangeEvent() {

    ChangeListener mockChangeListenerOne = mock(ChangeListener.class, "MockChangeListenerOne");
    ChangeListener mockChangeListenerTwo = mock(ChangeListener.class, "MockChangeListenerTwo");

    assertThat(changeSupport.add(mockChangeListenerOne), is(true));
    assertThat(changeSupport.add(mockChangeListenerTwo), is(true));

    changeSupport.fireChangeEvent();

    verify(mockChangeListenerOne, times(1)).stateChanged(isA(ChangeEvent.class));
    verify(mockChangeListenerTwo, times(1)).stateChanged(isA(ChangeEvent.class));
  }

  @Test
  public void hasListeners() {

    ChangeListener mockChangeListener = mock(ChangeListener.class);

    assertThat(changeSupport.hasListeners(), is(false));
    assertThat(changeSupport.add(null), is(false));
    assertThat(changeSupport.hasListeners(), is(false));
    assertThat(changeSupport.add(mockChangeListener), is(true));
    assertThat(changeSupport.hasListeners(), is(true));
    assertThat(changeSupport.remove(null), is(false));
    assertThat(changeSupport.hasListeners(), is(true));
    assertThat(changeSupport.remove(mockChangeListener), is(true));
    assertThat(changeSupport.hasListeners(), is(false));
  }

  @Test
  public void iterator() {

    ChangeListener mockChangeListenerOne = mock(ChangeListener.class, "MockChangeListenerOne");
    ChangeListener mockChangeListenerTwo = mock(ChangeListener.class, "MockChangeListenerTwo");
    ChangeListener mockChangeListenerThree = mock(ChangeListener.class, "MockChangeListenerThree");

    assertThat(changeSupport.add(mockChangeListenerOne), is(true));
    assertThat(changeSupport.add(mockChangeListenerTwo), is(true));
    assertThat(changeSupport.add(mockChangeListenerThree), is(true));
    assertThat(changeSupport.hasListeners(), is(true));
    assertThat(changeSupport.size(), is(equalTo(3)));

    List<ChangeListener> expectedChangeListeners = Arrays.asList(mockChangeListenerOne, mockChangeListenerTwo,
      mockChangeListenerThree);

    int index = 0;

    for (ChangeListener changeListener : changeSupport) {
      assertThat(changeListener, is(equalTo(expectedChangeListeners.get(index++))));
    }

    assertThat(index, is(equalTo(3)));
  }

  @Test
  public void immutableIterator() {

    ChangeListener mockChangeListener = mock(ChangeListener.class);

    assertThat(changeSupport.add(mockChangeListener), is(true));
    assertThat(changeSupport.contains(mockChangeListener), is(true));

    Iterator<ChangeListener> iterator = changeSupport.iterator();

    assertThat(iterator, is(notNullValue(Iterator.class)));
    assertThat(iterator.hasNext(), is(true));
    assertThat(iterator.next(), is(equalTo(mockChangeListener)));

    try {
      exception.expect(UnsupportedOperationException.class);
      exception.expectCause(is(nullValue(Throwable.class)));

      iterator.remove();
    }
    finally {
      assertThat(changeSupport.contains(mockChangeListener), is(true));
    }
  }

  @Test
  public void remove() {

    ChangeListener mockChangeListener = mock(ChangeListener.class);

    assertThat(changeSupport.contains(mockChangeListener), is(false));
    assertThat(changeSupport.remove(mockChangeListener), is(false));
    assertThat(changeSupport.add(mockChangeListener), is(true));
    assertThat(changeSupport.contains(mockChangeListener), is(true));
    assertThat(changeSupport.remove(null), is(false));
    assertThat(changeSupport.contains(mockChangeListener), is(true));
    assertThat(changeSupport.remove(mockChangeListener), is(true));
    assertThat(changeSupport.remove(mockChangeListener), is(false));
    assertThat(changeSupport.contains(mockChangeListener), is(false));
  }

  @Test
  public void size() {

    ChangeListener mockChangeListenerOne = mock(ChangeListener.class);
    ChangeListener mockChangeListenerTwo = mock(ChangeListener.class);

    assertThat(changeSupport.size(), is(equalTo(0)));
    assertThat(changeSupport.add(null), is(false));
    assertThat(changeSupport.size(), is(equalTo(0)));
    assertThat(changeSupport.add(mockChangeListenerOne), is(true));
    assertThat(changeSupport.size(), is(equalTo(1)));
    assertThat(changeSupport.add(mockChangeListenerTwo), is(true));
    assertThat(changeSupport.size(), is(equalTo(2)));
    assertThat(changeSupport.add(mockChangeListenerOne), is(true));
    assertThat(changeSupport.size(), is(equalTo(3)));
    assertThat(changeSupport.remove(null), is(false));
    assertThat(changeSupport.size(), is(equalTo(3)));
    assertThat(changeSupport.remove(mockChangeListenerOne), is(true));
    assertThat(changeSupport.remove(mockChangeListenerTwo), is(true));
    assertThat(changeSupport.size(), is(equalTo(1)));
    assertThat(changeSupport.remove(mockChangeListenerOne), is(true));
    assertThat(changeSupport.size(), is(equalTo(0)));
  }
}
