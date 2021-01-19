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
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import org.cp.elements.test.TestUtils;
import org.junit.Before;
import org.junit.Test;

/**
 * Unit Tests for {@link ChangeSupport}.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.beans.event.ChangeSupport
 * @see org.cp.elements.test.TestUtils
 * @since 1.0.0
 */
public class ChangeSupportTests {

  private Object source = new Object();

  private ChangeSupport changeSupport;

  @Before
  public void setup() {
    this.changeSupport = new ChangeSupport(source);
  }

  @Test
  public void createChangeSupport() {

    ChangeSupport changeSupport = new ChangeSupport(source);

    assertThat(changeSupport).isNotNull();
    assertThat(changeSupport.getSource()).isSameAs(source);
    assertThat(changeSupport.hasListeners()).isFalse();
    assertThat(changeSupport.size()).isEqualTo(0);
  }

  @Test(expected = IllegalArgumentException.class)
  public void createChangeSupportWithNullSource() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(() -> new ChangeSupport(null),
      () -> "Source cannot be null");
  }

  @Test
  public void addChangeListener() {

    ChangeListener mockChangeListener = mock(ChangeListener.class);

    assertThat(changeSupport.add(mockChangeListener)).isTrue();
    assertThat(changeSupport.contains(mockChangeListener)).isTrue();
  }

  @Test
  public void addChangeListenerTwiceIsAllowed() {

    ChangeListener mockChangeListener = mock(ChangeListener.class);

    assertThat(changeSupport.add(mockChangeListener)).isTrue();
    assertThat(changeSupport.contains(mockChangeListener)).isTrue();
    assertThat(changeSupport.size()).isEqualTo(1);
    assertThat(changeSupport.add(mockChangeListener)).isTrue();
    assertThat(changeSupport.contains(mockChangeListener)).isTrue();
    assertThat(changeSupport.size()).isEqualTo(2);
  }

  @Test
  public void addNullChangeListener() {

    assertThat(changeSupport.add(null)).isFalse();
    assertThat(changeSupport.contains(null)).isFalse();
    assertThat(changeSupport.hasListeners()).isFalse();
    assertThat(changeSupport.size()).isEqualTo(0);
  }

  @Test
  public void containsChangeListener()  {

    ChangeListener mockChangeListener = mock(ChangeListener.class);

    assertThat(changeSupport.contains(mockChangeListener)).isFalse();
    assertThat(changeSupport.add(mockChangeListener)).isTrue();
    assertThat(changeSupport.contains(mockChangeListener)).isTrue();
    assertThat(changeSupport.remove(mockChangeListener)).isTrue();
    assertThat(changeSupport.contains(mockChangeListener)).isFalse();
  }

  @Test
  public void createChangeEvent() {

    Object alternateSource = new Object();

    ChangeEvent changeEvent = changeSupport.createChangeEvent(alternateSource);

    assertThat(changeEvent).isNotNull();
    assertThat(changeEvent.getSource()).isSameAs(alternateSource);
    assertThat(changeEvent.getSource()).isNotEqualTo(changeSupport.getSource());
  }

  @Test
  public void fireChangeEvent() {

    ChangeListener mockChangeListenerOne = mock(ChangeListener.class, "MockChangeListenerOne");
    ChangeListener mockChangeListenerTwo = mock(ChangeListener.class, "MockChangeListenerTwo");

    assertThat(changeSupport.add(mockChangeListenerOne)).isTrue();
    assertThat(changeSupport.add(mockChangeListenerTwo)).isTrue();

    changeSupport.fireChangeEvent();

    verify(mockChangeListenerOne, times(1)).stateChanged(isA(ChangeEvent.class));
    verify(mockChangeListenerTwo, times(1)).stateChanged(isA(ChangeEvent.class));
  }

  @Test
  public void hasListeners() {

    ChangeListener mockChangeListener = mock(ChangeListener.class);

    assertThat(changeSupport.hasListeners()).isFalse();
    assertThat(changeSupport.add(null)).isFalse();
    assertThat(changeSupport.hasListeners()).isFalse();
    assertThat(changeSupport.add(mockChangeListener)).isTrue();
    assertThat(changeSupport.hasListeners()).isTrue();
    assertThat(changeSupport.remove(null)).isFalse();
    assertThat(changeSupport.hasListeners()).isTrue();
    assertThat(changeSupport.remove(mockChangeListener)).isTrue();
    assertThat(changeSupport.hasListeners()).isFalse();
  }

  @Test
  public void iterator() {

    ChangeListener mockChangeListenerOne = mock(ChangeListener.class, "MockChangeListenerOne");
    ChangeListener mockChangeListenerTwo = mock(ChangeListener.class, "MockChangeListenerTwo");
    ChangeListener mockChangeListenerThree = mock(ChangeListener.class, "MockChangeListenerThree");

    assertThat(changeSupport.add(mockChangeListenerOne)).isTrue();
    assertThat(changeSupport.add(mockChangeListenerTwo)).isTrue();
    assertThat(changeSupport.add(mockChangeListenerThree)).isTrue();
    assertThat(changeSupport.hasListeners()).isTrue();
    assertThat(changeSupport.size()).isEqualTo(3);

    List<ChangeListener> expectedChangeListeners = Arrays.asList(mockChangeListenerOne, mockChangeListenerTwo,
      mockChangeListenerThree);

    int index = 0;

    for (ChangeListener changeListener : changeSupport) {
      assertThat(changeListener).isEqualTo(expectedChangeListeners.get(index++));
    }

    assertThat(index).isEqualTo(3);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void immutableIterator() {

    ChangeListener mockChangeListener = mock(ChangeListener.class);

    assertThat(changeSupport.add(mockChangeListener)).isTrue();
    assertThat(changeSupport.contains(mockChangeListener)).isTrue();

    Iterator<ChangeListener> iterator = changeSupport.iterator();

    assertThat(iterator).isNotNull();
    assertThat(iterator.hasNext()).isTrue();
    assertThat(iterator.next()).isEqualTo(mockChangeListener);

    try {
      iterator.remove();
    }
    catch (UnsupportedOperationException expected) {

      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      assertThat(changeSupport.contains(mockChangeListener)).isTrue();
    }
  }

  @Test
  public void remove() {

    ChangeListener mockChangeListener = mock(ChangeListener.class);

    assertThat(changeSupport.contains(mockChangeListener)).isFalse();
    assertThat(changeSupport.remove(mockChangeListener)).isFalse();
    assertThat(changeSupport.add(mockChangeListener)).isTrue();
    assertThat(changeSupport.contains(mockChangeListener)).isTrue();
    assertThat(changeSupport.remove(null)).isFalse();
    assertThat(changeSupport.contains(mockChangeListener)).isTrue();
    assertThat(changeSupport.remove(mockChangeListener)).isTrue();
    assertThat(changeSupport.remove(mockChangeListener)).isFalse();
    assertThat(changeSupport.contains(mockChangeListener)).isFalse();
  }

  @Test
  public void size() {

    ChangeListener mockChangeListenerOne = mock(ChangeListener.class);
    ChangeListener mockChangeListenerTwo = mock(ChangeListener.class);

    assertThat(changeSupport.size()).isEqualTo(0);
    assertThat(changeSupport.add(null)).isFalse();
    assertThat(changeSupport.size()).isEqualTo(0);
    assertThat(changeSupport.add(mockChangeListenerOne)).isTrue();
    assertThat(changeSupport.size()).isEqualTo(1);
    assertThat(changeSupport.add(mockChangeListenerTwo)).isTrue();
    assertThat(changeSupport.size()).isEqualTo(2);
    assertThat(changeSupport.add(mockChangeListenerOne)).isTrue();
    assertThat(changeSupport.size()).isEqualTo(3);
    assertThat(changeSupport.remove(null)).isFalse();
    assertThat(changeSupport.size()).isEqualTo(3);
    assertThat(changeSupport.remove(mockChangeListenerOne)).isTrue();
    assertThat(changeSupport.remove(mockChangeListenerTwo)).isTrue();
    assertThat(changeSupport.size()).isEqualTo(1);
    assertThat(changeSupport.remove(mockChangeListenerOne)).isTrue();
    assertThat(changeSupport.size()).isEqualTo(0);
  }
}
