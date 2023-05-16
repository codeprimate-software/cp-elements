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
import static org.cp.elements.util.ArrayUtils.asIterator;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.Iterator;

import org.junit.jupiter.api.Test;

/**
 * Unit tests for {@link Registry}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.Registry
 * @since 1.0.0
 */
public class RegistryTests {

  @Test
  @SuppressWarnings("unchecked")
  public void isRegisteredWithNullObjectReturnsFalse() {

    Registry<Object> mockRegistry = mock(Registry.class);

    when(mockRegistry.isRegistered(any())).thenCallRealMethod();

    assertThat(mockRegistry.isRegistered(null)).isFalse();

    verify(mockRegistry, never()).iterator();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void isRegisteredWithRegisteredObjectReturnsTrue() {

    Object objectOne = new Object();
    Object objectTwo = new Object();
    Object objectThree = new Object();

    Registry<Object> mockRegistry = mock(Registry.class);

    when(mockRegistry.isRegistered(any())).thenCallRealMethod();
    when(mockRegistry.iterator()).thenReturn(asIterator(objectOne, objectTwo, objectThree));

    assertThat(mockRegistry.isRegistered(objectOne)).isTrue();
    assertThat(mockRegistry.isRegistered(objectTwo)).isTrue();

    verify(mockRegistry, times(2)).iterator();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void isRegisteredWithUnregisteredObjectReturnsFalse() {

    Object object = new Object();

    Registry<Object> mockRegistry = mock(Registry.class);

    when(mockRegistry.isRegistered(any())).thenCallRealMethod();
    when(mockRegistry.iterator()).thenReturn(Collections.emptyIterator());

    assertThat(mockRegistry.isRegistered(object)).isFalse();

    verify(mockRegistry, times(1)).iterator();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void unregisterRegisteredObject() {

    Object registeredObject = new Object();

    Iterator<Object> mockIterator = mock(Iterator.class);

    when(mockIterator.hasNext()).thenReturn(true, false);
    when(mockIterator.next()).thenReturn(registeredObject);

    Registry<Object> mockRegistry = mock(Registry.class);

    when(mockRegistry.unregister(any())).thenCallRealMethod();
    when(mockRegistry.iterator()).thenReturn(mockIterator);

    assertThat(mockRegistry.<Registry>unregister(registeredObject)).isEqualTo(mockRegistry);

    verify(mockIterator, times(2)).hasNext();
    verify(mockIterator, times(1)).next();
    verify(mockIterator, times(1)).remove();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void unregisterUnregisteredObject() {

    Object unregisteredObject = new Object();

    Iterator<Object> mockIterator = mock(Iterator.class);

    when(mockIterator.hasNext()).thenReturn(true, false);
    when(mockIterator.next()).thenReturn("test");

    Registry<Object> mockRegistry = mock(Registry.class);

    when(mockRegistry.unregister(any())).thenCallRealMethod();
    when(mockRegistry.iterator()).thenReturn(mockIterator);

    assertThat(mockRegistry.<Registry>unregister(unregisteredObject)).isEqualTo(mockRegistry);

    verify(mockIterator, times(2)).hasNext();
    verify(mockIterator, times(1)).next();
    verify(mockIterator, never()).remove();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void unregisterWhenNoObjectsAreRegistered() {

    Iterator<Object> mockIterator = mock(Iterator.class);

    when(mockIterator.hasNext()).thenReturn(false);

    Registry<Object> mockRegistry = mock(Registry.class);

    when(mockRegistry.unregister(any())).thenCallRealMethod();
    when(mockRegistry.iterator()).thenReturn(mockIterator);

    assertThat(mockRegistry.<Registry>unregister("test")).isEqualTo(mockRegistry);

    verify(mockIterator, times(1)).hasNext();
    verify(mockIterator, never()).next();
    verify(mockIterator, never()).remove();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void unregisterWithNullObject() {

    Iterator<Object> mockIterator = mock(Iterator.class);

    when(mockIterator.hasNext()).thenReturn(true, false);
    when(mockIterator.next()).thenReturn("null");

    Registry<Object> mockRegistry = mock(Registry.class);

    when(mockRegistry.unregister(any())).thenCallRealMethod();
    when(mockRegistry.iterator()).thenReturn(mockIterator);

    assertThat(mockRegistry.<Registry>unregister(null)).isEqualTo(mockRegistry);

    verify(mockIterator, times(2)).hasNext();
    verify(mockIterator, times(1)).next();
    verify(mockIterator, never()).remove();
  }
}
