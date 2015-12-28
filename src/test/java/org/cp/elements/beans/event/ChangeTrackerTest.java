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

package org.cp.elements.beans.event;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.beans.PropertyChangeEvent;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.junit.Test;

/**
 * The ChangeTrackerTest class is a test suite of test cases testing the contract and functionality of the
 * ChangeTracker class.
 *
 * @author John J. Blum
 * @see org.cp.elements.beans.event.ChangeTracker
 * @see org.junit.Assert
 * @see org.junit.Test
 * @since 1.0.0
 */
public class ChangeTrackerTest {

  private static final Object SOURCE = new Object();

  protected final PropertyChangeEvent createPropertyChangeEvent(final String propertyName,
                                                                final Object oldValue,
                                                                final Object newValue)
  {
    return new PropertyChangeEvent(SOURCE, propertyName, oldValue, newValue);
  }

  @Test
  public void testPropertyChange() {
    final ChangeTracker tracker = new ChangeTracker();

    assertNotNull(tracker);
    assertFalse(tracker.isModified());

    tracker.propertyChange(createPropertyChangeEvent("myProperty", null, "test"));

    assertTrue(tracker.isModified());
    assertTrue(tracker.isModified("myProperty"));
    assertFalse(tracker.isModified("yourProperty"));

    tracker.propertyChange(createPropertyChangeEvent("myProperty", "test", "testing"));

    assertTrue(tracker.isModified());
    assertTrue(tracker.isModified("myProperty"));
    assertFalse(tracker.isModified("yourProperty"));

    tracker.propertyChange(createPropertyChangeEvent("myProperty", "testing", "tested"));

    assertTrue(tracker.isModified());
    assertTrue(tracker.isModified("myProperty"));
    assertFalse(tracker.isModified("yourProperty"));

    tracker.propertyChange(createPropertyChangeEvent("myProperty", "tested", null));

    assertFalse(tracker.isModified());
    assertFalse(tracker.isModified("myProperty"));
    assertFalse(tracker.isModified("yourProperty"));

    tracker.propertyChange(createPropertyChangeEvent("yourProperty", null, null));

    assertFalse(tracker.isModified());
    assertFalse(tracker.isModified("myProperty"));
    assertFalse(tracker.isModified("yourProperty"));
  }

  @Test
  public void testIterator()  {
    final ChangeTracker tracker = new ChangeTracker();

    assertNotNull(tracker);
    assertFalse(tracker.isModified());

    tracker.propertyChange(createPropertyChangeEvent("propertyOne", null, "test"));
    tracker.propertyChange(createPropertyChangeEvent("propertyTwo", null, null));
    tracker.propertyChange(createPropertyChangeEvent("propertyThree", "null", null));
    tracker.propertyChange(createPropertyChangeEvent("propertyFour", "null", "null"));
    tracker.propertyChange(createPropertyChangeEvent("propertyFive", "null", "nil"));

    assertTrue(tracker.isModified());
    assertTrue(tracker.isModified("propertyOne"));
    assertFalse(tracker.isModified("propertyTwo"));
    assertTrue(tracker.isModified("propertyThree"));
    assertFalse(tracker.isModified("propertyFour"));
    assertTrue(tracker.isModified("propertyFive"));

    final Set<String> actualProperties = new HashSet<String>(3);

    for (final String property : tracker) {
      actualProperties.add(property);
    }

    assertFalse(actualProperties.isEmpty());
    assertEquals(3, actualProperties.size());
    assertTrue(actualProperties.contains("propertyOne"));
    assertFalse(actualProperties.contains("propertyTwo"));
    assertTrue(actualProperties.contains("propertyThree"));
    assertFalse(actualProperties.contains("propertyFour"));
    assertTrue(actualProperties.contains("propertyFive"));
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testIteratorUnmodifiable() {
    final ChangeTracker tracker = new ChangeTracker();

    assertNotNull(tracker);
    assertFalse(tracker.isModified());

    tracker.propertyChange(createPropertyChangeEvent("propertyOne", null, "test"));
    tracker.propertyChange(createPropertyChangeEvent("propertyTwo", null, null));

    assertTrue(tracker.isModified());
    assertTrue(tracker.isModified("propertyOne"));
    assertFalse(tracker.isModified("propertyTwo"));

    final Iterator<String> it = tracker.iterator();

    assertNotNull(it);
    assertTrue(it.hasNext());

    it.remove();
  }

}
