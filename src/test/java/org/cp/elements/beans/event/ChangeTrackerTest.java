/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.beans.event;

import static org.junit.Assert.*;

import java.beans.PropertyChangeEvent;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.junit.Test;

/**
 * The ChangeTrackerTest class is a test suite of test cases testing the contract and functionality of the
 * ChangeTracker class.
 * <p/>
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
