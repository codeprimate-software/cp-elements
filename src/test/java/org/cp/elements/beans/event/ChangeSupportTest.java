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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Iterator;
import java.util.List;

import org.cp.elements.lang.DateTimeUtils;
import org.cp.elements.test.AbstractMockingTestSuite;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * The ChangeSupportTest class is a test suite of test cases testing the contract and functionality of the
 * ChangeSupport class.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.beans.event.ChangeSupport
 * @see org.cp.elements.lang.DateTimeUtils
 * @see org.cp.elements.test.AbstractMockingTestSuite
 * @see org.junit.Test
 * @since 1.0.0
 */
public class ChangeSupportTest extends AbstractMockingTestSuite {

  private Object source;

  private ChangeSupport support;

  @Before
  public void setup() {
    source = new Object();
    support = new ChangeSupport(source);
  }

  @After
  public void tearDown() {
    source = support = null;
  }

  @Test
  public void testCreateChangeSupport() {
    final ChangeSupport support = new ChangeSupport(source);

    assertNotNull(support);
    assertSame(source, support.getSource());
    assertFalse(support.hasListeners());
    assertEquals(0, support.size());
  }

  @Test(expected = NullPointerException.class)
  public void testCreateChangeSupportWithNullSource() {
    try {
      new ChangeSupport(null);
    }
    catch (NullPointerException e) {
      assertEquals("The source of the change events cannot be null!", e.getMessage());
      throw e;
    }
  }

  @Test
  public void testAdd() {
    final ChangeListener mockListener = mockContext.mock(ChangeListener.class, "addTest");

    assertTrue(support.add(mockListener));
    assertTrue(support.contains(mockListener));
    assertTrue(support.hasListeners());
    assertEquals(1, support.size());
  }

  @Test
  public void testAddChangeListenerTwice() {
    final ChangeListener mockListener = mockContext.mock(ChangeListener.class, "addChangeListenerTwiceTest");

    assertTrue(support.add(mockListener));
    assertTrue(support.add(mockListener));
    assertTrue(support.contains(mockListener));
    assertTrue(support.hasListeners());
    assertEquals(2, support.size());
    assertTrue(support.remove(mockListener));
    assertTrue(support.contains(mockListener));
    assertTrue(support.hasListeners());
    assertEquals(1, support.size());
  }

  @Test
  public void testAddChangeListenerWithNull() {
    assertFalse(support.add(null));
    assertFalse(support.contains(null));
    assertFalse(support.hasListeners());
    assertEquals(0, support.size());
  }

  @Test
  public void testContains()  {
    final ChangeListener mockListener = mockContext.mock(ChangeListener.class, "containsTest");

    assertFalse(support.contains(mockListener));
    assertTrue(support.add(mockListener));
    assertTrue(support.contains(mockListener));
    assertTrue(support.remove(mockListener));
    assertFalse(support.contains(mockListener));
  }

  @Test
  public void testCreateChangeEvent() {
    final Object mockSource = new Object();

    final ChangeEvent event = support.createChangeEvent(mockSource);

    assertNotNull(event);
    assertSame(mockSource, event.getSource());
    assertFalse(event.getSource().equals(support.getSource()));
  }

  @Test
  public void testFireChangeEvent() {
    final TestChangeListener listenerOne = new TestChangeListener();
    final TestChangeListener listenerTwo = new TestChangeListener();

    assertTrue(support.add(listenerOne));
    assertTrue(support.contains(listenerOne));
    assertTrue(support.add(listenerTwo));
    assertTrue(support.contains(listenerTwo));
    assertFalse(listenerOne.isStateChangedCalled());
    assertFalse(listenerTwo.isStateChangedCalled());

    support.fireChangeEvent();

    assertTrue(listenerOne.isStateChangedCalled());
    assertTrue(listenerTwo.isStateChangedCalled());
  }

  @Test
  public void testHasListeners() {
    final ChangeListener mockListener = mockContext.mock(ChangeListener.class, "hasListenersTest");

    assertFalse(support.hasListeners());
    assertFalse(support.add(null));
    assertFalse(support.hasListeners());
    assertTrue(support.add(mockListener));
    assertTrue(support.hasListeners());
    assertFalse(support.remove(null));
    assertTrue(support.hasListeners());
    assertTrue(support.remove(mockListener));
    assertFalse(support.hasListeners());
  }

  @Test
  public void testIterator() {
    final ChangeListener mockListenerOne = mockContext.mock(ChangeListener.class, "iteratorTestOne");
    final ChangeListener mockListenerTwo = mockContext.mock(ChangeListener.class, "iteratorTestTwo");
    final ChangeListener mockListenerThree = mockContext.mock(ChangeListener.class, "iteratorTestThree");

    final List<ChangeListener> expectedListeners = Arrays.asList(mockListenerOne, mockListenerTwo, mockListenerThree);

    assertTrue(support.add(mockListenerOne));
    assertTrue(support.add(mockListenerTwo));
    assertTrue(support.add(mockListenerThree));

    final List<ChangeListener> actualListeners = new ArrayList<ChangeListener>(3);

    for (ChangeListener listener : support) {
      actualListeners.add(listener);
    }

    assertFalse(actualListeners.isEmpty());
    assertEquals(expectedListeners.size(), actualListeners.size());
    assertTrue(actualListeners.containsAll(expectedListeners));
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testImmutableIterator() {
    final ChangeListener mockListener = mockContext.mock(ChangeListener.class, "immutableIteratorTest");

    assertTrue(support.add(mockListener));
    assertTrue(support.contains(mockListener));
    assertTrue(support.hasListeners());
    assertEquals(1, support.size());

    final Iterator<ChangeListener> it = support.iterator();

    assertNotNull(it);
    assertTrue(it.hasNext());
    assertEquals(mockListener, it.next());

    it.remove();
  }

  @Test
  public void testRemove() {
    final ChangeListener mockListener = mockContext.mock(ChangeListener.class, "removeTest");

    assertFalse(support.remove(null));
    assertFalse(support.remove(mockListener));
    assertTrue(support.add(mockListener));
    assertTrue(support.remove(mockListener));
    assertFalse(support.remove(mockListener));
  }

  @Test
  public void testSize() {
    final ChangeListener mockListenerOne = mockContext.mock(ChangeListener.class, "sizeTestOne");
    final ChangeListener mockListenerTwo = mockContext.mock(ChangeListener.class, "sizeTestTwo");

    assertEquals(0, support.size());
    assertFalse(support.add(null));
    assertEquals(0, support.size());
    assertTrue(support.add(mockListenerOne));
    assertEquals(1, support.size());
    assertTrue(support.add(mockListenerTwo));
    assertTrue(support.add(mockListenerOne));
    assertEquals(3, support.size());
    assertFalse(support.remove(null));
    assertEquals(3, support.size());
    assertTrue(support.remove(mockListenerOne));
    assertTrue(support.remove(mockListenerTwo));
    assertEquals(1, support.size());
    assertTrue(support.remove(mockListenerOne));
    assertEquals(0, support.size());
    assertFalse(support.remove(mockListenerOne));
    assertFalse(support.remove(mockListenerTwo));
    assertEquals(0, support.size());
  }

  protected final class TestChangeListener implements ChangeListener {

    private boolean stateChangedCalled = false;

    private final Calendar now = Calendar.getInstance();

    public boolean isStateChangedCalled() {
      return stateChangedCalled;
    }

    public void stateChanged(final ChangeEvent event) {
      this.stateChangedCalled = true;
      assertEquals(support.getSource(), event.getSource());
      assertEquals(DateTimeUtils.truncate(now), DateTimeUtils.truncate(event.getChangeDateTime()));
    }
  }

}
