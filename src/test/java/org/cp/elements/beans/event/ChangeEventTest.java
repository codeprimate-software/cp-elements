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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;

import java.util.Calendar;

import org.cp.elements.time.DateTimeUtils;
import org.junit.Test;

/**
 * The ChangeEventTest class is a test suite of test cases testing the contract and functionality
 * of the ChangeEvent class.
 *
 * @author John J. Blum
 * @see org.cp.elements.beans.event.ChangeEvent
 * @see org.junit.Assert
 * @see org.junit.Test
 */
public class ChangeEventTest {

  @Test
  public void testCreateChangeEvent() {
    final Object expectedSource = new Object();
    final ChangeEvent event = new ChangeEvent(expectedSource);

    assertNotNull(event);
    assertSame(expectedSource, event.getSource());
    assertEquals(DateTimeUtils.truncate(Calendar.getInstance()), DateTimeUtils.truncate(event.getChangeDateTime()));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testCreateChangeEventWithNullSource() {
    new ChangeEvent(null);
  }

  @Test
  public void testChangeDateTimeImmutable() {
    final ChangeEvent event = new ChangeEvent(new Object());

    assertNotNull(event);

    final Calendar now = Calendar.getInstance();
    final Calendar changeDateTime = event.getChangeDateTime();

    assertNotNull(changeDateTime);
    assertEquals(DateTimeUtils.truncate(now), DateTimeUtils.truncate(changeDateTime));

    changeDateTime.add(Calendar.YEAR, -5);

    assertEquals(now.get(Calendar.YEAR) - 5, changeDateTime.get(Calendar.YEAR));
    assertFalse(changeDateTime.equals(event.getChangeDateTime()));
  }

}
