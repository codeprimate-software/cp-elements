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

import java.util.Calendar;

import org.cp.elements.lang.DateTimeUtils;
import org.junit.Test;

/**
 * The ChangeEventTest class is a test suite of test cases testing the contract and functionality 
 * of the ChangeEvent class.
 * <p/>
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
