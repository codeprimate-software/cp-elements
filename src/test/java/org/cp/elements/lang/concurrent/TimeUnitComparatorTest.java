/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 *
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 *
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 *
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 *
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.lang.concurrent;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.lessThan;
import static org.junit.Assert.assertThat;

import java.util.concurrent.TimeUnit;

import org.junit.Test;

/**
 * The TimeUnitComparatorTest class is a test suite of test cases testing the contract and functionality
 * of the TimeUnitComparator class.
 *
 * @author John J. Blum
 * @see java.util.concurrent.TimeUnit
 * @see org.junit.Test
 * @see org.cp.elements.lang.concurrent.TimeUnitComparator
 * @since 1.0.0
 */
public class TimeUnitComparatorTest {

  @Test
  public void compareTimeUnitsAllEqualTo() {
    assertThat(TimeUnitComparator.INSTANCE.compare(null, null), is(equalTo(0)));
    assertThat(TimeUnitComparator.INSTANCE.compare(TimeUnit.DAYS, TimeUnit.DAYS), is(equalTo(0)));
    assertThat(TimeUnitComparator.INSTANCE.compare(TimeUnit.HOURS, TimeUnit.HOURS), is(equalTo(0)));
    assertThat(TimeUnitComparator.INSTANCE.compare(TimeUnit.MINUTES, TimeUnit.MINUTES), is(equalTo(0)));
    assertThat(TimeUnitComparator.INSTANCE.compare(TimeUnit.SECONDS, TimeUnit.SECONDS), is(equalTo(0)));
    assertThat(TimeUnitComparator.INSTANCE.compare(TimeUnit.MILLISECONDS, TimeUnit.MILLISECONDS), is(equalTo(0)));
    assertThat(TimeUnitComparator.INSTANCE.compare(TimeUnit.MICROSECONDS, TimeUnit.MICROSECONDS), is(equalTo(0)));
    assertThat(TimeUnitComparator.INSTANCE.compare(TimeUnit.NANOSECONDS, TimeUnit.NANOSECONDS), is(equalTo(0)));
  }

  @Test
  public void compareTimeUnitsAllGreaterThan() {
    assertThat(TimeUnitComparator.INSTANCE.compare(null, TimeUnit.DAYS), is(greaterThan(0)));
    assertThat(TimeUnitComparator.INSTANCE.compare(TimeUnit.DAYS, TimeUnit.HOURS), is(greaterThan(0)));
    assertThat(TimeUnitComparator.INSTANCE.compare(TimeUnit.HOURS, TimeUnit.MINUTES), is(greaterThan(0)));
    assertThat(TimeUnitComparator.INSTANCE.compare(TimeUnit.MINUTES, TimeUnit.SECONDS), is(greaterThan(0)));
    assertThat(TimeUnitComparator.INSTANCE.compare(TimeUnit.SECONDS, TimeUnit.MILLISECONDS), is(greaterThan(0)));
    assertThat(TimeUnitComparator.INSTANCE.compare(TimeUnit.MILLISECONDS, TimeUnit.MICROSECONDS), is(greaterThan(0)));
    assertThat(TimeUnitComparator.INSTANCE.compare(TimeUnit.MICROSECONDS, TimeUnit.NANOSECONDS), is(greaterThan(0)));
  }

  @Test
  public void compareTimeUnitsAllLessThan() {
    assertThat(TimeUnitComparator.INSTANCE.compare(TimeUnit.NANOSECONDS, TimeUnit.MICROSECONDS), is(lessThan(0)));
    assertThat(TimeUnitComparator.INSTANCE.compare(TimeUnit.MICROSECONDS, TimeUnit.MILLISECONDS), is(lessThan(0)));
    assertThat(TimeUnitComparator.INSTANCE.compare(TimeUnit.MILLISECONDS, TimeUnit.SECONDS), is(lessThan(0)));
    assertThat(TimeUnitComparator.INSTANCE.compare(TimeUnit.SECONDS, TimeUnit.MINUTES), is(lessThan(0)));
    assertThat(TimeUnitComparator.INSTANCE.compare(TimeUnit.MINUTES, TimeUnit.HOURS), is(lessThan(0)));
    assertThat(TimeUnitComparator.INSTANCE.compare(TimeUnit.HOURS, TimeUnit.DAYS), is(lessThan(0)));
    assertThat(TimeUnitComparator.INSTANCE.compare(TimeUnit.DAYS, null), is(lessThan(0)));
  }

}
