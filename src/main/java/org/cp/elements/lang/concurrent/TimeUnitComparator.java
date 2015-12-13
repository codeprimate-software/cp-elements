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

import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;

/**
 * The TimeUnitComparator class is a Comparator implementation that compares 2 java.util.concurrent.TimeUnit objects
 * for order.
 *
 * @author John J. Blum
 * @see java.util.Comparator
 * @see java.util.concurrent.TimeUnit
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class TimeUnitComparator implements Comparator<TimeUnit> {

  public static final Comparator<TimeUnit> INSTANCE = new TimeUnitComparator();

  private static final Map<TimeUnit, Integer> TIME_UNIT_VALUE = new HashMap<>(TimeUnit.values().length + 1);

  static {
    int value = 0;
    TIME_UNIT_VALUE.put(TimeUnit.NANOSECONDS, ++value);
    TIME_UNIT_VALUE.put(TimeUnit.MICROSECONDS, ++value);
    TIME_UNIT_VALUE.put(TimeUnit.MILLISECONDS, ++value);
    TIME_UNIT_VALUE.put(TimeUnit.SECONDS, ++value);
    TIME_UNIT_VALUE.put(TimeUnit.MINUTES, ++value);
    TIME_UNIT_VALUE.put(TimeUnit.HOURS, ++value);
    TIME_UNIT_VALUE.put(TimeUnit.DAYS, ++value);
    TIME_UNIT_VALUE.put(null, Integer.MAX_VALUE);
  }

  /**
   * Compares 2 TimeUnit values for order.
   *
   * @param timeUnitOne the first TimeUnit operand in the relational comparison.
   * @param timeUnitTwo the second TimeUnit operand in the relational comparison
   * @return an integer value indicating the relative order of TimeUnit 1 to TimeUnit 2 returning a negative value
   * if TimeUnit 1 is less than TimeUnit 2, a positive value if Time Unit 1 is greater than TimeUnit 2 or 0
   * if the 2 TimUnit objects are equal.
   * @see java.util.Comparator#compare(Object, Object)
   * @see java.util.concurrent.TimeUnit
   */
  @Override
  public int compare(final TimeUnit timeUnitOne, final TimeUnit timeUnitTwo) {
    return Integer.valueOf(String.valueOf(TIME_UNIT_VALUE.get(timeUnitOne))).compareTo(
      TIME_UNIT_VALUE.get(timeUnitTwo));
  }

}
