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
