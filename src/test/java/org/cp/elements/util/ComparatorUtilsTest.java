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

package org.cp.elements.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.cp.elements.test.TestUtils;
import org.junit.Test;

/**
 * The ComparatorUtilsTest class is a test suite class of test cases testing the contract and functionality
 * of the ComparatorUtilsTest class.
 *
 * @author John J. Blum
 * @see org.cp.elements.test.TestUtils
 * @see org.cp.elements.util.ComparatorUtils
 * @see org.junit.Assert
 * @see org.junit.Test
 * @since 1.0.0
 */
public class ComparatorUtilsTest {

  private static final String NULL = null;

  @Test
  public void testCompareIgnoreNull() throws Exception {
    TestUtils.assertPositive(ComparatorUtils.compareIgnoreNull(NULL, NULL));
    TestUtils.assertPositive(ComparatorUtils.compareIgnoreNull(null, "null"));
    TestUtils.assertNegative(ComparatorUtils.compareIgnoreNull("null", null));
    assertEquals(0, ComparatorUtils.compareIgnoreNull("test", "test"));
    TestUtils.assertNegative(ComparatorUtils.compareIgnoreNull("test", "testing"));
    TestUtils.assertPositive(ComparatorUtils.compareIgnoreNull("tested", "test"));
    TestUtils.assertPositive(ComparatorUtils.compareIgnoreNull("test", "TEST"));
  }

  @Test
  public void testInvert() {
    List<Number> numbers = new ArrayList<>(Arrays.asList(2, 1, 3));

    Comparator<Number> numbersComparator = (numberOne, numberTwo) -> (numberOne.intValue() - numberTwo.intValue());

    Collections.sort(numbers,  numbersComparator);

    int currentNumber = Integer.MIN_VALUE;

    for (final Number number : numbers) {
      assertTrue(currentNumber < number.intValue());
      currentNumber = number.intValue();
    }

    Collections.sort(numbers, ComparatorUtils.invert(numbersComparator));

    currentNumber = Integer.MAX_VALUE;

    for (final Number number : numbers) {
      assertTrue(currentNumber > number.intValue());
      currentNumber = number.intValue();
    }
  }

}
