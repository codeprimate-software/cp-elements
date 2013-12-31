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

package org.cp.elements.util;

import static org.junit.Assert.*;

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
 * <p/>
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
    final List<Number> numbers = new ArrayList<Number>(Arrays.asList(2, 1, 3));

    final Comparator<Number> numbersComparator = new Comparator<Number>() {
      @Override public int compare(final Number numberOne, final Number numberTwo) {
        return (numberOne.intValue() - numberTwo.intValue());
      }
    };

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
