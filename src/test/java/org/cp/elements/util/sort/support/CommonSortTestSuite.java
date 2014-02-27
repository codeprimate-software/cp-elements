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

package org.cp.elements.util.sort.support;

import static org.junit.Assert.*;

import java.util.Collections;
import java.util.List;
import java.util.Random;

import org.cp.elements.lang.StringUtils;
import org.cp.elements.test.AbstractMockingTestSuite;
import org.cp.elements.util.ArrayUtils;
import org.cp.elements.util.sort.AbstractSorter;
import org.cp.elements.util.sort.Sorter;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * The CommonSortTestSuite class is an abstract base class encapsulating test functionality and logic common to all
 * Sorter based test classes.  In addition, this abstract test suite class setups a sortable data structure and basic
 * test case to test sort order functionality of the Sorter.
 * <p/>
 * @author John J. Blum
 * @see java.util.Random
 * @see org.cp.elements.test.AbstractMockingTestSuite
 * @see org.cp.elements.util.sort.AbstractSorter
 * @see org.cp.elements.util.sort.Sorter
 * @see org.junit.Test
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class CommonSortTestSuite extends AbstractMockingTestSuite {

  protected static final int DEFAULT_ELEMENT_COUNT_TO_SORT = 10000;
  protected static final int DEFAULT_MAXIMUM_NUMBER_VALUE = 1000000;

  protected Integer[] numbers;

  protected void assertShuffled(final Iterable<Integer> numbers) {
    Integer previousNumber = Integer.MAX_VALUE;

    for (Integer currentNumber : numbers) {
      if (currentNumber > previousNumber) {
        return;
      }

      previousNumber = currentNumber;
    }

    fail("The array of numbers is not shuffled!");
  }

  protected void assertSorted(final Iterable<Integer> numbers) {
    Integer previousNumber = -1;

    for (Integer currentNumber : numbers) {
      assertTrue(String.format("%1$d is not less than equal to %2$d!", previousNumber, currentNumber),
        previousNumber <= currentNumber);
      previousNumber = currentNumber;
    }
  }

  protected int getMaximumNumberValue() {
    return DEFAULT_MAXIMUM_NUMBER_VALUE;
  }

  protected int getNumberOfElementsToSort() {
    return DEFAULT_ELEMENT_COUNT_TO_SORT;
  }

  protected abstract Sorter getSorter();

  protected boolean runSetup() {
    return true;
  }

  @Before
  public void setup() {
    super.setup();

    if (runSetup()) {
      Random numberGenerator = new Random(System.currentTimeMillis());

      numbers = new Integer[getNumberOfElementsToSort()];

      assertNotNull(numbers);
      assertEquals(getNumberOfElementsToSort(), numbers.length);

      for (int index = 0; index < numbers.length; index++) {
        numbers[index] = numberGenerator.nextInt(getMaximumNumberValue());
      }

      TestSorter.TestSortableArrayList<Integer> numberList = new TestSorter.TestSortableArrayList<Integer>(numbers);

      assertNotNull(numberList);
      assertEquals(numbers.length, numberList.size());

      Collections.shuffle(numberList, numberGenerator);

      assertShuffled(ArrayUtils.iterable(numbers));
    }
  }

  @After
  public void tearDown() {
    super.tearDown();
    numbers = null;
  }

  @Test
  public void testSort() {
    Sorter sorter = getSorter();

    assertNotNull("The Sorter implementation was not configured and initialized properly!", sorter);

    Integer[] sortedNumbers = sorter.sort(numbers);

    assertNotNull(sortedNumbers);
    assertEquals(getNumberOfElementsToSort(), sortedNumbers.length);
    assertSorted(ArrayUtils.iterable(sortedNumbers));
  }

  protected static class TestSorter extends AbstractSorter {

    @Override
    public <E> List<E> sort(final List<E> elements) {
      throw new UnsupportedOperationException(StringUtils.NOT_IMPLEMENTED);
    }

    protected static class TestSortableArrayList<E> extends SortableArrayList<E> {

      public TestSortableArrayList(final E... elements) {
        super(elements);
      }
    }
  }

}
