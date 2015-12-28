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

package org.cp.elements.util.sort.support;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.Collections;
import java.util.List;
import java.util.Random;

import org.cp.elements.lang.Constants;
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
 *
 * @author John J. Blum
 * @see java.util.Random
 * @see org.cp.elements.test.AbstractMockingTestSuite
 * @see org.cp.elements.util.sort.AbstractSorter
 * @see org.cp.elements.util.sort.Sorter
 * @see org.junit.Test
 * @since 1.0.0
 */
@SuppressWarnings({ "deprecation", "unused" })
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
    if (runSetup()) {
      Random numberGenerator = new Random(System.currentTimeMillis());

      numbers = new Integer[getNumberOfElementsToSort()];

      assertNotNull(numbers);
      assertEquals(getNumberOfElementsToSort(), numbers.length);

      for (int index = 0; index < numbers.length; index++) {
        numbers[index] = numberGenerator.nextInt(getMaximumNumberValue());
      }

      TestSorter.TestSortableArrayList<Integer> numberList = new TestSorter.TestSortableArrayList<>(numbers);

      assertNotNull(numberList);
      assertEquals(numbers.length, numberList.size());

      Collections.shuffle(numberList, numberGenerator);

      assertShuffled(ArrayUtils.iterable(numbers));
    }
  }

  @After
  public void tearDown() {
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
      throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
    }

    protected static class TestSortableArrayList<E> extends SortableArrayList<E> {

      @SafeVarargs
      public TestSortableArrayList(final E... elements) {
        super(elements);
      }
    }
  }

}
