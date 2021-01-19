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
package org.cp.elements.util.sort.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.cp.elements.util.ArrayUtils.asIterable;
import static org.junit.Assert.fail;

import java.util.Collections;
import java.util.List;
import java.util.Random;

import org.cp.elements.lang.Constants;
import org.cp.elements.util.sort.AbstractSorter;
import org.cp.elements.util.sort.Sorter;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * The {@link CommonSortTestSuite} class is an abstract base class encapsulating test functionality and logic
 * common to all {@link Sorter} based test classes.
 *
 * In addition, this abstract test suite class setups a sortable data structure and basic test case
 * to test sort order functionality of the {@link Sorter}.
 *
 * @author John J. Blum
 * @see java.util.Random
 * @see org.junit.Test
 * @see org.cp.elements.util.sort.AbstractSorter
 * @see org.cp.elements.util.sort.Sorter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class CommonSortTestSuite {

  protected static final int DEFAULT_ELEMENT_COUNT_TO_SORT = 10000;
  protected static final int DEFAULT_MAXIMUM_NUMBER_VALUE = 1000000;

  protected Integer[] numbers;

  protected void assertShuffled(Iterable<Integer> numbers) {
    Integer previousNumber = Integer.MAX_VALUE;

    for (Integer currentNumber : numbers) {
      if (currentNumber > previousNumber) {
        return;
      }

      previousNumber = currentNumber;
    }

    fail("The array of numbers is not shuffled!");
  }

  protected void assertSorted(Iterable<Integer> numbers) {

    Integer previousNumber = -1;

    for (Integer currentNumber : numbers) {

      assertThat(previousNumber).as("%1$d is not less than equal to %2$d", previousNumber, currentNumber)
        .isLessThanOrEqualTo(currentNumber);

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

      assertThat(numbers).isNotNull();
      assertThat(numbers.length).isEqualTo(getNumberOfElementsToSort());

      for (int index = 0; index < numbers.length; index++) {
        numbers[index] = numberGenerator.nextInt(getMaximumNumberValue());
      }

      TestSorter.TestSortableArrayList<Integer> numberList = new TestSorter.TestSortableArrayList<>(numbers);

      assertThat(numberList).isNotNull();
      assertThat(numberList.size()).isEqualTo(numbers.length);

      Collections.shuffle(numberList, numberGenerator);

      assertShuffled(asIterable(numbers));
    }
  }

  @After
  public void tearDown() {
    numbers = null;
  }

  @Test
  public void sort() {
    Sorter sorter = getSorter();

    assertThat(sorter).as("The Sorter implementation was not configured and initialized properly!").isNotNull();

    Integer[] sortedNumbers = sorter.sort(numbers);

    assertThat(sortedNumbers).isNotNull();
    assertThat(sortedNumbers.length).isEqualTo(getNumberOfElementsToSort());
    assertSorted(asIterable(sortedNumbers));
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
