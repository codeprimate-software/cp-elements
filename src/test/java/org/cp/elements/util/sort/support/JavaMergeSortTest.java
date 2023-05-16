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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.cp.elements.util.sort.Sorter;
import org.junit.jupiter.api.Test;

/**
 * The JavaMergeSortTest class is a test suite of test cases testing the contract and functionality
 * of the JavaMergeSort class and Java's modified Merge Sort algorithm.
 *
 * @author John J. Blum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.util.sort.Sorter
 * @see org.cp.elements.util.sort.support.CommonSortTestSuite
 * @see org.cp.elements.util.sort.support.JavaMergeSort
 * @since 1.0.0
 */
public class JavaMergeSortTest extends CommonSortTestSuite {

  @Override
  protected Sorter getSorter() {
    return new JavaMergeSort();
  }

  @Test
  public void testSortCollection() {
    Sorter sorter = getSorter();

    assertNotNull("The Sorter implementation was not configured and initialized properly!", sorter);

    List<Integer> sortedNumbers = sorter.sort(new ArrayList<>(Arrays.asList(numbers)));

    assertNotNull(sortedNumbers);
    assertEquals(getNumberOfElementsToSort(), sortedNumbers.size());
    assertSorted(sortedNumbers);
  }

}
