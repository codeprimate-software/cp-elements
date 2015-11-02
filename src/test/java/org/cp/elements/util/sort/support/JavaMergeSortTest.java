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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.cp.elements.util.sort.Sorter;
import org.junit.Test;

/**
 * The JavaMergeSortTest class is a test suite of test cases testing the contract and functionality
 * of the JavaMergeSort class and Java's modified Merge Sort algorithm.
 * <p/>
 * @author John J. Blum
 * @see org.junit.Test
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
