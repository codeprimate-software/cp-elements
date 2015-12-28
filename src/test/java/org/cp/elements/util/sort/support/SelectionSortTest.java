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

import org.cp.elements.util.sort.Sorter;

/**
 * The SelectionSortTest class is a test suite of test cases testing the contract and functionality of the SelectionSort
 * class and the Selection Sort algorithm.
 *
 * @author John J. Blum
 * @see org.cp.elements.util.sort.Sorter
 * @see org.cp.elements.util.sort.support.CommonSortTestSuite
 * @see org.cp.elements.util.sort.support.SelectionSort
 * @since 1.0.0
 */
public class SelectionSortTest extends CommonSortTestSuite {

  @Override
  protected Sorter getSorter() {
    return new SelectionSort();
  }

}
