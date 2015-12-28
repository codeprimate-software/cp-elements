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

import java.util.List;

import org.jmock.Expectations;
import org.junit.Test;

/**
 * The ShellSortTest class is a test suite of test cases testing the contract and functionality of the ShellSort
 * class and the Shell Sort algorithm.
 *
 * @author John J. Blum
 * @see org.cp.elements.util.sort.Sorter
 * @see org.cp.elements.util.sort.support.CommonSortTestSuite
 * @see org.cp.elements.util.sort.support.ShellSort
 * @since 1.0.0
 */
public class ShellSortTest extends CommonSortTestSuite {

  @Override
  protected ShellSort getSorter() {
    return new ShellSort();
  }

  @Test
  public void testGetGap() {
    final List mockList = mockContext.mock(List.class, "Sortable List");

    mockContext.checking(new Expectations() {{
      oneOf(mockList).size();
      will(returnValue(300));
    }});

    ShellSort sorter = getSorter();

    assertEquals(100, sorter.getGap(mockList));
  }

}
