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

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.List;

import org.junit.Test;

/**
 * Test suite of test cases testing the contract and functionality of the {@link ShellSort} class
 * and the Shell Sort algorithm.
 *
 * @author John J. Blum
 * @see org.cp.elements.util.sort.Sorter
 * @see org.cp.elements.util.sort.support.CommonSortTestSuite
 * @see org.cp.elements.util.sort.support.ShellSort
 * @since 1.0.0
 */
public class ShellSortTests extends CommonSortTestSuite {

  @Override
  protected ShellSort getSorter() {
    return new ShellSort();
  }

  @Test
  public void getGap() {
    List mockList = mock(List.class, "Sortable List");

    when(mockList.size()).thenReturn(300);

    ShellSort sorter = getSorter();

    assertThat(sorter.getGap(mockList), is(equalTo(100)));

    verify(mockList, times(1)).size();
  }
}
