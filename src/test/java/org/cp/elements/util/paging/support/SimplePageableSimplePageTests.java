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

package org.cp.elements.util.paging.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.cp.elements.util.paging.support.SimplePageable.SimplePage;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Comparator;
import java.util.List;

import org.cp.elements.util.paging.Page;
import org.cp.elements.util.paging.PageNotFoundException;
import org.junit.Test;

/**
 * Unit tests for {@link SimplePageable.SimplePage}.
 *
 * @author John Blum
 * @see java.util.List
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.util.paging.Page
 * @see org.cp.elements.util.paging.support.SimplePageable
 * @see org.cp.elements.util.paging.support.SimplePageable.SimplePage
 * @since 1.0.0
 */
public class SimplePageableSimplePageTests {

  @Test
  @SuppressWarnings("all")
  public void constructSimplePageForPageOneInPageable() {

    List<Object> mockElements = mock(List.class);
    List<Object> mockList = mock(List.class);

    when(mockList.isEmpty()).thenReturn(false);
    when(mockList.size()).thenReturn(SimplePageable.DEFAULT_PAGE_SIZE * 2);
    when(mockList.subList(anyInt(), anyInt())).thenReturn(mockElements);

    SimplePageable<Object> pageable = spy(SimplePageable.of(mockList));

    SimplePage page = SimplePage.of(pageable);

    assertThat(page).isNotNull();
    assertThat(page.getElements()).isEqualTo(mockElements);
    assertThat(page.getNumber()).isEqualTo(1);
    assertThat(page.getPageable()).isSameAs(pageable);

    verify(mockList, times(1)).isEmpty();
    verify(mockList, times(2)).size();
    verify(mockList, times(1)).subList(eq(0), eq(20));
    verify(pageable, times(1)).isEmpty();
    verify(pageable, times(1)).count();
    verify(pageable, times(2)).getPageSize();
  }

  @Test
  @SuppressWarnings("all")
  public void constructSimplePageForPageTwoInPageable() {

    List<Object> mockElements = mock(List.class);
    List<Object> mockList = mock(List.class);

    when(mockList.isEmpty()).thenReturn(false);
    when(mockList.size()).thenReturn(SimplePageable.DEFAULT_PAGE_SIZE * 2);
    when(mockList.subList(anyInt(), anyInt())).thenReturn(mockElements);

    SimplePageable<Object> pageable = spy(SimplePageable.of(mockList));

    SimplePage page = SimplePage.of(pageable, 2);

    assertThat(page).isNotNull();
    assertThat(page.getElements()).isEqualTo(mockElements);
    assertThat(page.getNumber()).isEqualTo(2);
    assertThat(page.getPageable()).isSameAs(pageable);

    verify(mockList, times(1)).isEmpty();
    verify(mockList, times(2)).size();
    verify(mockList, times(1)).subList(eq(20), eq(40));
    verify(pageable, times(1)).isEmpty();
    verify(pageable, times(1)).count();
    verify(pageable, times(2)).getPageSize();
  }

  @Test(expected = IllegalArgumentException.class)
  public void constructSimplePageWithNullPageableThrowsException() {

    try {
      SimplePage.of(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Pageable is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void constructSimplePageWithEmptyPageableThrowsException() {

    try {
      SimplePage.of(SimplePageable.empty());
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Pageable object must contain pages");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void constructSimplePageWithNegativePageNumberThrowsException() {

    try {
      SimplePage.of(SimplePageable.of("test"), -1);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Page number [-1] must be greater than 0");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void constructSimplePageWithPageNumberGreaterThanTheNumberOfPagesThrowsException() {

    try {
      SimplePage.of(SimplePageable.of("test"), 2);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Page number [2] must be less than equal to the number of pages [1]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void hasNextFromPageOneWithTwoPagesReturnsTrue() {

    List<Object> mockList = mock(List.class);

    when(mockList.isEmpty()).thenReturn(false);
    when(mockList.size()).thenReturn(SimplePageable.DEFAULT_PAGE_SIZE + 1);

    SimplePageable pageable = SimplePageable.of(mockList);

    assertThat(pageable).isNotNull();
    assertThat(pageable.count()).isEqualTo(2);

    Page page = pageable.getPage(1);

    assertThat(page).isNotNull();
    assertThat(page.getNumber()).isEqualTo(1);
    assertThat(page.hasNext()).isTrue();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void hasNextFromPageTwoWithTwoPagesReturnsFalse() {

    List<Object> mockList = mock(List.class);

    when(mockList.isEmpty()).thenReturn(false);
    when(mockList.size()).thenReturn(SimplePageable.DEFAULT_PAGE_SIZE * 2);

    SimplePageable pageable = SimplePageable.of(mockList);

    assertThat(pageable).isNotNull();
    assertThat(pageable.count()).isEqualTo(2);

    Page page = pageable.getPage(2);

    assertThat(page).isNotNull();
    assertThat(page.getNumber()).isEqualTo(2);
    assertThat(page.hasNext()).isFalse();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void nextPageFromPageOneWithTwoPagesReturnsNextPage() {

    SimplePageable<Object> pageable = SimplePageable.<Object>of("test", "testing", "tested").with(2);

    assertThat(pageable).isNotNull();
    assertThat(pageable).isNotEmpty();
    assertThat(pageable.count()).isEqualTo(2);

    Page<Object> pageOne = pageable.getPage(1);

    assertThat(pageOne).isNotNull();
    assertThat(pageOne.getNumber()).isEqualTo(1);
    assertThat(pageOne.size()).isEqualTo(2);
    assertThat(pageOne).containsExactly("test", "testing");
    assertThat(pageOne.hasNext()).isTrue();

    Page<Object> pageTwo = pageOne.next();

    assertThat(pageTwo).isNotNull();
    assertThat(pageTwo.getNumber()).isEqualTo(2);
    assertThat(pageTwo.size()).isEqualTo(1);
    assertThat(pageTwo).containsExactly("tested");
    assertThat(pageTwo.hasNext()).isFalse();
  }

  @Test(expected = PageNotFoundException.class)
  @SuppressWarnings("unchecked")
  public void nextPageFromPageOneWithOnePageThrowsException() {

    SimplePageable<Object> pageable = SimplePageable.of("test", "testing", "tested");

    assertThat(pageable).isNotNull();
    assertThat(pageable).isNotEmpty();
    assertThat(pageable.count()).isEqualTo(1);

    Page<Object> pageOne = pageable.iterator().next();

    assertThat(pageOne).isNotNull();
    assertThat(pageOne.getNumber()).isEqualTo(1);
    assertThat(pageOne.size()).isEqualTo(3);
    assertThat(pageOne).containsExactly("test", "testing", "tested");
    assertThat(pageOne.hasNext()).isFalse();

    try {
      pageOne.next();
    }
    catch (PageNotFoundException expected) {

      assertThat(expected).hasMessage("No next page after [1]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void hasPreviousFromPageTwoReturnsTrue() {

    List<Object> mockList = mock(List.class);

    when(mockList.isEmpty()).thenReturn(false);
    when(mockList.size()).thenReturn(SimplePageable.DEFAULT_PAGE_SIZE * 2);

    SimplePageable pageable = SimplePageable.of(mockList);

    assertThat(pageable).isNotNull();
    assertThat(pageable.count()).isEqualTo(2);

    Page page = pageable.getPage(2);

    assertThat(page).isNotNull();
    assertThat(page.getNumber()).isEqualTo(2);
    assertThat(page.hasPrevious()).isTrue();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void hasPreviousFromPageOneReturnsFalse() {

    List<Object> mockList = mock(List.class);

    when(mockList.isEmpty()).thenReturn(false);
    when(mockList.size()).thenReturn(SimplePageable.DEFAULT_PAGE_SIZE * 2);

    SimplePageable pageable = SimplePageable.of(mockList);

    assertThat(pageable).isNotNull();
    assertThat(pageable.count()).isEqualTo(2);

    Page page = pageable.getPage(1);

    assertThat(page).isNotNull();
    assertThat(page.getNumber()).isEqualTo(1);
    assertThat(page.hasPrevious()).isFalse();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void previousPageFromPageTwoReturnsPageOne() {

    SimplePageable<Object> pageable = SimplePageable.<Object>of("test", "testing", "tested").with(2);

    assertThat(pageable).isNotNull();
    assertThat(pageable).isNotEmpty();
    assertThat(pageable.count()).isEqualTo(2);

    Page<Object> pageTwo = pageable.getPage(2);

    assertThat(pageTwo).isNotNull();
    assertThat(pageTwo.getNumber()).isEqualTo(2);
    assertThat(pageTwo.size()).isEqualTo(1);
    assertThat(pageTwo).containsExactly("tested");
    assertThat(pageTwo.hasPrevious()).isTrue();

    Page<Object> pageOne = pageTwo.previous();

    assertThat(pageOne).isNotNull();
    assertThat(pageOne.getNumber()).isEqualTo(1);
    assertThat(pageOne.size()).isEqualTo(2);
    assertThat(pageOne).containsExactly("test", "testing");
    assertThat(pageOne.hasNext()).isTrue();
  }

  @Test(expected = PageNotFoundException.class)
  @SuppressWarnings("unchecked")
  public void previousPageFromPageOneThrowsException() {

    SimplePageable<Object> pageable = SimplePageable.of("test", "testing", "tested");

    assertThat(pageable).isNotNull();
    assertThat(pageable).isNotEmpty();
    assertThat(pageable.count()).isEqualTo(1);

    Page<Object> pageOne = pageable.iterator().next();

    assertThat(pageOne).isNotNull();
    assertThat(pageOne.getNumber()).isEqualTo(1);
    assertThat(pageOne.size()).isEqualTo(3);
    assertThat(pageOne).containsExactly("test", "testing", "tested");
    assertThat(pageOne.hasPrevious()).isFalse();

    try {
      pageOne.previous();
    }
    catch (PageNotFoundException expected) {

      assertThat(expected).hasMessage("No previous page before [1]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void sortPageIsCorrect() {

    SimplePageable<Integer> pageable = SimplePageable.of(5, 8, 2, 9, 1, 4, 6, 7, 3).with(3);

    assertThat(pageable).isNotNull();
    assertThat(pageable).isNotEmpty();
    assertThat(pageable.getPageSize()).isEqualTo(3);
    assertThat(pageable.count()).isEqualTo(3);
    assertThat(pageable.getList()).containsExactly(5, 8, 2, 9, 1, 4, 6, 7, 3);

    Page<Integer> pageTwo = pageable.getPage(2);

    assertThat(pageTwo).isNotNull();
    assertThat(pageTwo).isNotEmpty();
    assertThat(pageTwo.getNumber()).isEqualTo(2);
    assertThat(pageTwo.hasNext()).isTrue();
    assertThat(pageTwo.hasPrevious()).isTrue();
    assertThat(pageTwo.size()).isEqualTo(3);
    assertThat(pageTwo).containsExactly(9, 1, 4);

    pageTwo.sort(Comparator.naturalOrder());

    assertThat(pageTwo.size()).isEqualTo(3);
    assertThat(pageTwo).containsExactly(1, 4, 9);
    assertThat(pageable.getList()).containsExactly(5, 8, 2, 1, 4, 9, 6, 7, 3);
  }
}
