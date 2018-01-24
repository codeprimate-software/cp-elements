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
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

import org.cp.elements.util.paging.Page;
import org.cp.elements.util.paging.PageNotFoundException;
import org.junit.Test;

/**
 * Unit tests for {@link SimplePageable}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.util.paging.support.SimplePageable
 * @since 1.0.0
 */
public class SimplePageableTests {

  @Test
  public void constructSimplePageableWithListAndDefaultPageSize() {

    List<Object> list = Arrays.asList("test", "testing", "tested");

    SimplePageable<Object> pageable = new SimplePageable<>(list);

    assertThat(pageable).isNotNull();
    assertThat(pageable.getList()).isEqualTo(list);
    assertThat(pageable.getPageSize()).isEqualTo(SimplePageable.DEFAULT_PAGE_SIZE);
  }

  @Test
  public void constructSimplePageableWithListAndPageSize() {

    List<Object> list = Arrays.asList("test", "testing", "tested");

    SimplePageable<Object> pageable = new SimplePageable<>(list, 5);

    assertThat(pageable).isNotNull();
    assertThat(pageable.getList()).isEqualTo(list);
    assertThat(pageable.getPageSize()).isEqualTo(5);
  }

  @Test(expected = IllegalArgumentException.class)
  public void constructSimplePageableWithInvalidPageSize() {

    try {
      new SimplePageable<>(Collections.emptyList(), -1);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Page size [-1] must be greater than 0");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void constructSimplePageableWithNullList() {

    try {
      new SimplePageable<>(null, 5);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("List is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void emptySimplePageable() {

    SimplePageable<Object> pageable = SimplePageable.empty();

    assertThat(pageable).isNotNull();
    assertThat(pageable).isEmpty();
    assertThat(pageable.getList()).isEmpty();
    assertThat(pageable.getPageSize()).isEqualTo(SimplePageable.DEFAULT_PAGE_SIZE);
  }

  @Test
  public void simplePageableOfNullArray() {

    SimplePageable<Object> pageable = SimplePageable.of((Object[]) null);

    assertThat(pageable).isNotNull();
    assertThat(pageable.getList()).isEmpty();
    assertThat(pageable.getPageSize()).isEqualTo(SimplePageable.DEFAULT_PAGE_SIZE);
  }

  @Test
  public void simplePageableOfNullList() {

    SimplePageable<Object> pageable = SimplePageable.of(null, 10);

    assertThat(pageable).isNotNull();
    assertThat(pageable.getList()).isEmpty();
    assertThat(pageable.getPageSize()).isEqualTo(10);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void pageCountForListSizeOfZeroIsZero() {

    SimplePageable pageable = SimplePageable.empty();

    assertThat(pageable).isNotNull();
    assertThat(pageable.count()).isEqualTo(0);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void pageCountForListSizeOfOneUsingDefaultPageSizeIsOne() {

    SimplePageable pageable = SimplePageable.of(Collections.singletonList("test"));

    assertThat(pageable).isNotNull();
    assertThat(pageable.count()).isEqualTo(1);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void pageCountForListSizeOfTwoUsingPageSizeOfTwoIsOne() {

    SimplePageable pageable = SimplePageable.of(Arrays.asList("test", "testing"), 2);

    assertThat(pageable).isNotNull();
    assertThat(pageable.count()).isEqualTo(1);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void pageCountForListSizeOfTwoUsingPageSizeOfOneIsTwo() {

    SimplePageable pageable = SimplePageable.of(Arrays.asList("test", "testing"), 1);

    assertThat(pageable).isNotNull();
    assertThat(pageable.count()).isEqualTo(2);
  }

  @Test
  public void isEmptyOnSimplePageableCallsListIsEmpty() {

    List<?> mockList = mock(List.class);

    when(mockList.isEmpty()).thenReturn(true);

    SimplePageable<?> pageable = SimplePageable.of(mockList);

    assertThat(pageable.isEmpty()).isTrue();

    verify(mockList, times(1)).isEmpty();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void firstPageOfSinglePageSimplePageableReturnsFirstPage() {

    Page<Object> mockPage = mock(Page.class);

    SimplePageable<Object> pageable = spy(SimplePageable.empty());

    doAnswer(invocation -> Collections.singletonList(mockPage).iterator()).when(pageable).iterator();

    assertThat(pageable).isNotNull();
    assertThat(pageable.firstPage()).isEqualTo(mockPage);
    assertThat(pageable.firstPage()).isEqualTo(mockPage);

    verify(pageable, times(2)).getPage(eq(1));
    verify(pageable, times(2)).iterator();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void firstPageOfMultiPageSimplePageableReturnsFirstPage() {

    Page<Object> mockPageOne = mock(Page.class);
    Page<Object> mockPageTwo = mock(Page.class);

    SimplePageable<Object> pageable = spy(SimplePageable.empty());

    doAnswer(invocation -> Arrays.asList(mockPageOne, mockPageTwo).iterator()).when(pageable).iterator();

    assertThat(pageable).isNotNull();
    assertThat(pageable.firstPage()).isEqualTo(mockPageOne);
    assertThat(pageable.firstPage()).isEqualTo(mockPageOne);

    verify(pageable, times(2)).getPage(eq(1));
    verify(pageable, times(2)).iterator();
  }

  @Test(expected = PageNotFoundException.class)
  public void firstPageOfEmptySimplePageableThrowsException() {

    try {
      SimplePageable.empty().firstPage();
    }
    catch (PageNotFoundException expected) {

      assertThat(expected).hasMessage("No first page");
      assertThat(expected).hasCauseInstanceOf(PageNotFoundException.class);
      assertThat(expected.getCause()).hasMessage("Page with number [1] not found");
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = NoSuchElementException.class)
  public void iteratorNextOnSimplePageableWithNoPagesThrowsException() {

    try {

      SimplePageable<Object> pageable = SimplePageable.of();

      assertThat(pageable).isNotNull();
      assertThat(pageable).isEmpty();

      Iterator<Page<Object>> pages = pageable.iterator();

      assertThat(pages).isNotNull();
      assertThat(pages.hasNext()).isFalse();

      pages.next();
    }
    catch (NoSuchElementException expected) {

      assertThat(expected).hasMessage("No more pages");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void lastPageWithSinglePageSimplePageableReturnsLastPage() {

    Page<Object> mockPage = mock(Page.class);

    List<Page<Object>> pages = Collections.singletonList(mockPage);

    SimplePageable<Object> pageable = spy(SimplePageable.empty());

    doAnswer(invocation -> pages.iterator()).when(pageable).iterator();
    doReturn(pages.size()).when(pageable).count();

    assertThat(pageable).isNotNull();
    assertThat(pageable.lastPage()).isEqualTo(mockPage);
    assertThat(pageable.lastPage()).isEqualTo(mockPage);

    verify(pageable, times(2)).getPage(eq(1));
    verify(pageable, times(2)).iterator();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void lastPageWithMultiPageSimplePageableReturnsLastPage() {

    Page<Object> mockPageOne = mock(Page.class);
    Page<Object> mockPageTwo = mock(Page.class);

    List<Page<Object>> pages = Arrays.asList(mockPageOne, mockPageTwo);

    SimplePageable<Object> pageable = spy(SimplePageable.empty());

    doAnswer(invocation -> pages.iterator()).when(pageable).iterator();
    doReturn(pages.size()).when(pageable).count();

    assertThat(pageable).isNotNull();
    assertThat(pageable.lastPage()).isEqualTo(mockPageTwo);
    assertThat(pageable.lastPage()).isEqualTo(mockPageTwo);

    verify(pageable, times(2)).getPage(eq(2));
    verify(pageable, times(2)).iterator();
  }

  @Test(expected = PageNotFoundException.class)
  @SuppressWarnings("unchecked")
  public void lastPageWithEmptySimplePageableReturnsLastPage() {

    try {
      SimplePageable.empty().lastPage();
    }
    catch (PageNotFoundException expected) {

      assertThat(expected).hasMessage("No last page");
      assertThat(expected).hasCauseInstanceOf(IllegalArgumentException.class);
      assertThat(expected.getCause()).hasMessage("Page number [0] must be greater than 0");
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void simplePageableWithNoPages() {

    SimplePageable<Object> pageable = SimplePageable.of();

    assertThat(pageable).isNotNull();
    assertThat(pageable).isEmpty();
    assertThat(pageable.count()).isEqualTo(0);

    Iterator<Page<Object>> pages = pageable.iterator();

    assertThat(pages).isNotNull();
    assertThat(pages.hasNext()).isFalse();
  }

  @Test
  public void simplePageableWithSinglePage() {

    List<String> strings = Arrays.asList("test", "testing", "tested");

    SimplePageable<String> pageableStrings = SimplePageable.of(strings);

    assertThat(pageableStrings).isNotNull();
    assertThat(pageableStrings).isNotEmpty();
    assertThat(pageableStrings.count()).isEqualTo(1);
    assertThat(pageableStrings.getPageSize()).isEqualTo(SimplePageable.DEFAULT_PAGE_SIZE);

    Iterator<Page<String>> pages = pageableStrings.iterator();

    assertThat(pages).isNotNull();
    assertThat(pages.hasNext()).isTrue();

    Page<String> page = pages.next();

    assertThat(page).isNotNull();
    assertThat(page).containsExactly("test", "testing", "tested");
    assertThat(pages.hasNext()).isFalse();
  }

  @Test
  public void simplePageableWithThreeFullPages() {

    List<Integer> integers = Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8, 9);

    SimplePageable<Integer> pageableIntegers = SimplePageable.of(integers, 3);

    assertThat(pageableIntegers).isNotNull();
    assertThat(pageableIntegers).isNotEmpty();
    assertThat(pageableIntegers.count()).isEqualTo(3);

    Iterator<Page<Integer>> pages = pageableIntegers.iterator();

    assertThat(pages).isNotNull();
    assertThat(pages.hasNext()).isTrue();

    Page<Integer> pageOne = pages.next();

    assertThat(pageOne).isNotNull();
    assertThat(pageOne).containsExactly(1, 2, 3);
    assertThat(pages.hasNext()).isTrue();

    Page<Integer> pageTwo = pages.next();

    assertThat(pageTwo).isNotNull();
    assertThat(pageTwo).containsExactly(4, 5, 6);
    assertThat(pages.hasNext()).isTrue();

    Page<Integer> pageThree = pages.next();

    assertThat(pageThree).isNotNull();
    assertThat(pageThree).containsExactly(7, 8, 9);
    assertThat(pages.hasNext()).isFalse();
  }

  @Test
  public void simplePageableWithTwoFullPagesAndOnePartialPage() {

    List<Integer> integers = Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8, 9);

    SimplePageable<Integer> pageableIntegers = SimplePageable.of(integers, 4);

    assertThat(pageableIntegers).isNotNull();
    assertThat(pageableIntegers).isNotEmpty();
    assertThat(pageableIntegers.count()).isEqualTo(3);

    Iterator<Page<Integer>> pages = pageableIntegers.iterator();

    assertThat(pages).isNotNull();
    assertThat(pages.hasNext()).isTrue();

    Page<Integer> pageOne = pages.next();

    assertThat(pageOne).isNotNull();
    assertThat(pageOne).containsExactly(1, 2, 3, 4);
    assertThat(pages.hasNext()).isTrue();

    Page<Integer> pageTwo = pages.next();

    assertThat(pageTwo).isNotNull();
    assertThat(pageTwo).containsExactly(5, 6, 7, 8);
    assertThat(pages.hasNext()).isTrue();

    Page<Integer> pageThree = pages.next();

    assertThat(pageThree).isNotNull();
    assertThat(pageThree).containsExactly(9);
    assertThat(pages.hasNext()).isFalse();
  }

  @Test
  public void sortPageableIsCorrect() {

    List<Integer> integers = Arrays.asList(4, 9, 6, 2, 8, 5, 1, 3, 7);

    SimplePageable<Integer> pageableIntegers = SimplePageable.of(integers, 5);

    assertThat(pageableIntegers).isNotNull();
    assertThat(pageableIntegers).isNotEmpty();
    assertThat(pageableIntegers.count()).isEqualTo(2);

    Iterator<Page<Integer>> pages = pageableIntegers.iterator();

    assertThat(pages).isNotNull();
    assertThat(pages.hasNext()).isTrue();

    Page<Integer> pageOne = pages.next();

    assertThat(pageOne).isNotNull();
    assertThat(pageOne).containsExactly(4, 9, 6, 2, 8);
    assertThat(pages.hasNext()).isTrue();

    Page<Integer> pageTwo = pages.next();

    assertThat(pageTwo).isNotNull();
    assertThat(pageTwo).containsExactly(5, 1, 3, 7);
    assertThat(pages.hasNext()).isFalse();

    pageableIntegers.sort(Comparator.naturalOrder());

    assertThat(pageableIntegers.count()).isEqualTo(2);

    pages = pageableIntegers.iterator();

    assertThat(pages).isNotNull();
    assertThat(pages.hasNext()).isTrue();

    pageOne = pages.next();

    assertThat(pageOne).isNotNull();
    assertThat(pageOne).containsExactly(1, 2, 3, 4, 5);
    assertThat(pages.hasNext()).isTrue();

    pageTwo = pages.next();

    assertThat(pageTwo).isNotNull();
    assertThat(pageTwo).containsExactly(6, 7, 8, 9);
    assertThat(pages.hasNext()).isFalse();
  }
}
