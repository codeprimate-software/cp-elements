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
package org.cp.elements.util.paging.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.stream.IntStream;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.ThrowableAssertions;
import org.cp.elements.util.paging.Page;
import org.cp.elements.util.paging.PageNotFoundException;

/**
 * Unit Tests for {@link SimplePageable}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.util.paging.support.SimplePageable
 * @since 1.0.0
 */
class SimplePageableTests {

  private List<Integer> newList(int size) {
    List<Integer> list = new ArrayList<>(size);
    IntStream.range(0, size).forEach(list::add);
    return list;
  }

  @Test
  void constructSimplePageableWithArrayAndDefaultPageSize() {

    SimplePageable<Object> pageable = SimplePageable.of("test", "testing", "tested");

    assertThat(pageable).isNotNull();
    assertThat(pageable.getList()).containsExactly("test", "testing", "tested");
    assertThat(pageable.getPageSize()).isEqualTo(SimplePageable.DEFAULT_PAGE_SIZE);
  }

  @Test
  void constructSimplePageableWithArrayAndPageSize() {

    SimplePageable<Object> pageable = SimplePageable.<Object>of("test", "testing", "tested").with(1);

    assertThat(pageable).isNotNull();
    assertThat(pageable.getList()).containsExactly("test", "testing", "tested");
    assertThat(pageable.getPageSize()).isEqualTo(1);
  }

  @Test
  void constructSimplePageableWithListAndDefaultPageSize() {

    List<Object> list = Arrays.asList("test", "testing", "tested");

    SimplePageable<Object> pageable = new SimplePageable<>(list);

    assertThat(pageable).isNotNull();
    assertThat(pageable.getList()).isEqualTo(list);
    assertThat(pageable.getPageSize()).isEqualTo(SimplePageable.DEFAULT_PAGE_SIZE);
  }

  @Test
  void constructSimplePageableWithListAndPageSize() {

    List<Object> list = Arrays.asList("test", "testing", "tested");

    SimplePageable<Object> pageable = SimplePageable.of(list).with(3);

    assertThat(pageable).isNotNull();
    assertThat(pageable.getList()).isEqualTo(list);
    assertThat(pageable.getPageSize()).isEqualTo(3);
  }

  @Test
  void constructSimplePageableWithInvalidPageSize() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new SimplePageable<>(Collections.emptyList(), -1))
      .withMessage("Page size [-1] must be greater than 0")
      .withNoCause();
  }

  @Test
  void constructSimplePageableWithNullList() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new SimplePageable<>(null))
      .withMessage("List is required")
      .withNoCause();
  }

  @Test
  void emptySimplePageable() {

    SimplePageable<Object> pageable = SimplePageable.empty();

    assertThat(pageable).isNotNull();
    assertThat(pageable).isEmpty();
    assertThat(pageable.getList()).isEmpty();
    assertThat(pageable.getPageSize()).isEqualTo(SimplePageable.DEFAULT_PAGE_SIZE);
  }

  @Test
  void simplePageableOfNullArray() {

    SimplePageable<Object> pageable = SimplePageable.of((Object[]) null);

    assertThat(pageable).isNotNull();
    assertThat(pageable).isEmpty();
    assertThat(pageable.getList()).isEmpty();
    assertThat(pageable.getPageSize()).isEqualTo(SimplePageable.DEFAULT_PAGE_SIZE);
  }

  @Test
  void simplePageableOfNullList() {

    SimplePageable<Object> pageable = SimplePageable.of((List<Object>) null).with(10);

    assertThat(pageable).isNotNull();
    assertThat(pageable).isEmpty();
    assertThat(pageable.getList()).isEmpty();
    assertThat(pageable.getPageSize()).isEqualTo(10);
  }

  @Test
  void pageCountForListSizeOfZeroIsZero() {

    SimplePageable<?> pageable = SimplePageable.empty();

    assertThat(pageable).isNotNull();
    assertThat(pageable.count()).isEqualTo(0);
  }

  @Test
  void pageCountForListSizeOfOneUsingDefaultPageSizeIsOne() {

    SimplePageable<?> pageable = SimplePageable.of(Collections.singletonList("test"));

    assertThat(pageable).isNotNull();
    assertThat(pageable.count()).isEqualTo(1);
  }

  @Test
  void pageCountForListSizeOfTwoTimesDefaultPageSizeUsingDefaultPageSizeIsTwo() {

    List<?> mockList = newList(SimplePageable.DEFAULT_PAGE_SIZE * 2);

    SimplePageable<?> pageable = SimplePageable.of(mockList);

    assertThat(pageable).isNotNull();
    assertThat(pageable.count()).isEqualTo(2);
  }

  @Test
  void pageCountForListSizeOfTwoUsingPageSizeOfOneIsTwo() {

    SimplePageable<?> pageable = SimplePageable.of(Arrays.asList("test", "testing")).with(1);

    assertThat(pageable).isNotNull();
    assertThat(pageable.count()).isEqualTo(2);
  }

  @Test
  void pageCountForListSizeOfTwoUsingPageSizeOfTwoIsOne() {

    SimplePageable<?> pageable = SimplePageable.of(Arrays.asList("test", "testing")).with(2);

    assertThat(pageable).isNotNull();
    assertThat(pageable.count()).isEqualTo(1);
  }

  @Test
  void isEmptyOnSimplePageableCallsListIsEmpty() {

    List<?> list = newList(0);

    SimplePageable<?> pageable = SimplePageable.of(list);

    assertThat(pageable.isEmpty()).isTrue();
  }

  @Test
  @SuppressWarnings("unchecked")
  void firstPageOfSinglePageSimplePageableReturnsFirstPage() {

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
  void firstPageOfMultiPageSimplePageableReturnsFirstPage() {

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

  @Test
  void firstPageOfEmptySimplePageableThrowsException() {

    ThrowableAssertions.assertThatThrowableOfType(PageNotFoundException.class)
      .isThrownBy(args -> SimplePageable.empty().firstPage())
      .havingMessage("No first page")
      .causedBy(PageNotFoundException.class)
      .havingMessage( "Page with number [1] not found")
      .withNoCause();
  }

  @Test
  void iteratorNextOnSimplePageableWithNoPagesThrowsException() {

    assertThatExceptionOfType(NoSuchElementException.class)
      .isThrownBy(() -> {

        SimplePageable<Object> pageable = SimplePageable.of();

        assertThat(pageable).isNotNull();
        assertThat(pageable).isEmpty();

        Iterator<Page<Object>> pages = pageable.iterator();

        assertThat(pages).isNotNull();
        assertThat(pages.hasNext()).isFalse();

        pages.next();
      })
      .withMessage("No more pages")
      .withNoCause();
  }

  @Test
  @SuppressWarnings("unchecked")
  void lastPageWithSinglePageSimplePageableReturnsLastPage() {

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
  void lastPageWithMultiPageSimplePageableReturnsLastPage() {

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

  @Test
  void lastPageWithEmptySimplePageableReturnsLastPage() {

    ThrowableAssertions.assertThatThrowableOfType(PageNotFoundException.class)
      .isThrownBy(args -> SimplePageable.empty().lastPage())
      .havingMessage("No last page")
      .causedBy(IllegalArgumentException.class)
      .havingMessage("Page number [0] must be greater than 0")
      .withNoCause();
  }

  @Test
  void simplePageableWithNoPages() {

    SimplePageable<Object> pageable = SimplePageable.of();

    assertThat(pageable).isNotNull();
    assertThat(pageable).isEmpty();
    assertThat(pageable.count()).isEqualTo(0);

    Iterator<Page<Object>> pages = pageable.iterator();

    assertThat(pages).isNotNull();
    assertThat(pages.hasNext()).isFalse();
  }

  @Test
  void simplePageableWithSinglePage() {

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
  void simplePageableWithThreeFullPages() {

    List<Integer> integers = Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8, 9);

    SimplePageable<Integer> pageableIntegers = SimplePageable.of(integers).with(3);

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
  void simplePageableWithTwoAndOneHalfPages() {

    List<Integer> integers = Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8, 9);

    SimplePageable<Integer> pageableIntegers = SimplePageable.of(integers).with(4);

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
  void sortPageableIsCorrect() {

    List<Integer> integers = Arrays.asList(4, 9, 6, 2, 8, 5, 1, 3, 7);

    SimplePageable<Integer> pageableIntegers = SimplePageable.of(integers).with(5);

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

  @Test
  void withCorrectPageSize() {

    SimplePageable<?> pageable = SimplePageable.empty().with(10);

    assertThat(pageable).isNotNull();
    assertThat(pageable.getPageSize()).isEqualTo(10);
  }

  @Test
  void withInvalidPageSizeThrowsException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> SimplePageable.empty().with(-1))
      .withMessage("Page size [-1] must be greater than 0")
      .withNoCause();
  }

  @Test
  void withPageSizeZeroThrowsException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> SimplePageable.empty().with(0))
      .withMessage("Page size [0] must be greater than 0")
      .withNoCause();
  }
}
