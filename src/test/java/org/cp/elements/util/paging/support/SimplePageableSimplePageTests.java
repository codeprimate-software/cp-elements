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
import static org.cp.elements.util.paging.support.SimplePageable.SimplePage;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.IntStream;

import org.junit.jupiter.api.Test;

import org.cp.elements.util.paging.Page;
import org.cp.elements.util.paging.PageNotFoundException;

/**
 * Unit Tests for {@link SimplePageable.SimplePage}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.util.paging.Page
 * @see org.cp.elements.util.paging.Pageable
 * @see org.cp.elements.util.paging.support.SimplePageable
 * @see org.cp.elements.util.paging.support.SimplePageable.SimplePage
 * @since 1.0.0
 */
class SimplePageableSimplePageTests {

  private List<Integer> newList(int size) {
    List<Integer> list = new ArrayList<>(size);
    IntStream.range(0, size).forEach(list::add);
    return list;
  }

  @Test
  void constructSimplePageForPageOneInPageable() {

    List<?> list = newList(SimplePageable.DEFAULT_PAGE_SIZE * 2);

    SimplePageable<?> pageable = SimplePageable.of(list);

    SimplePage<?> page = SimplePage.of(pageable);

    assertThat(page).isNotNull();
    assertThat(page.getNumber()).isEqualTo(1);
    assertThat(page.getPageable()).isSameAs(pageable);
    assertThat(page.getElements()).isEqualTo(list.subList(0, SimplePageable.DEFAULT_PAGE_SIZE));
  }

  @Test
  @SuppressWarnings({ "unchecked", "all" })
  void constructSimplePageForPageTwoInPageable() {

    List<?> list = newList(SimplePageable.DEFAULT_PAGE_SIZE * 2);

    SimplePageable<?> pageable = SimplePageable.of(list);

    SimplePage page = SimplePage.of(pageable, 2);

    assertThat(page).isNotNull();
    assertThat(page.getNumber()).isEqualTo(2);
    assertThat(page.getPageable()).isSameAs(pageable);
    assertThat(page.getElements()).isEqualTo(list.subList(SimplePageable.DEFAULT_PAGE_SIZE, list.size()));
  }

  @Test
  void constructSimplePageWithNullPageableThrowsException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> SimplePage.of(null))
      .withMessage("Pageable is required")
      .withNoCause();
  }

  @Test
  void constructSimplePageWithEmptyPageableThrowsException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> SimplePage.of(SimplePageable.empty()))
      .withMessage("Pageable object must contain pages")
      .withNoCause();
  }

  @Test
  void constructSimplePageWithNegativePageNumberThrowsException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> SimplePage.of(SimplePageable.of("test"), -1))
      .withMessage("Page number [-1] must be greater than 0")
      .withNoCause();
  }

  @Test
  void constructSimplePageWithPageNumberGreaterThanTheNumberOfPagesThrowsException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> SimplePage.of(SimplePageable.of("test"), 2))
      .withMessage("Page number [2] must be less than equal to the number of pages [1]")
      .withNoCause();
  }

  @Test
  void hasNextFromPageOneWithTwoPagesReturnsTrue() {

    List<?> list = newList(SimplePageable.DEFAULT_PAGE_SIZE + 1);

    SimplePageable<?> pageable = SimplePageable.of(list);

    assertThat(pageable).isNotNull();
    assertThat(pageable.count()).isEqualTo(2);

    Page<?> page = pageable.getPage(1);

    assertThat(page).isNotNull();
    assertThat(page.getNumber()).isEqualTo(1);
    assertThat(page.hasNext()).isTrue();
  }

  @Test
  void hasNextFromPageTwoWithTwoPagesReturnsFalse() {

    List<?> list = newList(SimplePageable.DEFAULT_PAGE_SIZE * 2);

    SimplePageable<?> pageable = SimplePageable.of(list);

    assertThat(pageable).isNotNull();
    assertThat(pageable.count()).isEqualTo(2);

    Page<?> page = pageable.getPage(2);

    assertThat(page).isNotNull();
    assertThat(page.getNumber()).isEqualTo(2);
    assertThat(page.hasNext()).isFalse();
  }

  @Test
  void nextPageFromPageOneWithTwoPagesReturnsNextPage() {

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

  @Test
  void nextPageFromPageOneWithOnePageThrowsException() {

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

    assertThatExceptionOfType(PageNotFoundException.class)
      .isThrownBy(pageOne::next)
      .withMessage("No next page after [1]")
      .withNoCause();
  }

  @Test
  void hasPreviousFromPageTwoReturnsTrue() {

    List<?> list = newList(SimplePageable.DEFAULT_PAGE_SIZE * 2);

    SimplePageable<?> pageable = SimplePageable.of(list);

    assertThat(pageable).isNotNull();
    assertThat(pageable.count()).isEqualTo(2);

    Page<?> page = pageable.getPage(2);

    assertThat(page).isNotNull();
    assertThat(page.getNumber()).isEqualTo(2);
    assertThat(page.hasPrevious()).isTrue();
  }

  @Test
  void hasPreviousFromPageOneReturnsFalse() {

    List<?> list = newList(SimplePageable.DEFAULT_PAGE_SIZE * 2);

    SimplePageable<?> pageable = SimplePageable.of(list);

    assertThat(pageable).isNotNull();
    assertThat(pageable.count()).isEqualTo(2);

    Page<?> page = pageable.getPage(1);

    assertThat(page).isNotNull();
    assertThat(page.getNumber()).isEqualTo(1);
    assertThat(page.hasPrevious()).isFalse();
  }

  @Test
  void previousPageFromPageTwoReturnsPageOne() {

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

  @Test
  void previousPageFromPageOneThrowsException() {

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

    assertThatExceptionOfType(PageNotFoundException.class)
      .isThrownBy(pageOne::previous)
      .withMessage("No previous page before [1]")
      .withNoCause();
  }

  @Test
  void sortPageIsCorrect() {

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
