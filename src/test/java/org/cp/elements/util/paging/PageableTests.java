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

package org.cp.elements.util.paging;

import static org.assertj.core.api.Assertions.assertThat;
import static org.cp.elements.util.ArrayUtils.asIterator;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;

import org.junit.Test;

/**
 * Unit tests for {@link Pageable}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.util.paging.Pageable
 * @since 1.0.0
 */
public class PageableTests {

  @Test
  public void isEmptyWithEmptyPageableObjectReturnsTrue() {

    Pageable<?> mockPageable = mock(Pageable.class);

    when(mockPageable.count()).thenReturn(0);
    when(mockPageable.isEmpty()).thenCallRealMethod();

    assertThat(mockPageable.isEmpty()).isTrue();

    verify(mockPageable, times(1)).count();
  }

  @Test
  public void isEmptyWithNonEmptyPageableObjectReturnsFalse() {

    Pageable<?> mockPageable = mock(Pageable.class);

    when(mockPageable.count()).thenReturn(2);
    when(mockPageable.isEmpty()).thenCallRealMethod();

    assertThat(mockPageable.isEmpty()).isFalse();

    verify(mockPageable, times(1)).count();
  }

  @Test
  public void isEmptyWithPageableObjectHavingNegativeCountReturnsTrue() {

    Pageable<?> mockPageable = mock(Pageable.class);

    when(mockPageable.count()).thenReturn(-1);
    when(mockPageable.isEmpty()).thenCallRealMethod();

    assertThat(mockPageable.isEmpty()).isTrue();

    verify(mockPageable, times(1)).count();
  }

  @Test
  public void isEmptyWithSingleElementPageableObjectReturnsFalse() {

    Pageable<?> mockPageable = mock(Pageable.class);

    when(mockPageable.count()).thenReturn(1);
    when(mockPageable.isEmpty()).thenCallRealMethod();

    assertThat(mockPageable.isEmpty()).isFalse();

    verify(mockPageable, times(1)).count();
  }

  @Test
  public void countWithNoPagesReturnsZero() {

    Pageable<?> mockPageable = mock(Pageable.class);

    when(mockPageable.iterator()).thenReturn(Collections.emptyIterator());
    when(mockPageable.spliterator()).thenCallRealMethod();
    when(mockPageable.count()).thenCallRealMethod();

    assertThat(mockPageable.count()).isZero();

    verify(mockPageable, times(1)).iterator();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void countWithPagesReturnsCount() {

    Page<Object> mockPageOne = mock(Page.class);
    Page<Object> mockPageTwo = mock(Page.class);
    Page<Object> mockPageThree = mock(Page.class);

    Pageable<Object> mockPageable = mock(Pageable.class);

    when(mockPageable.iterator()).thenReturn(asIterator(mockPageOne, mockPageTwo, mockPageThree));
    when(mockPageable.spliterator()).thenCallRealMethod();
    when(mockPageable.count()).thenCallRealMethod();

    assertThat(mockPageable.count()).isEqualTo(3);

    verify(mockPageable, times(1)).iterator();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void firstPageWithMultiplePagesReturnsFirstPage() {

    Page<Object> mockPageOne = mock(Page.class);
    Page<Object> mockPageTwo = mock(Page.class);
    Page<Object> mockPageThree = mock(Page.class);

    Pageable<Object> mockPageable = mock(Pageable.class);

    when(mockPageable.iterator()).thenAnswer(invocation -> asIterator(mockPageOne, mockPageTwo, mockPageThree));
    when(mockPageable.getPage(anyInt())).thenCallRealMethod();
    when(mockPageable.firstPage()).thenCallRealMethod();

    assertThat(mockPageable.firstPage()).isEqualTo(mockPageOne);
    assertThat(mockPageable.firstPage()).isEqualTo(mockPageOne);

    verify(mockPageable, times(2)).getPage(eq(1));
    verify(mockPageable, times(2)).iterator();
  }

  @Test(expected = PageNotFoundException.class)
  @SuppressWarnings("unchecked")
  public void firstPageWithNoPagesThrowsException() {

    Pageable<Object> mockPageable = mock(Pageable.class);

    when(mockPageable.iterator()).thenReturn(Collections.emptyIterator());
    when(mockPageable.getPage(anyInt())).thenCallRealMethod();
    when(mockPageable.firstPage()).thenCallRealMethod();

    try {
      mockPageable.firstPage();
    }
    catch (PageNotFoundException expected) {

      assertThat(expected).hasMessage("No first page");
      assertThat(expected).hasCauseInstanceOf(PageNotFoundException.class);
      assertThat(expected.getCause()).hasMessage("Page with number [1] not found");
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockPageable, times(1)).getPage(eq(1));
      verify(mockPageable, times(1)).iterator();
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void firstPageWithOnePageReturnsFirstPage() {

    Page<Object> mockPage = mock(Page.class);

    Pageable<Object> mockPageable = mock(Pageable.class);

    when(mockPageable.iterator()).thenAnswer(invocation -> asIterator(mockPage));
    when(mockPageable.getPage(anyInt())).thenCallRealMethod();
    when(mockPageable.firstPage()).thenCallRealMethod();

    assertThat(mockPageable.firstPage()).isEqualTo(mockPage);
    assertThat(mockPageable.firstPage()).isEqualTo(mockPage);

    verify(mockPageable, times(2)).getPage(eq(1));
    verify(mockPageable, times(2)).iterator();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void getPageWhenPageExists() {

    Page<Object> mockPageOne = mock(Page.class);
    Page<Object> mockPageTwo = mock(Page.class);
    Page<Object> mockPageThree = mock(Page.class);

    Pageable<Object> mockPageable = mock(Pageable.class);

    when(mockPageable.iterator()).thenAnswer(invocation -> asIterator(mockPageOne, mockPageTwo, mockPageThree));
    when(mockPageable.getPage(anyInt())).thenCallRealMethod();

    assertThat(mockPageable.getPage(2)).isEqualTo(mockPageTwo);
    assertThat(mockPageable.getPage(1)).isEqualTo(mockPageOne);
    assertThat(mockPageable.getPage(3)).isEqualTo(mockPageThree);

    verify(mockPageable, times(3)).iterator();
  }

  @Test(expected = PageNotFoundException.class)
  @SuppressWarnings("unchecked")
  public void getPageWhenPageDoesNotExistThrowsException() {

    Pageable<Object> mockPageable = mock(Pageable.class);

    when(mockPageable.iterator()).thenReturn(Collections.emptyIterator());
    when(mockPageable.getPage(anyInt())).thenCallRealMethod();

    try {
      mockPageable.getPage(1);
    }
    catch (PageNotFoundException expected) {

      assertThat(expected).hasMessage("Page with number [1] not found");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockPageable, times(1)).iterator();
    }
  }

  @Test(expected = IllegalArgumentException.class)
  @SuppressWarnings("unchecked")
  public void getPageWithInvalidPageNumberThrowsException() {

    Pageable<Object> mockPageable = mock(Pageable.class);

    when(mockPageable.getPage(anyInt())).thenCallRealMethod();

    try {
      mockPageable.getPage(-1);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Page number [-1] must be greater than 0");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockPageable, never()).iterator();
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void lastPageWithMultiplePagesReturnsLastPage() {

    Page<Object> mockPageOne = mock(Page.class);
    Page<Object> mockPageTwo = mock(Page.class);
    Page<Object> mockPageThree = mock(Page.class);

    Pageable<Object> mockPageable = mock(Pageable.class);

    when(mockPageable.iterator()).thenAnswer(invocation -> asIterator(mockPageOne, mockPageTwo, mockPageThree));
    when(mockPageable.count()).thenCallRealMethod();
    when(mockPageable.getPage(anyInt())).thenCallRealMethod();
    when(mockPageable.lastPage()).thenCallRealMethod();
    when(mockPageable.spliterator()).thenCallRealMethod();

    assertThat(mockPageable.lastPage()).isEqualTo(mockPageThree);
    assertThat(mockPageable.lastPage()).isEqualTo(mockPageThree);

    verify(mockPageable, times(2)).count();
    verify(mockPageable, times(2)).getPage(eq(3));
    verify(mockPageable, times(4)).iterator();
  }

  @Test(expected = PageNotFoundException.class)
  @SuppressWarnings("unchecked")
  public void lastPageWithNoPagesThrowsException() {

    Pageable<Object> mockPageable = mock(Pageable.class);

    when(mockPageable.iterator()).thenReturn(Collections.emptyIterator());
    when(mockPageable.count()).thenCallRealMethod();
    when(mockPageable.getPage(anyInt())).thenCallRealMethod();
    when(mockPageable.lastPage()).thenCallRealMethod();
    when(mockPageable.spliterator()).thenCallRealMethod();

    try {
      mockPageable.lastPage();
    }
    catch (PageNotFoundException expected) {

      assertThat(expected).hasMessage("No last page");
      assertThat(expected).hasCauseInstanceOf(IllegalArgumentException.class);
      assertThat(expected.getCause()).hasMessage("Page number [0] must be greater than 0");
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockPageable, times(1)).count();
      verify(mockPageable, times(1)).getPage(eq(0));
      verify(mockPageable, times(1)).iterator();
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void lastPageWithOnePagesReturnsLastPage() {

    Page<Object> mockPage = mock(Page.class);

    Pageable<Object> mockPageable = mock(Pageable.class);

    when(mockPageable.iterator()).thenAnswer(invocation -> asIterator(mockPage));
    when(mockPageable.count()).thenCallRealMethod();
    when(mockPageable.getPage(anyInt())).thenCallRealMethod();
    when(mockPageable.lastPage()).thenCallRealMethod();
    when(mockPageable.spliterator()).thenCallRealMethod();

    assertThat(mockPageable.lastPage()).isEqualTo(mockPage);
    assertThat(mockPageable.lastPage()).isEqualTo(mockPage);

    verify(mockPageable, times(2)).count();
    verify(mockPageable, times(2)).getPage(eq(1));
    verify(mockPageable, times(4)).iterator();
  }
}
