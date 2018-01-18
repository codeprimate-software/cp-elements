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

package org.cp.elements.util.paging;

import static org.assertj.core.api.Assertions.assertThat;
import static org.cp.elements.util.ArrayUtils.asIterator;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.mock;
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
  public void getPageWhePageExists() {

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
  public void getPageWhePageDoesNotExistThrowsException() {

    Page<Object> mockPage = mock(Page.class);

    Pageable<Object> mockPageable = mock(Pageable.class);

    when(mockPageable.iterator()).thenReturn(asIterator(mockPage));
    when(mockPageable.getPage(anyInt())).thenCallRealMethod();

    try {
      mockPageable.getPage(2);
    }
    catch (PageNotFoundException expected) {

      assertThat(expected).hasMessage("Page with number [2] not found");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockPageable, times(1)).iterator();
    }
  }
}
