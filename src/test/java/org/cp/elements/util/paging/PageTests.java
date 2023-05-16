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
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;

import org.junit.jupiter.api.Test;

/**
 * Unit tests for {@link Page}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.util.paging.Page
 * @since 1.0.0
 */
public class PageTests {

  @Test
  public void sizeOfEmptyPageReturnsZero() {

    Page<?> mockPage = mock(Page.class);

    when(mockPage.iterator()).thenReturn(Collections.emptyIterator());
    when(mockPage.spliterator()).thenCallRealMethod();
    when(mockPage.size()).thenCallRealMethod();

    assertThat(mockPage.size()).isZero();

    verify(mockPage, times(1)).iterator();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void sizeOfNonEmptyPageReturnsSize() {

    Page<Object> mockPage = mock(Page.class);

    when(mockPage.iterator()).thenReturn(asIterator(1, 2, 3));
    when(mockPage.spliterator()).thenCallRealMethod();
    when(mockPage.size()).thenCallRealMethod();

    assertThat(mockPage.size()).isEqualTo(3);

    verify(mockPage, times(1)).iterator();
  }
}
