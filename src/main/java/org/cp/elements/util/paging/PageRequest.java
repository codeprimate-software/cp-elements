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

import org.cp.elements.lang.Orderable;

/**
 * The {@link PageRequest} class models a request in a pageable (paging) operation
 * that indicates the starting {@link Page} as well as the number of elements or items
 * per {@link Page}.
 *
 * @author John J. Blum
 * @see java.lang.Comparable
 * @see org.cp.elements.lang.Orderable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface PageRequest<T extends Comparable<T>> extends Orderable<T> {

  /**
   * Returns an {@link Integer#TYPE integer value} indicating the number of {@link Page pages} to prefetch.
   *
   * @return an {@link Integer#TYPE integer value} indicating the number of {@link Page pages} to prefetch.
   */
  int getFetchSize();

  /**
   * Gets the {@link Integer#TYPE requested size} for the {@link Page pages} returned in the {@link Pageable}.
   *
   * @return an integer value indicated the desired number of elements or items per {@link Page}.
   */
  int getPageSize();

  /**
   * Gets the requested starting {@link Page} within the {@link Pageable}.
   *
   * @return an integer value indicating the {@link Page} number to start at in the {@link Pageable}.
   */
  int getStartingPageNumber();

}
