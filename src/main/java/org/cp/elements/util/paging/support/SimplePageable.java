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

import static org.cp.elements.lang.ElementsExceptionsFactory.newPageNotFoundException;
import static org.cp.elements.util.ArrayUtils.nullSafeArray;
import static org.cp.elements.util.CollectionUtils.nullSafeList;

import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

import org.cp.elements.lang.Assert;
import org.cp.elements.util.paging.Page;
import org.cp.elements.util.paging.PageNotFoundException;
import org.cp.elements.util.paging.Pageable;

/**
 * The {@link SimplePageable} class is an implementation {@link Pageable} that adapts, or wraps either an array
 * or a {@link List} to provide paging capabilities.
 *
 * @author John Blum
 * @param <T> {@link Class type} of the elements or items contained in the {@link Page pages}
 * of this {@link Pageable} object.
 * @see java.util.Iterator
 * @see org.cp.elements.util.paging.Page
 * @see org.cp.elements.util.paging.Pageable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class SimplePageable<T> implements Pageable<T> {

  protected static final int DEFAULT_PAGE_SIZE = 20;

  /**
   * Factory method used to construct a new empty {@link SimplePageable}.
   *
   * @param <T> {@link Class type} of elements contained in the returned {@link SimplePageable}.
   * @return a new empty {@link SimplePageable}.
   * @see #of(List)
   */
  public static <T> SimplePageable<T> empty() {
    return of(Collections.emptyList());
  }

  /**
   * Factory method used to construct a new instance of {@link SimplePageable} from the given array.
   *
   * The returned {@link Pageable} object wraps the given array to provide paging functionality.
   * The {@link Pageable} object uses the default page size of 20 elements per {@link Page}.
   *
   * @param <T> {@link Class type} of elements contained in the array.
   * @param array {@link Object} array to wrap and provide paging functionality for.
   * @return a new instance of {@link Pageable} wrapping the given array to provide paging functionality.
   * @see #SimplePageable(List)
   */
  @SafeVarargs
  public static <T> SimplePageable<T> of(T... array) {
    return new SimplePageable<>(Arrays.asList(nullSafeArray(array)));
  }

  /**
   * Factory method used to construct a new instance of {@link SimplePageable} for the given {@link List}.
   *
   * The returned {@link Pageable} object wraps the given {@link List} to provide paging functionality.
   * The {@link Pageable} object uses the default page size of 20 elements per {@link Page}.
   *
   * @param <T> {@link Class type} of elements contained in the {@link List}.
   * @param list {@link List} to wrap and provide paging functionality for.
   * @return a new instance of {@link Pageable} wrapping the given {@link List} to provide paging functionality.
   * @see #SimplePageable(List)
   * @see java.util.List
   */
  public static <T> SimplePageable<T> of(List<T> list) {
    return new SimplePageable<>(nullSafeList(list));
  }

  private int pageSize;

  private final List<T> list;

  /**
   * Constructs a new instance of {@link SimplePageable} initialized with the given {@link List}
   * and default page size of 20 elements per {@link Page}.
   *
   * @param list {@link List} to wrap and provide paging functionality for.
   * @throws IllegalArgumentException if the {@link List} is {@literal null}.
   * @see #SimplePageable(List, int)
   */
  public SimplePageable(List<T> list) {
    this(list, DEFAULT_PAGE_SIZE);
  }

  /**
   * Constructs a new instance of {@link SimplePageable} initialized with the given {@link List} and given page size.
   *
   * @param list {@link List} to wrap and provide paging functionality for.
   * @param pageSize integer indicating the desired number of elements per {@link Page}
   * @throws IllegalArgumentException if the {@link List} is {@literal null} or the {@code pageSize}
   * is less than equal to {@literal 0}.
   */
  public SimplePageable(List<T> list, int pageSize) {

    Assert.notNull(list, "List is required");
    Assert.isTrue(pageSize > 0, "Page size [%d] must be greater than 0", pageSize);

    this.list = list;
    this.pageSize = pageSize;
  }

  /**
   * Returns a reference to the wrapped {@link List} for which paging functionality is provided.
   *
   * @return a reference to the wrapped {@link List} for which paging functionality is provided.
   * @see java.util.List
   */
  protected List<T> getList() {
    return this.list;
  }

  /**
   * Returns the configured page size for this {@link Pageable}, which determines
   * the number of elements per {@link Page}.
   *
   * @return an integer value indicating the configured number of elements per {@link Page}.
   */
  protected int getPageSize() {
    return this.pageSize;
  }

  /**
   * Get the total number of {@link Page pages} in this {@link Pageable} object.
   *
   * @return an int value indicating the total number of {@link Page pages} in this {@link Pageable} object.
   */
  @Override
  public int count() {

    int listSize = getList().size();
    int pageSize = getPageSize();
    int pageCount = listSize / pageSize;

    pageCount += listSize % pageSize > 0 ? 1 : 0;

    return pageCount;
  }

  /**
   * Determines whether this {@link Pageable} object contains any {@link Page Pages}.
   *
   * @return a boolean value indicating whether this {@link Pageable} object contains any {@link Page Pages}.
   * @see #count()
   */
  @Override
  public boolean isEmpty() {
    return getList().isEmpty();
  }

  /**
   * Iterates over the {@link Page Pages} contained in this {@link Pageable object}.
   *
   * @return an {@link Iterator} over the {@link Page Pages} contained in this {@link Pageable object}.
   * @see java.util.Iterator
   */
  @Override
  public Iterator<Page<T>> iterator() {

    return new Iterator<Page<T>>() {

      int currentIndex = 0;
      int currentPageNumber = 1;
      int pageSize = getPageSize();

      @Override
      public boolean hasNext() {
        return this.currentIndex < getList().size();
      }

      @Override
      public Page<T> next() {

        Assert.isTrue(hasNext(), new NoSuchElementException("No more pages"));

        Page<T> page = SimplePage.of(SimplePageable.this, this.currentPageNumber);

        this.currentIndex += this.pageSize;
        this.currentPageNumber++;

        return page;
      }
    };
  }

  /**
   * Sorts all the elements contained in this {@link Pageable} object.
   *
   * @param orderBy {@link Comparator} used to sort (order) all the elements across all the {@link Page Pages}
   * contained by this {@link Pageable}.
   * @see java.util.Comparator
   * @see #getList()
   */
  @Override
  public void sort(Comparator<T> orderBy) {
    getList().sort(orderBy);
  }

  /**
   * Sets the size used by this {@link Pageable} for dividing elements into {@link Page Pages} using the given value.
   *
   * @param pageSize integer indicating the number of elements per {@link Page}.
   * @return this {@link SimplePageable}.
   * @throws IllegalArgumentException if {@code pageSize} is less than equal to {@literal 0}.
   */
  public SimplePageable<T> with(int pageSize) {

    Assert.isTrue(pageSize > 0, "Page size [%d] must be greater than 0", pageSize);

    this.pageSize = pageSize;

    return this;
  }

  /**
   * {@link SimplePage} is an Abstract Data Type (ADT) modeling a single {@link Page} contained by
   * this {@link SimplePageable} object.
   *
   * @param <T> {@link Class type} of the elements contained in this {@link Page}.
   * @see org.cp.elements.util.paging.Page
   */
  protected static class SimplePage<T> implements Page<T> {

    private final int pageNumber;

    private final List<T> elements;

    private final SimplePageable<T> pageable;

    /**
     * Factory method used to construct a new instance of {@link SimplePage} initialized with the source,
     * underlying {@link SimplePageable} object containing this {@link Page} and this {@link Page Page's}
     * page number.
     *
     * This factory method constructs a new {@link SimplePage} for the first {@link Page}
     * in the {@link Pageable} object.
     *
     * @param <T> {@link Class type} of the elements contained in this {@link Page}.
     * @param pageable reference to the {@link SimplePageable} object that contains this {@link Page}.
     * @return a new {@link SimplePage} linked to the given {@link SimplePageable}.
     * @throws IllegalArgumentException if the {@link SimplePageable} object is {@literal null}
     * or the {@link Pageable} object is empty.
     * @see org.cp.elements.util.paging.support.SimplePageable
     * @see #of(SimplePageable, int)
     */
    protected static <T> SimplePage<T> of(SimplePageable<T> pageable) {
      return of(pageable, 1);
    }

    /**
     * Factory method used to construct a new instance of {@link SimplePage} initialized with the source,
     * underlying {@link SimplePageable} object containing this {@link Page} and this {@link Page Page's}
     * page number.
     *
     * @param <T> {@link Class type} of the elements contained in this {@link Page}.
     * @param pageable reference to the {@link SimplePageable} object that contains this {@link Page}.
     * @param pageNumber integer value indicating this {@link Page Page's} page number in
     * the collection of {@link Page Pages} contained by the {@link SimplePageable} object.
     * @return a new {@link SimplePage} linked to the given {@link SimplePageable}.
     * @throws IllegalArgumentException if the {@link SimplePageable} object is {@literal null},
     * the {@link Pageable} object is empty or the page number exceeds the number of {@link Page Pages}
     * contained by the given {@link Pageable} object.
     */
    protected static <T> SimplePage<T> of(SimplePageable<T> pageable, int pageNumber) {
      return new SimplePage<>(pageable, pageNumber);
    }

    /**
     * Constructs a new instance of {@link SimplePage} initialized with the source, underlying {@link SimplePageable}
     * object containing this {@link Page} and this {@link Page Page's} page number.
     *
     * @param pageable reference to the {@link SimplePageable} object that contains this {@link Page}.
     * @param pageNumber integer value indicating this {@link Page Page's} page number in
     * the collection of {@link Page Pages} contained by the {@link SimplePageable} object.
     * @throws IllegalArgumentException if the {@link SimplePageable} object is {@literal null},
     * the {@link Pageable} object is empty or the page number exceeds the number of {@link Page Pages}
     * contained by the given {@link Pageable} object.
     * @see org.cp.elements.util.paging.support.SimplePageable
     */
    protected SimplePage(SimplePageable<T> pageable, int pageNumber) {

      Assert.notNull(pageable, "Pageable is required");
      Assert.isFalse(pageable.isEmpty(), "Pageable object must contain pages");

      int pageCount = pageable.count();

      Assert.isTrue(pageNumber > 0, "Page number [%d] must be greater than 0", pageNumber);

      Assert.isTrue(pageNumber <= pageCount,
        "Page number [%1$d] must be less than equal to the number of pages [%2$d]", pageNumber, pageCount);

      List<T> list = pageable.getList();

      int pageSize = pageable.getPageSize();
      int fromIndex = Math.max((pageNumber - 1) * pageSize, 0);
      int toIndex = Math.min(fromIndex + pageSize, list.size());

      this.pageable = pageable;
      this.pageNumber = pageNumber;
      this.elements = list.subList(fromIndex, toIndex);
    }

    /**
     * Returns a {@link List} of elements contained in this {@link Page}.
     *
     * @return a {@link List} of elements contained in this {@link Page}.
     * @see java.util.List
     */
    protected List<T> getElements() {
      return this.elements;
    }

    /**
     * Returns this {@link Page Page's} page number.
     *
     * @return an integer value with this {@link Page Page's} page number.
     */
    @Override
    public int getNumber() {
      return this.pageNumber;
    }

    /**
     * Returns a reference to the backing {@link Pageable} object.
     *
     * @return a reference to the backing {@link Pageable} object.
     * @see org.cp.elements.util.paging.support.SimplePageable
     */
    protected SimplePageable<T> getPageable() {
      return this.pageable;
    }

    /**
     * Determines whether there is a next {@link Page}.
     *
     * @return a boolean value indicating whether there is a next {@link Page}.
     */
    @Override
    public boolean hasNext() {

      int pageSize = getPageable().getPageSize();

      return getNumber() * pageSize < getPageable().getList().size();
    }

    /**
     * Returns the next {@link Page} in the sequence of {@link Page Pages}.
     *
     * @return the next {@link Page} in the sequence of {@link Page Pages}.
     * @throws PageNotFoundException if there is no next {@link Page}.
     * @see org.cp.elements.util.paging.Page
     */
    @Override
    public Page<T> next() {

      Assert.isTrue(hasNext(), newPageNotFoundException("No next page after [%d]", getNumber()));

      return getPageable().getPage(getNumber() + 1);
    }

    /**
     * Returns an {@link Iterator} over the elements contained this {@link Page}.
     *
     * @return an {@link Iterator} over the elements contained this {@link Page}.
     * @see java.util.Iterator
     */
    @Override
    public Iterator<T> iterator() {
      return getElements().iterator();
    }

    /**
     * Determines whether there is a previous {@link Page}.
     *
     * @return a boolean value indicating whether there is a previous {@link Page}.
     */
    @Override
    public boolean hasPrevious() {
      return getNumber() > 1;
    }

    /**
     * Returns the previous {@link Page} in the sequence of {@link Page Pages}.
     *
     * @return the previous {@link Page} in the sequence of {@link Page Pages}.
     * @throws PageNotFoundException if there is no previous {@link Page}.
     * @see org.cp.elements.util.paging.Page
     */
    @Override
    public Page<T> previous() {

      Assert.isTrue(hasPrevious(), newPageNotFoundException("No previous page before [%d]", getNumber()));

      return getPageable().getPage(getNumber() - 1);
    }

    /**
     * Indicates the number of elements or items contained in this {@link Page}.
     *
     * @return an integer value indicating the number of elements or items contained in this {@link Page}.
     */
    @Override
    public int size() {
      return getElements().size();
    }

    /**
     * Sorts (orders) only the elements contained in this {@link Page}.
     *
     * @param orderBy {@link Comparator} used to sort (order) only the elements contained in this {@link Page}.
     * @see java.util.Comparator
     */
    @Override
    public void sort(Comparator<T> orderBy) {
      getElements().sort(orderBy);
    }
  }
}
