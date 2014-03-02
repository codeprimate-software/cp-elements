/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.util.sort;

import java.util.AbstractList;
import java.util.Comparator;
import java.util.List;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ObjectUtils;

/**
 * The AbstractSorter class is base class encapsulating functionality common to all Sorter implementations.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.util.sort.Sorter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractSorter implements Sorter {

  private Comparator orderBy;

  /**
   * Gets the Comparator used to order the elements in the collection.
   * <p/>
   * @param <E> the type of elements in the Collection to compare.
   * @return the Comparator used to order the collection elements.
   * @see java.util.Comparator
   */
  @Override
  @SuppressWarnings("unchecked")
  public <E> Comparator<E> getOrderBy() {
    return ObjectUtils.defaultIfNull(orderBy, ComparableComparator.INSTANCE);
  }

  /**
   * Sets the Comparator used to order the elements in the collection.
   * <p/>
   * @param orderBy the Comparator to use for ordering the collection elements.
   * @see java.util.Comparator
   */
  public void setOrderBy(final Comparator orderBy) {
    this.orderBy = orderBy;
  }

  /**
   * Sorts an array of elements as defined by the Comparator, or as determined by the elements in the array
   * if the elements are Comparable.
   * <p/>
   * @param <E> the type of elements in the array.
   * @param elements the array of elements to sort.
   * @return the array of elements sorted.
   * @see #sort(java.util.List)
   * @see java.util.Arrays#asList(Object[])
   * @see java.util.ArrayList
   */
  @Override
  @SuppressWarnings("unchecked")
  public <E> E[] sort(final E... elements) {
    sort(new SortableArrayList(elements));
    return elements;
  }

  /**
   * Swaps elements at 2 different positions (indexes) in the List of elements.
   * <p/>
   * @param <E> the Class type of the elements in the List.
   * @param elements the List of elements to perform an element swap on.
   * @param index1 the index of the first element to swap.
   * @param index2 the index of the second element to swap.
   * @see java.util.List
   */
  protected <E> void swap(final List<E> elements, final int index1, final int index2) {
    E elementFromIndex1 = elements.get(index1);
    elements.set(index1, elements.get(index2));
    elements.set(index2, elementFromIndex1);
  }

  /**
   * The ComparableComparator class is a Comparator implementation comparing two Comparable objects using the natural
   * ordering of the objects to determine order.
   * <p/>
   * @param <T> the Comparable Class type of the objects to compare.
   */
  protected static class ComparableComparator<T extends Comparable<T>> implements Comparator<T> {

    protected static final ComparableComparator INSTANCE = new ComparableComparator();

    @Override
    public int compare(final T comparable1, final T comparable2) {
      return comparable1.compareTo(comparable2);
    }
  }

  /**
   * The SorableArrayList class is a List implementation wrapping an modifiable array of elements that can be sorted.
   * <p/>
   * @param <E> the Class type of the elements in the array/List.
   * @see java.util.AbstractList
   */
  protected static class SortableArrayList<E> extends AbstractList<E> {

    private final E[] elements;

    public SortableArrayList(final E... elements) {
      Assert.notNull(elements, "The array of elements to wrap in a List cannot be null!");
      this.elements = elements;
    }

    @Override
    public E get(final int index) {
      return elements[index];
    }

    @Override
    public E set(final int index, final E element) {
      E previousElement = elements[index];
      elements[index] = element;
      return previousElement;
    }

    @Override
    public int size() {
      return elements.length;
    }
  }

}
