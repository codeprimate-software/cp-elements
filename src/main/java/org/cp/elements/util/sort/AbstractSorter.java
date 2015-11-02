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

import java.lang.reflect.Method;
import java.util.AbstractList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.util.search.SearchException;

/**
 * The AbstractSorter class is base class encapsulating functionality common to all Sorter implementations.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.util.sort.Sorter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractSorter implements Sorter {

  protected static final boolean DEFAULT_CUSTOM_COMPARATOR_ALLOWED = true;

  private boolean customComparatorAllowed = DEFAULT_CUSTOM_COMPARATOR_ALLOWED;

  private Comparator orderBy;

  /**
   * Determines whether a custom Comparator is allowed by this Sorter when sorting and ordering elements in the
   * collection to be sorted.
   * <p/>
   * @return a boolean value indicating whether a custom Comparator is allowed by this Sorter when sorting and ordering
   * elements in the collection being sorted.
   * @see #setCustomComparatorAllowed(boolean)
   */
  public boolean isCustomComparatorAllowed() {
    return customComparatorAllowed;
  }

  /**
   * Sets whether a custom Comparator is allowed by this Sorter when sorting and ordering elements in the collection
   * to be sorted.
   * <p/>
   * @param customComparatorAllowed a boolean value indicating whether a custom Comparator is allowed by this Sorter
   * when sorting and ordering elements in the collection being sorted.
   * @see #isCustomComparatorAllowed()
   */
  public void setCustomComparatorAllowed(final boolean customComparatorAllowed) {
    this.customComparatorAllowed = customComparatorAllowed;
  }

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
    return ObjectUtils.defaultIfNull(ComparatorHolder.get(), ObjectUtils.defaultIfNull(
      orderBy, ComparableComparator.INSTANCE));
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
   * Sorts the List representation of the Sortable implementing object as defined by the 'orderBy' Comparator, or as
   * determined by elements in the Sortable collection if the elements are Comparable.
   * <p/>
   * @param <E> the Class type of elements in the Sortable.
   * @param sortable the Sortable implementing object containing the collection of elements to sort.
   * @return the Sortable implementing object sorted.
   * @see #configureComparator(Sortable)
   * @see #getOrderBy()
   * @see #sort(java.util.List)
   * @see org.cp.elements.util.sort.Sortable#asList()
   */
  @Override
  public <E> Sortable<E> sort(final Sortable<E> sortable) {
    try {
      sort(configureComparator(sortable).asList());
      return sortable;
    }
    finally {
      ComparatorHolder.unset();
    }
  }

  /**
   * Sorts the List representation of the @Sortable annotated object as defined by the 'orderBy' Comparator, or as
   * determined by elements in the Sortable collection if the elements are Comparable.
   * <p/>
   * @param <T> the Class type of object annotated with the @Sortable annotation.
   * @param sortableAnnotatedObject the @Sortable annotated object containing the collection of elements to sort.
   * @return the @Sortable annotated object sorted.
   * @see #asList(Object, org.cp.elements.util.sort.annotation.Sortable)
   * @see #configureComparator(org.cp.elements.util.sort.annotation.Sortable)
   * @see #getSortableMetaData(Object)
   * @see #getOrderBy()
   * @see #sort(java.util.List)
   * @see org.cp.elements.util.sort.annotation.Sortable#listMethod()
   */
  @Override
  public <T> T sort(final T sortableAnnotatedObject) {
    try {
      sort(asList(sortableAnnotatedObject, configureComparator(getSortableMetaData(sortableAnnotatedObject))));
      return sortableAnnotatedObject;
    }
    finally {
      ComparatorHolder.unset();
    }
  }

  /**
   * Gets the Sortable annotated meta-data from an @Sortable object, which is used to determine how to sort
   * the object's contents.
   * <p/>
   * @param sortableAnnotatedObject an Object annotated with the @Sortable annotation meta-data.
   * @return the @Sortable annotation meta-data on an @Sortable annotated object.
   * @throws NullPointerException if the @Sortable annotated object reference is null!
   * @throws SearchException if the object is not annotated with the @Sortable annotation meta-data.
   * @see java.lang.Class#getAnnotation(Class)
   * @see org.cp.elements.util.sort.annotation.Sortable
   */
  protected org.cp.elements.util.sort.annotation.Sortable getSortableMetaData(final Object sortableAnnotatedObject) {
    Assert.notNull(sortableAnnotatedObject, "The @Sortable annotated object cannot be null!");

    org.cp.elements.util.sort.annotation.Sortable sortableMetaData = sortableAnnotatedObject.getClass().getAnnotation(
      org.cp.elements.util.sort.annotation.Sortable.class);

    Assert.notNull(sortableMetaData, new SortException(String.format(
      "To sort an object of type (%1$s), the class must be annotated with the (%2$s) annotation!",
        sortableAnnotatedObject.getClass().getName(), org.cp.elements.util.sort.annotation.Sortable.class.getName())));

    return sortableMetaData;
  }

  /**
   * Configures the Comparator to use during the sort operation by the calling Thread.
   * <p/>
   * @param <T> the Class type of the elements in the list.
   * @param sortable the Sortable implementing object specifying the Comparator to use to order elements in the list
   * during the sort operation.
   * @return the Sortable implementing object to method chaining purposes.
   * @see #configureComparator(org.cp.elements.util.sort.annotation.Sortable)
   * @see #isCustomComparatorAllowed()
   * @see ComparatorHolder#set(java.util.Comparator)
   * @see org.cp.elements.util.sort.Sortable#getOrderBy()
   * @see java.util.Comparator
   */
  protected <T> Sortable<T> configureComparator(final Sortable<T> sortable) {
    if (isCustomComparatorAllowed()) {
      Comparator<?> comparator = sortable.getOrderBy();

      if (comparator != null) {
        ComparatorHolder.set(comparator);
      }
    }

    return sortable;
  }

  /**
   * Configures the Comparator to use during the sort operation by the calling Thread.
   * <p/>
   * @param sortableMetaData the @Sortable annotation specifying the Comparator to use to order elements in the list
   * during the sort operation.
   * @return the @Sortable annotation to method chaining purposes.
   * @throws SortException if an instance of the Comparator class type specified in the @Sortable annotation cannot be
   * constructed.
   * @see #configureComparator(Sortable)
   * @see #isCustomComparatorAllowed()
   * @see ComparatorHolder#set(java.util.Comparator)
   * @see org.cp.elements.util.sort.annotation.Sortable#orderBy()
   * @see java.util.Comparator
   */
  protected org.cp.elements.util.sort.annotation.Sortable configureComparator(final org.cp.elements.util.sort.annotation.Sortable sortableMetaData) {
    try {
      if (isCustomComparatorAllowed()) {
        Class<? extends Comparator> comparatorClass = sortableMetaData.orderBy();

        if (!Comparator.class.equals(comparatorClass)) {
          ComparatorHolder.set(comparatorClass.newInstance());
        }
      }

      return sortableMetaData;
    }
    catch (Exception e) {
      throw new SortException(String.format(
        "Error occurred creating an instance of Comparator class (%1$s) to be used by this Sorter (%2$s)!"
          + " The Comparator class (%1$s) must have a public no-arg constructor!",
            sortableMetaData.orderBy().getName(), this.getClass().getName()), e);
    }
  }

  /**
   * Gets the list of elements to sort from the specified @Sortable annotated object based on the specified @Sortable
   * annotation meta-data.
   * <p/>
   * @param <E> the Class type of the elements in the list.
   * @param obj the @Sortable annotated object containing the list of elements to sort.
   * @param sortableMetaData the @Sortable annotation meta-data indicating the list method return the collection
   * of elements to sort.
   * @return the list of elements to sort.
   * @see org.cp.elements.util.sort.annotation.Sortable#listMethod()
   * @see java.lang.Class#getMethod(String, Class[])
   * @see java.lang.reflect.Method#invoke(Object, Object...)
   * @see java.util.Collections#emptyList()
   */
  @SuppressWarnings("unchecked")
  protected <E> List<E> asList(final Object obj, final org.cp.elements.util.sort.annotation.Sortable sortableMetaData) {
    try {
      Method asList = obj.getClass().getMethod(sortableMetaData.listMethod());
      List<E> list = (List<E>) asList.invoke(obj);
      return ObjectUtils.defaultIfNull(list, Collections.<E>emptyList());
    }
    catch (Exception e) {
      throw new SortException(String.format(
        "Error occurred getting the list of elements to sort from the (%1$s) method on object of type (%2$s)!",
          sortableMetaData.listMethod(), obj.getClass().getName()), e);
    }
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
   * The ComparatorHolder class is a holder of a Comparable for the calling Thread during the sort operation.
   * <p/>
   * @see java.lang.ThreadLocal
   * @see java.util.Comparator
   */
  protected static class ComparatorHolder {

    private static final ThreadLocal<Comparator<?>> COMPARATOR_HOLDER = new ThreadLocal<>();

    public static Comparator<?> get() {
      return COMPARATOR_HOLDER.get();
    }

    public static boolean isSet() {
      return (get() != null);
    }

    public static void set(final Comparator<?> comparator) {
      COMPARATOR_HOLDER.set(comparator);
    }

    public static void unset() {
      COMPARATOR_HOLDER.remove();
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

    @SafeVarargs
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
