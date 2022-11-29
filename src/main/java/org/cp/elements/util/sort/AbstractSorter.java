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
package org.cp.elements.util.sort;

import static org.cp.elements.lang.ElementsExceptionsFactory.newSortException;

import java.lang.reflect.Method;
import java.util.AbstractList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.lang.support.SmartComparator;
import org.cp.elements.util.search.SearchException;

/**
 * Abstract base class encapsulating functionality common to all {@link Sorter} implementations.
 *
 * @author John J. Blum
 * @see org.cp.elements.util.sort.Sorter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractSorter implements Sorter {

  protected static final boolean DEFAULT_CUSTOM_COMPARATOR_ALLOWED = true;

  private boolean customComparatorAllowed = DEFAULT_CUSTOM_COMPARATOR_ALLOWED;

  @SuppressWarnings("rawtypes")
  private Comparator orderBy;

  /**
   * Determines whether a custom Comparator is allowed by this Sorter when sorting and ordering elements in the
   * collection to be sorted.
   *
   * @return a boolean value indicating whether a custom Comparator is allowed by this Sorter when sorting and ordering
   * elements in the collection being sorted.
   * @see #setCustomComparatorAllowed(boolean)
   */
  public boolean isCustomComparatorAllowed() {
    return this.customComparatorAllowed;
  }

  /**
   * Sets whether a custom Comparator is allowed by this Sorter when sorting and ordering elements in the collection
   * to be sorted.
   *
   * @param customComparatorAllowed a boolean value indicating whether a custom Comparator is allowed by this Sorter
   * when sorting and ordering elements in the collection being sorted.
   * @see #isCustomComparatorAllowed()
   */
  public void setCustomComparatorAllowed(boolean customComparatorAllowed) {
    this.customComparatorAllowed = customComparatorAllowed;
  }

  /**
   * Gets the Comparator used to order the elements in the collection.
   *
   * @param <E> the type of elements in the Collection to compare.
   * @return the Comparator used to order the collection elements.
   * @see java.util.Comparator
   */
  @Override
  @SuppressWarnings("unchecked")
  public <E> Comparator<E> getOrderBy() {
    return ObjectUtils.returnFirstNonNullValue(ComparatorHolder.get(), this.orderBy,
      SmartComparator.ComparableComparator.INSTANCE);
  }

  /**
   * Sets the Comparator used to order the elements in the collection.
   *
   * @param orderBy the Comparator to use for ordering the collection elements.
   * @see java.util.Comparator
   */
  @SuppressWarnings("rawtypes")
  public void setOrderBy(Comparator orderBy) {
    this.orderBy = orderBy;
  }

  /**
   * Sorts an array of elements as defined by the Comparator, or as determined by the elements in the array
   * if the elements are Comparable.
   *
   * @param <E> the type of elements in the array.
   * @param elements the array of elements to sort.
   * @return the array of elements sorted.
   * @see #sort(java.util.List)
   * @see java.util.Arrays#asList(Object[])
   * @see java.util.ArrayList
   */
  @Override
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public <E> E[] sort(E... elements) {
    sort(new SortableArrayList(elements));
    return elements;
  }

  /**
   * Sorts the List representation of the Sortable implementing object as defined by the 'orderBy' Comparator, or as
   * determined by elements in the Sortable collection if the elements are Comparable.
   *
   * @param <E> the Class type of elements in the Sortable.
   * @param sortable the Sortable implementing object containing the collection of elements to sort.
   * @return the Sortable implementing object sorted.
   * @see #configureComparator(Sortable)
   * @see #getOrderBy()
   * @see #sort(java.util.List)
   * @see org.cp.elements.util.sort.Sortable#asList()
   */
  @Override
  public <E> Sortable<E> sort(Sortable<E> sortable) {

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
   *
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
  public <T> T sort(T sortableAnnotatedObject) {

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
   *
   * @param sortableAnnotatedObject an Object annotated with the @Sortable annotation meta-data.
   * @return the @Sortable annotation meta-data on an @Sortable annotated object.
   * @throws NullPointerException if the @Sortable annotated object reference is null!
   * @throws SearchException if the object is not annotated with the @Sortable annotation meta-data.
   * @see java.lang.Class#getAnnotation(Class)
   * @see org.cp.elements.util.sort.annotation.Sortable
   */
  protected org.cp.elements.util.sort.annotation.Sortable getSortableMetaData(Object sortableAnnotatedObject) {

    Assert.notNull(sortableAnnotatedObject, "The @Sortable annotated object cannot be null!");

    org.cp.elements.util.sort.annotation.Sortable sortableMetaData = sortableAnnotatedObject.getClass()
      .getAnnotation(org.cp.elements.util.sort.annotation.Sortable.class);

    Assert.notNull(sortableMetaData, newSortException(
      "To sort an object of type (%1$s), the class must be annotated with the (%2$s) annotation!",
        sortableAnnotatedObject.getClass().getName(), org.cp.elements.util.sort.annotation.Sortable.class.getName()));

    return sortableMetaData;
  }

  /**
   * Configures the Comparator to use during the sort operation by the calling Thread.
   *
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
  protected <T> Sortable<T> configureComparator(Sortable<T> sortable) {

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
   *
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
  @SuppressWarnings("rawtypes")
  protected org.cp.elements.util.sort.annotation.Sortable configureComparator(
      org.cp.elements.util.sort.annotation.Sortable sortableMetaData) {

    try {
      if (isCustomComparatorAllowed()) {

        Class<? extends Comparator> comparatorClass = sortableMetaData.orderBy();

        if (!Comparator.class.equals(comparatorClass)) {
          ComparatorHolder.set(comparatorClass.newInstance());
        }
      }

      return sortableMetaData;
    }
    catch (Exception cause) {
      throw newSortException(cause,
        "Error occurred creating an instance of Comparator class (%1$s) to be used by this Sorter (%2$s)!"
          + " The Comparator class (%1$s) must have a public no-arg constructor!",
            sortableMetaData.orderBy().getName(), this.getClass().getName());
    }
  }

  /**
   * Gets the list of elements to sort from the specified @Sortable annotated object based on the specified @Sortable
   * annotation meta-data.
   *
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
  protected <E> List<E> asList(Object obj, org.cp.elements.util.sort.annotation.Sortable sortableMetaData) {

    try {
      Method asList = obj.getClass().getMethod(sortableMetaData.listMethod());
      List<E> list = (List<E>) asList.invoke(obj);
      return ObjectUtils.returnFirstNonNullValue(list, Collections.emptyList());
    }
    catch (Exception cause) {
      throw newSortException(cause,
        "Error occurred getting the list of elements to sort from the (%1$s) method on object of type (%2$s)!",
          sortableMetaData.listMethod(), obj.getClass().getName());
    }
  }

  /**
   * Swaps elements at 2 different positions (indexes) in the List of elements.
   *
   * @param <E> the Class type of the elements in the List.
   * @param elements the List of elements to perform an element swap on.
   * @param index1 the index of the first element to swap.
   * @param index2 the index of the second element to swap.
   * @see java.util.List
   */
  protected <E> void swap(List<E> elements, int index1, int index2) {

    E elementFromIndex1 = elements.get(index1);

    elements.set(index1, elements.get(index2));
    elements.set(index2, elementFromIndex1);
  }

  /**
   * A holder of a {@link Comparable object} used by the calling {@link Thread} during the sort operation.
   *
   * @see java.lang.ThreadLocal
   * @see java.util.Comparator
   */
  protected static class ComparatorHolder {

    private static final ThreadLocal<Comparator<?>> COMPARATOR_HOLDER = new ThreadLocal<>();

    public static @Nullable Comparator<?> get() {
      return COMPARATOR_HOLDER.get();
    }

    @NullSafe
    public static boolean isSet() {
      return get() != null;
    }

    public static void set(@Nullable Comparator<?> comparator) {
      COMPARATOR_HOLDER.set(comparator);
    }

    public static void unset() {
      COMPARATOR_HOLDER.remove();
    }
  }

  /**
   * {@link List} implementation wrapping a modifiable array of elements that can be sorted.
   *
   * @param <E> {@link Class type} of the elements in the array / {@link List}.
   * @see java.util.AbstractList
   */
  @SuppressWarnings("PMD.ArrayIsStoredDirectly")
  protected static class SortableArrayList<E> extends AbstractList<E> {

    private final E[] elements;

    @SafeVarargs
    public SortableArrayList(E... elements) {
      this.elements = ObjectUtils.requireObject(elements,
        "The array of elements to wrap in a List is required");
    }

    @Override
    public E get(int index) {
      return this.elements[index];
    }

    @Override
    public E set(int index, E element) {
      E previousElement = this.elements[index];
      this.elements[index] = element;
      return previousElement;
    }

    @Override
    public int size() {
      return this.elements.length;
    }
  }
}
