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

package org.cp.elements.util.search;

import java.util.Collection;

/**
 * The Searcher interface defines a contract for implementing classes responsible for searching a collection of objects
 * in order to find a specified match or a collection of matches.
 *
 * @author John J. Blum
 * @see java.util.Collection
 * @see org.cp.elements.util.search.AbstractSearcher
 * @see org.cp.elements.util.search.Matcher
 * @see org.cp.elements.util.search.Searchable
 * @see org.cp.elements.util.search.SearcherFactory
 * @see org.cp.elements.util.search.SearchType
 * @see org.cp.elements.util.search.annotation.Searchable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Searcher {

  /**
   * Gets the Matcher used to match and find the desired element or elements in the collection.
   *
   * @param <E> the Class type of elements in the collection.
   * @return the Matcher used to match and find the desired element or elements in the collection during
   * the search operation.
   * @see org.cp.elements.util.search.Matcher
   */
  <E> Matcher<E> getMatcher();

  /**
   * Searches the array of elements in order to find the element or elements matching the criteria defined
   * by the Matcher.
   *
   * @param <E> the Class type of elements in the array.
   * @param array the array of elements to search.
   * @return the element in the array matching the search criteria defined by the Matcher.
   * @see #getMatcher()
   * @see #search(java.util.Collection)
   * @see #search(Searchable)
   * @see #search(Object)
   * @see #searchForAll(Object[])
   */
  @SuppressWarnings({ "unchecked", "varargs" })
  <E> E search(E... array);

  /**
   * Searches the Collection of elements in order to find the element or elements matching the criteria defined
   * by the Matcher.
   *
   * @param <E> the Class type of elements in the Collection.
   * @param collection the Collection of elements to search.
   * @return the element in the Collection matching the search criteria defined by the Matcher.
   * @see #getMatcher()
   * @see #search(Object[])
   * @see #search(Searchable)
   * @see #search(Object)
   * @see #searchForAll(java.util.Collection)
   * @see java.util.Collection
   */
  <E> E search(Collection<E> collection);

  /**
   * Searches the Searchable object in order to find the element or elements matching the criteria defined
   * by the Matcher.
   *
   * @param <E> the Class type of elements to search in the Searchable object.
   * @param searchable the Searchable object to search.
   * @return the element in the Searchable object matching the search criteria defined by the Matcher.
   * @see #getMatcher()
   * @see #search(Object[])
   * @see #search(java.util.Collection)
   * @see #search(Object)
   * @see #searchForAll(Searchable)
   * @see org.cp.elements.util.search.Searchable
   */
  <E> E search(Searchable<E> searchable);

  /**
   * Searches the @Searchable annotated object in order to find the element or elements matching the criteria defined
   * by the Matcher.
   *
   * @param <E> the Class type of elements to search in the @Searchable annotated object.
   * @param searchableAnnotatedObject the @Searchable annotated object to search.
   * @return the element in the @Searchable annotated object matching the search criteria defined by the Matcher.
   * @see #getMatcher()
   * @see #search(Object[])
   * @see #search(java.util.Collection)
   * @see #search(Searchable)
   * @see #searchForAll(Object)
   * @see org.cp.elements.util.search.annotation.Searchable
   */
  <E> E search(Object searchableAnnotatedObject);

  /**
   * Searches an array of elements finding all elements in the array matching the criteria defined by the Matcher.
   *
   * @param <E> the Class type of elements in the array.
   * @param array the array of elements to search.
   * @return an Iterable object containing all elements in the array that match the criteria defined by the Matcher.
   * @see #getMatcher()
   * @see #search(Object[])
   * @see #searchForAll(java.util.Collection)
   * @see #searchForAll(Searchable)
   * @see #searchForAll(Object)
   * @see java.lang.Iterable
   */
  @SuppressWarnings({ "unchecked", "varargs" })
  <E> Iterable<E> searchForAll(E... array);

  /**
   * Searches a collection of elements finding all elements in the collection matching the criteria defined
   * by the Matcher.
   *
   * @param <E> the Class type of elements in the collection.
   * @param collection the collection of elements to search.
   * @return an Iterable object containing all elements in the collection that match the criteria defined
   * by the Matcher.
   * @see #getMatcher()
   * @see #search(java.util.Collection)
   * @see #searchForAll(Object[])
   * @see #searchForAll(Searchable)
   * @see #searchForAll(Object)
   * @see java.lang.Iterable
   * @see java.util.Collection
   */
  <E> Iterable<E> searchForAll(Collection<E> collection);

  /**
   * Searches the Searchable object finding all elements in the Searchable object matching the criteria
   * defined by the Matcher.
   *
   * @param <E> the Class type of elements to search in the Searchable object.
   * @param searchable the Searchable object to search.
   * @return an Iterable object containing all elements from the Searchable object that match the criteria
   * defined by the Matcher.
   * @see #getMatcher()
   * @see #search(Searchable)
   * @see #searchForAll(Object[])
   * @see #searchForAll(java.util.Collection)
   * @see #searchForAll(Object)
   * @see java.lang.Iterable
   * @see org.cp.elements.util.search.Searchable
   */
  <E> Iterable<E> searchForAll(Searchable<E> searchable);

  /**
   * Searches the @Searchable annotated object finding all elements in the object matching the criteria
   * defined by the Matcher.
   *
   * @param <E> the Class type of elements to search in the @Searchable annotated object.
   * @param searchableAnnotatedObject the @Searchable annotated object to search.
   * @return an Iterable object containing all elements from the @Searchable annotated object matching the criteria
   * defined by the Matcher.
   * @throws NullPointerException if the @Searchable annotated object is null!
   * @see #search(Object)
   * @see #searchForAll(Object[])
   * @see #searchForAll(java.util.Collection)
   * @see #searchForAll(Searchable)
   * @see java.lang.Iterable
   * @see org.cp.elements.util.search.annotation.Searchable
   */
  <E> Iterable<E> searchForAll(Object searchableAnnotatedObject);

}
