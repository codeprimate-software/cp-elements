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

import static org.cp.elements.lang.ElementsExceptionsFactory.newSearchException;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ClassUtils;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;

/**
 * Abstract base class encapsulating functionality common to all {@link Searcher} implementations.
 *
 * @author John J. Blum
 * @see MatcherHolder
 * @see org.cp.elements.util.search.Matcher
 * @see org.cp.elements.util.search.Searchable
 * @see org.cp.elements.util.search.Searcher
 * @see org.cp.elements.util.search.annotation.Searchable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractSearcher implements Searcher {

  protected static final boolean DEFAULT_CUSTOM_MATCHER_ALLOWED = true;

  private boolean customMatcherAllowed = DEFAULT_CUSTOM_MATCHER_ALLOWED;

  @SuppressWarnings("rawtypes")
  private Matcher matcher;

  /**
   * Determines whether a custom Matcher class as defined by the @Searchable object's 'matcher' attribute is allowed to
   * override the Searcher's Matcher when searching for elements in the collection/searchable.
   *
   * @return a boolean value indicating whether a custom Matcher is allowed to override the Searcher's Matcher during
   * the search operation.
   * @see #setCustomMatcherAllowed(boolean)
   */
  public boolean isCustomMatcherAllowed() {
    return this.customMatcherAllowed;
  }

  /**
   * Sets whether a custom Matcher class as defined by the @Searchable object's 'matcher' attribute is allowed to
   * override the Searcher's Matcher when searching for elements in the collection/searchable.
   *
   * @param customMatcherAllowed a boolean value indicating whether a custom Matcher is allowed to override the
   * Searcher's Matcher during the search operation.
   * @see #isCustomMatcherAllowed()
   */
  public void setCustomMatcherAllowed(boolean customMatcherAllowed) {
    this.customMatcherAllowed = customMatcherAllowed;
  }

  /**
   * Gets the Matcher used to match and find the desired element or elements in the collection.
   *
   * @param <E> the Class type of elements in the collection.
   * @return the Matcher used to match and find the desired element or elements in the collection during
   * the search operation.
   * @throws IllegalStateException if the Matcher used by this Searcher was not configured.
   * @see #setMatcher(Matcher)
   * @see org.cp.elements.util.search.Matcher
   */
  @Override
  @SuppressWarnings("unchecked")
  public <E> Matcher<E> getMatcher() {

    Matcher<E> localMatcher = (Matcher<E>) MatcherHolder.get();

    Assert.state(localMatcher != null || this.matcher != null,
      "A reference to a Matcher used by this Searcher ({0}) for searching and matching elements"
        + " in the collection was not properly configured!",
        getClass().getName());

    return ObjectUtils.returnFirstNonNullValue(localMatcher, this.matcher);
  }

  /**
   * Sets the Matcher used to match and find the desired element or elements in the collection.
   *
   * @param matcher the Matcher used to match and find the desired element or elements in the collection during
   * the search operation.
   * @throws NullPointerException if the Matcher reference to be used by this Searcher is null.
   * @see #getMatcher()
   * @see org.cp.elements.util.search.Matcher
   */
  @SuppressWarnings("rawtypes")
  public void setMatcher(@NotNull Matcher matcher) {
    Assert.notNull(matcher, "The Matcher used to match elements in the collection during the search operation"
        + " by this Searcher ({0}) cannot be null!",
      getClass().getName());
    this.matcher = matcher;
  }

  /**
   * Searches the array of elements in order to find the element or elements matching the criteria defined
   * by the Matcher.
   *
   * @param <E> the Class type of elements in the array.
   * @param array the array of elements to search.
   * @return the element in the array matching the search criteria defined by the Matcher.
   * @see #search(java.util.Collection)
   * @see java.util.Arrays#asList(Object[])
   */
  @Override
  @SuppressWarnings({ "unchecked", "varargs" })
  public <E> E search(E... array) {
    return search(Arrays.asList(array));
  }

  /**
   * Searches the Searchable object in order to find the element or elements matching the criteria defined
   * by the Matcher.
   *
   * @param <E> the Class type of elements to search in the Searchable object.
   * @param searchable the Searchable object to search.
   * @return the element in the Searchable object matching the search criteria defined by the Matcher.
   * @see #configureMatcher(Searchable)
   * @see #search(java.util.Collection)
   * @see org.cp.elements.util.search.Searchable#asList()
   */
  @Override
  public <E> E search(Searchable<E> searchable) {

    try {
      return search(configureMatcher(searchable).asList());
    }
    finally {
      MatcherHolder.unset();
    }
  }

  /**
   * Searches the @Searchable annotated object in order to find the element or elements matching the criteria
   * defined by the Matcher.
   *
   * @param <E> the Class type of elements to search in the @Searchable annotated object.
   * @param searchableAnnotatedObject the @Searchable annotated object to search.
   * @return the element in the @Searchable annotated object matching the search criteria defined by the Matcher.
   * @see #getSearchableMetaData(Object)
   * @see #configureMatcher(org.cp.elements.util.search.annotation.Searchable)
   * @see #asList(Object, org.cp.elements.util.search.annotation.Searchable)
   * @see #search(java.util.Collection)
   * @see org.cp.elements.util.search.annotation.Searchable#listMethod()
   */
  @Override
  public <E> E search(Object searchableAnnotatedObject) {

    try {
      return search(this.asList(searchableAnnotatedObject, configureMatcher(getSearchableMetaData(
        searchableAnnotatedObject))));
    }
    finally {
      MatcherHolder.unset();
    }
  }

  /**
   * Searches an array of elements finding all elements in the array matching the criteria defined by the Matcher.
   *
   * @param <E> the Class type of elements in the array.
   * @param array the array of elements to search.
   * @return an Iterable object containing all elements in the array that match the criteria defined by the Matcher.
   * @throws NullPointerException if the array is null!
   * @see #searchForAll(java.util.Collection)
   * @see java.lang.Iterable
   * @see java.util.Arrays#asList(Object[])
   */
  @SuppressWarnings({ "unchecked", "varargs" })
  public <E> Iterable<E> searchForAll(E... array) {
    return searchForAll(Arrays.asList(array));
  }

  /**
   * Searches a collection of elements finding all elements in the collection matching the criteria
   * defined by the Matcher.
   *
   * @param <E> the Class type of elements in the collection.
   * @param collection the collection of elements to search.
   * @return an Iterable object containing all elements in the collection that match the criteria
   * defined by the Matcher.
   * @throws NullPointerException if the collection is null!
   * @see #getMatcher()
   * @see java.lang.Iterable
   * @see java.util.Collection
   */
  public <E> Iterable<E> searchForAll(Collection<E> collection) {

    Assert.notNull(collection, "The collection to search cannot be null!");

    List<E> results = new ArrayList<>(collection.size());

    for (E element : collection) {
      if (getMatcher().isMatch(element)) {
        results.add(element);
      }
    }

    return results;
  }

  /**
   * Searches the Searchable object finding all elements in the Searchable object matching the criteria
   * defined by the Matcher.
   *
   * @param <E> the Class type of elements to search in the Searchable object.
   * @param searchable the Searchable object to search.
   * @return an Iterable object containing all elements from the Searchable object that match the criteria
   * defined by the Matcher.
   * @throws NullPointerException if the Searchable object is null!
   * @see #configureMatcher(Searchable)
   * @see #searchForAll(java.util.Collection)
   * @see java.lang.Iterable
   * @see org.cp.elements.util.search.Searchable#asList()
   */
  public <E> Iterable<E> searchForAll(Searchable<E> searchable) {

    try {
      return searchForAll(configureMatcher(searchable).asList());
    }
    finally {
      MatcherHolder.unset();
    }
  }

  /**
   * Searches the @Searchable annotated object finding all elements in the object matching the criteria
   * defined by the Matcher.
   *
   * @param <E> the Class type of elements to search in the @Searchable annotated object.
   * @param searchableAnnotatedObject the @Searchable annotated object to search.
   * @return an Iterable object containing all elements from the @Searchable annotated object matching the criteria
   * defined by the Matcher.
   * @throws NullPointerException if the @Searchable annotated object is null!
   * @see #getSearchableMetaData(Object)
   * @see #configureMatcher(org.cp.elements.util.search.annotation.Searchable)
   * @see #asList(Object, org.cp.elements.util.search.annotation.Searchable)
   * @see #searchForAll(java.util.Collection)
   * @see java.lang.Iterable
   * @see org.cp.elements.util.search.annotation.Searchable#listMethod()
   */
  public <E> Iterable<E> searchForAll(Object searchableAnnotatedObject) {

    try {
      return searchForAll(this.asList(searchableAnnotatedObject, configureMatcher(getSearchableMetaData(
        searchableAnnotatedObject))));
    }
    finally {
      MatcherHolder.unset();
    }
  }

  /**
   * Gets the @Searchable annotation and meta-data from the specified object.
   *
   * @param obj the object who's Class type must be annotated with the @Searchable annotation.
   * @return the @Searchable annotation meta-data as specified on the object's Class type.
   * @throws NullPointerException if the object is null.
   * @throws SearchException if the object's Class type is not annotated with @Searchable annotation meta-data.
   * @see org.cp.elements.util.search.annotation.Searchable
   */
  protected org.cp.elements.util.search.annotation.Searchable getSearchableMetaData(@NotNull Object obj) {

    Assert.notNull(obj, "The object to search cannot be null!");

    org.cp.elements.util.search.annotation.Searchable searchableAnnotation = obj.getClass().getAnnotation(
      org.cp.elements.util.search.annotation.Searchable.class);

    Assert.notNull(searchableAnnotation, new SearchException(String.format(
      "To search an object of type (%1$s), the class must be annotated with the (%2$s) annotation!",
        obj.getClass().getName(), org.cp.elements.util.search.annotation.Searchable.class.getName())));

    return searchableAnnotation;
  }

  /**
   * Configures the desired Matcher used to match and find elements from the Searchable implementing object based on
   * the annotation meta-data, applying only to the calling Thread and only if a custom Matcher is allowed.  The Matcher
   * is local to calling Thread during the search operation and does not change the Matcher set on the Searcher.
   *
   * @param <T> the class type of the elements in the {@link Searchable} collection.
   * @param searchable the Searchable implementing object used to determine the desired Matcher used to match and find
   * elements during the search operation.
   * @return the Searchable implementing object evaluated, allowing this method call to be chained.
   * @see #isCustomMatcherAllowed()
   * @see MatcherHolder#set(Matcher)
   * @see org.cp.elements.util.search.Searchable#getMatcher()
   */
  protected <T> Searchable<T> configureMatcher(Searchable<T> searchable) {

    if (isCustomMatcherAllowed()) {

      Matcher<T> matcher = searchable.getMatcher();

      if (matcher != null) {
        MatcherHolder.set(matcher);
      }
    }

    return searchable;
  }

  /**
   * Configures the desired Matcher used to match and find elements from the @Searchable annotated object based on
   * the annotation meta-data, applying only to the calling Thread and only if a custom Matcher is allowed.  The Matcher
   * is local to calling Thread during the search operation and does not change the Matcher set on the Searcher.
   *
   * @param searchableAnnotation the @Searchable annotation meta-data used to determine the desired Matcher used to
   * match and find elements during the search operation.
   * @return the @Searchable annotation meta-data evaluated, allowing this method call to be chained.
   * @see #isCustomMatcherAllowed()
   * @see MatcherHolder#set(Matcher)
   * @see org.cp.elements.util.search.annotation.Searchable#matcher()
   */
  @SuppressWarnings("rawtypes")
  protected org.cp.elements.util.search.annotation.Searchable configureMatcher(
      org.cp.elements.util.search.annotation.Searchable searchableAnnotation) {

    try {
      if (isCustomMatcherAllowed()) {

        Class<? extends Matcher> matcherClass = searchableAnnotation.matcher();

        if (!Matcher.class.equals(matcherClass)) {
          MatcherHolder.set(matcherClass.newInstance());
        }
      }

      return searchableAnnotation;
    }
    catch (Exception cause) {
      throw newSearchException(cause,
        "Error occurred creating an instance of Matcher class (%1$s) to be used by this Searcher (%2$s)!"
          + " The Matcher class (%1$s) must have a public no-arg constructor!",
            searchableAnnotation.matcher().getName(), this.getClass().getName());
    }
  }

  /**
   * Converts the given object into a searchable list of elements as specified by the @Searchable annotation meta-data
   * 'listMethod' attribute value.
   *
   * @param <E> the Class type of the elements in the list.
   * @param obj the object to convert into a searchable list of elements.
   * @param searchableAnnotation the @Searchable annotation meta-data specifying the method on the object with the
   * 'listMethod' attribute that converts the object into a searchable list of elements.
   * @return a searchable list of elements from the given object.
   * @see java.lang.reflect.Method#invoke(Object, Object...)
   * @see org.cp.elements.lang.ClassUtils#getMethod(Class, String, Class[])
   * @see org.cp.elements.util.search.annotation.Searchable
   */
  @SuppressWarnings("unchecked")
  protected <E> List<E> asList(Object obj, org.cp.elements.util.search.annotation.Searchable searchableAnnotation) {

    try {
      Method listMethod = ClassUtils.getMethod(obj.getClass(), searchableAnnotation.listMethod());
      List<E> collection = (List<E>) listMethod.invoke(obj);
      return ObjectUtils.returnFirstNonNullValue(collection, Collections.emptyList());
    }
    catch (Exception cause) {
      throw newSearchException(cause,
        "Error occurred getting the list of elements to search from the (%1$s) method on object of type (%2$s)!",
          searchableAnnotation.listMethod(), obj.getClass().getName());
    }
  }

  /**
   * The MatcherHolder class is a holder of a Matcher for the calling Thread during the search operation.
   *
   * @see java.lang.ThreadLocal
   * @see org.cp.elements.util.search.Matcher
   */
  protected static class MatcherHolder {

    private static final ThreadLocal<Matcher<?>> MATCHER_HOLDER = new ThreadLocal<>();

    public static Matcher<?> get() {
      return MATCHER_HOLDER.get();
    }

    public static boolean isSet() {
      return (get() != null);
    }

    public static void set(final Matcher<?> matcher) {
      MATCHER_HOLDER.set(matcher);
    }

    public static void unset() {
      MATCHER_HOLDER.remove();
    }
  }
}
