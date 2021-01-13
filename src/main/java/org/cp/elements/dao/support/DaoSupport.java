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
package org.cp.elements.dao.support;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import org.cp.elements.lang.Assert;
import org.cp.elements.util.CollectionUtils;

/**
 * The Data Access Object (DAO) Support interface defines the basic contract and functionality
 * for the CRUD (CREATE, READ, UPDATE, DELETE) data access operations.
 *
 * @author John J. Blum
 * @param <T> {@link Class type} of the application domain model object being persisted.
 * @see java.util.function.Function
 * @see java.util.function.Predicate
 * @see java.util.Comparator
 * @see java.util.List
 * @see java.util.Optional
 * @see java.util.Set
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface DaoSupport<T> {

  /**
   * Creates a new instance of a Bean with a {@link Class type} of {@code T}.
   *
   * @return a new Bean.
   */
  T create();

  /**
   * Creates a new instance of a Bean with a {@link Class type} of {@code T}.
   *
   * The new Bean is passed to the {@code callback} {@link Function} in order to perform any necessary initialization.
   *
   * @param callback {@link Function} used to initialize the Bean; must not be {@literal null}.
   * @return a new [initialized] Bean.
   * @throws IllegalArgumentException if {@link Function} is {@literal null}.
   * @see java.util.function.Function
   * @see #create()
   */
  default T create(Function<T, T> callback) {

    Assert.notNull(callback, "Callback Function is required");

    return callback.apply(create());
  }

  /**
   * Finds all Beans persisted to the data store.
   *
   * Returns an {@link List#isEmpty() empty List} by default.
   *
   * @return all Beans in the data store.
   * @see #findAll(Comparator)
   * @see java.util.List
   */
  default List<T> findAll() {
    return Collections.emptyList();
  }

  /**
   * Finds all Beans persisted to the data store ordered (sorted) by the given {@link Comparator}.
   *
   * @param orderBy {@link Comparator} used to order (sort) the Beans in the {@link List}.
   * @return all Beans persisted to the data store.
   * @throws IllegalArgumentException if the {@link Comparator} used to order (sort) the results
   * is {@literal null}.
   * @see java.util.Comparator
   * @see java.util.List
   * @see #findAll()
   */
  default List<T> findAll(Comparator<T> orderBy) {

    Assert.notNull(orderBy, "Comparator used to order (sort) the beans is required");

    List<T> beans = CollectionUtils.nullSafeList(findAll());

    beans.sort(orderBy);

    return beans;
  }

  /**
   * Finds all Beans matching the criteria defined by the given {@link Predicate}.
   *
   * Returns a {@link Collections#emptyList()} by default.
   *
   * @param queryPredicate {@link Predicate} defining the criteria used to match Beans.
   * @return a {@link List} of all Beans matching the criteria defined by the {@link Predicate}.
   * @throws IllegalArgumentException if the Query {@link Predicate} is {@literal null}.
   * @see #findAll(Predicate, Comparator)
   * @see java.util.function.Predicate
   * @see java.util.List
   */
  default List<T> findAll(Predicate<T> queryPredicate) {

    Assert.notNull(queryPredicate, "Query Predicate is required");

    List<T> beans = findAll();

    return CollectionUtils.nullSafeList(beans).stream()
      .filter(queryPredicate)
      .collect(Collectors.toList());
  }

  /**
   * Finds all Beans matching the criteria defined by the given {@link Predicate}
   * ordered by (sorted by) the given {@link Comparator}.
   *
   * @param queryPredicate {@link Predicate} defining the criteria used to match Beans.
   * @param orderBy {@link Comparator} used to order (sort) the Beans in the {@link List}.
   * @return a {@link List} of all Beans matching the criteria defined by the {@link Predicate}.
   * @throws IllegalArgumentException if the Query {@link Predicate} or the {@link Comparator}
   * used to order (sort) the results is {@literal null}.
   * @see java.util.function.Predicate
   * @see java.util.Comparator
   * @see #findAll(Predicate)
   * @see java.util.List
   */
  default List<T> findAll(Predicate<T> queryPredicate, Comparator<T> orderBy) {

    Assert.notNull(queryPredicate, "Query Predicate is required");
    Assert.notNull(orderBy, "Comparator used to order (sort) the beans is required");

    List<T> beans = CollectionUtils.nullSafeList(findAll(queryPredicate));

    beans.sort(orderBy);

    return beans;
  }

  /**
   * Finds a single {@link Optional} Bean matching the criteria defined by the given {@link Predicate}.
   *
   * Returns {@link Optional#empty()} by default.
   *
   * @param queryPredicate {@link Predicate} defining the criteria used to match a single Bean.
   * @return a single {@link Optional} Bean matching the criteria defined by the {@link Predicate}.
   * @see java.util.function.Predicate
   * @see java.util.Optional
   * @see #findAll(Predicate)
   */
  default Optional<T> findBy(Predicate<T> queryPredicate) {
    return Optional.empty();
  }

  /**
   * Loads the specified Bean from the data store with the given {@code ID}.
   *
   * @param <ID> {@link Class type} of the Beans' Identifier (ID).
   * @param id Identifier identifying the Bean to load from the data store.
   * @return the Bean loaded from the data store.
   * @see org.cp.elements.lang.Identifiable#getId()
   */
  <ID> T load(ID id);

  /**
   * Removes the specified Bean from the data store.
   *
   * @param bean the Bean object specifying the ID of the entity to remove from the data store.
   * @return a boolean value indicating whether a Bean with ID was successfully removed from the data store.
   * @see org.cp.elements.lang.Identifiable#getId()
   */
  boolean remove(T bean);

  /**
   * Removes all Beans from the data store matching the given {@link Predicate} filter.
   *
   * @param filter {@link Predicate} used to match the Beans to remove from the data store.
   * @return a boolean value indicating whether this operation mutated the data store.
   * @throws IllegalArgumentException if the {@link Predicate} Filter is {@literal null}.
   * @see java.util.function.Predicate
   * @see #findAll(Predicate)
   * @see #removeAll(Set)
   */
  default boolean removeAll(Predicate<T> filter) {

    Assert.notNull(filter, "Predicate identifying beans to remove is required");

    return removeAll(CollectionUtils.asSet(CollectionUtils.nullSafeList(findAll(filter))));
  }

  /**
   * Removes all {@link Set Beans} from the data store.
   *
   * @param beans {@link Set} of Beans to remove from the data store.
   * @return a boolean value indicating whether this {@code removeAll} operation mutated the data store.
   * @see #remove(Object)
   * @see java.util.Set
   */
  default boolean removeAll(Set<T> beans) {

    Set<T> removedBeans = CollectionUtils.nullSafeSet(beans).stream()
      .filter(Objects::nonNull)
      .filter(this::remove)
      .collect(Collectors.toSet());

    return !removedBeans.isEmpty();
  }

  /**
   * Saves the specified Bean to the data store.
   *
   * If the Bean is new (usually indicated by the absence of an ID), then the Bean is inserted in the data store,
   * otherwise the data store is updated with the Bean's current state.
   *
   * @param bean the Bean object who's stated is persisted (inserted/updated) to the data store.
   * @return the Bean object in a persisted, saved state.  This is also transaction indicating the Bean is no longer
   * in a modified state.
   * @see org.cp.elements.lang.Auditable#isModified()
   * @see org.cp.elements.lang.Identifiable#isNew()
   */
  T save(T bean);

  /**
   * Saves all {@link Set Beans} to the data store.
   *
   * @param beans {@link Set} of Beans to save to the data store.
   * @return a new {@link Set} of Beans that were successfully saved to the data store.
   * @see java.util.Set
   * @see #save(Object)
   */
  default Set<T> saveAll(Set<T> beans) {

    return CollectionUtils.nullSafeSet(beans).stream()
      .filter(Objects::nonNull)
      .map(this::save)
      .collect(Collectors.toSet());
  }
}
