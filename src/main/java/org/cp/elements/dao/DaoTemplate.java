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
package org.cp.elements.dao;

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
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.util.CollectionUtils;

/**
 * The Data Access Object (DAO) Template interface defines a basic contract and functionality
 * for the CRUD ({@literal CREATE, READ, UPDATE, DELETE}) data access operations
 * as well as simple queries.
 *
 * @author John J. Blum
 * @param <T> {@link Class type} of {@link Object application domain model object} being persisted.
 * @see java.util.function.Function
 * @see java.util.function.Predicate
 * @see java.util.Comparator
 * @see java.util.List
 * @see java.util.Optional
 * @see java.util.Set
 * @see <a href="https://en.wikipedia.org/wiki/Template_method_pattern">Template Method Software Design Pattern</a>
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface DaoTemplate<T> {

  /**
   * Creates a new instance of a Bean of {@link Class type} {@link T}.
   *
   * @return a new Bean.
   */
  T create();

  /**
   * Creates a new instance of a Bean of {@link Class type} {@link T} initialized with
   * the given, required {@link Function}.
   * <p>
   * The new Bean is passed to the {@code callback} {@link Function} in order to perform any necessary initialization
   * or other customizations before the Bean is returned.
   *
   * @param callback {@link Function} used to initialize the Bean; must not be {@literal null}.
   * @return a new [initialized] Bean.
   * @throws IllegalArgumentException if {@link Function} is {@literal null}.
   * @see java.util.function.Function
   * @see #create()
   */
  default T create(@NotNull Function<T, T> callback) {

    Assert.notNull(callback, "Callback Function is required");

    return callback.apply(create());
  }

  /**
   * Finds all Beans persisted to the data store.
   * <p>
   * Returns an {@link List#isEmpty() empty List} by default.
   *
   * @return all Beans contained in the data store.
   * @see #findAll(Comparator)
   * @see java.util.List
   */
  default List<T> findAll() {
    return Collections.emptyList();
  }

  /**
   * Finds all Beans persisted to the data store ordered (sorted) by the given, required {@link Comparator}.
   *
   * @param orderBy {@link Comparator} used to order (sort) the Beans in the {@link List};
   * must not be {@literal null}.
   * @return all Beans contained to the data store ordered (sorted) by the {@link Comparator}.
   * @throws IllegalArgumentException if the {@link Comparator} used to order (sort) the results
   * is {@literal null}.
   * @see java.util.Comparator
   * @see java.util.List
   * @see #findAll()
   */
  default List<T> findAll(@NotNull Comparator<T> orderBy) {

    Assert.notNull(orderBy, "Comparator used to order (sort) the beans is required");

    List<T> beans = CollectionUtils.nullSafeList(findAll());

    beans.sort(orderBy);

    return beans;
  }

  /**
   * Finds all Beans matching the criteria defined by the given, required {@link Predicate}.
   * <p>
   * Returns a {@link Collections#emptyList()} by default.
   *
   * @param queryPredicate {@link Predicate} defining the criteria used to match Beans;
   * must not be {@literal null}.
   * @return a {@link List} of all Beans matching the criteria defined by the {@link Predicate}.
   * @throws IllegalArgumentException if the Query {@link Predicate} is {@literal null}.
   * @see java.util.function.Predicate
   * @see java.util.List
   * @see #findAll()
   */
  default List<T> findAll(@NotNull Predicate<T> queryPredicate) {

    Assert.notNull(queryPredicate, "Query Predicate is required");

    List<T> beans = findAll();

    return CollectionUtils.nullSafeList(beans).stream()
      .filter(queryPredicate)
      .collect(Collectors.toList());
  }

  /**
   * Finds all Beans matching the criteria defined by the given, required {@link Predicate}
   * then ordered by (sorted by) the given, required {@link Comparator}.
   *
   * @param queryPredicate {@link Predicate} defining the criteria used to match Beans; must not be {@literal null}.
   * @param orderBy {@link Comparator} used to order (sort) the Beans in the {@link List}; must not be {@literal null}.
   * @return a {@link List} of all Beans matching the criteria defined by the {@link Predicate}, then ordered (sorted)
   * by the {@link Comparator}.
   * @throws IllegalArgumentException if the Query {@link Predicate} used to filter, or the {@link Comparator}
   * used to order (sort) the results, is {@literal null}.
   * @see java.util.function.Predicate
   * @see java.util.Comparator
   * @see #findAll(Predicate)
   * @see java.util.List
   */
  default List<T> findAll(@NotNull Predicate<T> queryPredicate, @NotNull Comparator<T> orderBy) {

    Assert.notNull(queryPredicate, "Query Predicate is required");
    Assert.notNull(orderBy, "Comparator used to order (sort) the beans is required");

    List<T> beans = CollectionUtils.nullSafeList(findAll(queryPredicate));

    beans.sort(orderBy);

    return beans;
  }

  /**
   * Finds a single, {@link Optional} Bean matching the criteria defined by the given, required {@link Predicate}.
   * <p>
   * Returns {@link Optional#empty()} by default.
   *
   * @param queryPredicate {@link Predicate} defining the criteria used to match a single Bean.
   * @return a single, {@link Optional} Bean matching the criteria defined by the {@link Predicate}.
   * @see java.util.function.Predicate
   * @see java.util.Optional
   * @see #findAll(Predicate)
   */
  default Optional<T> findBy(@NotNull Predicate<T> queryPredicate) {
    return Optional.empty();
  }

  /**
   * Loads the specified Bean having the given {@link ID} from the data store.
   *
   * @param <ID> {@link Class type} of the Bean's identifier ({@literal ID}).
   * @param id Identifier identifying the Bean to load from the data store.
   * @return the Bean having the given {@link ID} loaded from the data store.
   * @see org.cp.elements.lang.Identifiable#getId()
   */
  <ID> T load(ID id);

  /**
   * Removes the given Bean from the data store.
   *
   * @param bean Bean object specifying the ID of the entity to remove from the data store.
   * @return a boolean value indicating whether a Bean with ID was successfully removed from the data store.
   * @see org.cp.elements.lang.Identifiable#getId()
   */
  boolean remove(T bean);

  /**
   * Removes all Beans from the data store matching the given, required {@link Predicate} filter.
   *
   * @param filter {@link Predicate} used to match the Beans to remove from the data store.
   * @return a boolean value indicating whether this {@code removeAll} operation mutated the data store.
   * @throws IllegalArgumentException if {@link Predicate} is {@literal null}.
   * @see java.util.function.Predicate
   * @see #findAll(Predicate)
   * @see #removeAll(Set)
   */
  default boolean removeAll(@NotNull Predicate<T> filter) {

    Assert.notNull(filter, "Predicate identifying beans to remove is required");

    return removeAll(CollectionUtils.asSet(CollectionUtils.nullSafeList(findAll(filter))));
  }

  /**
   * Removes all Beans contained in the given {@link Set} from the data store.
   *
   * @param beans {@link Set} of Beans to remove from the data store.
   * @return a boolean value indicating whether this {@code removeAll} operation mutated the data store.
   * @see #remove(Object)
   * @see java.util.Set
   */
  default boolean removeAll(Set<T> beans) {

    return CollectionUtils.nullSafeSet(beans).stream()
      .filter(Objects::nonNull)
      .map(this::remove)
      .reduce((booleanOne, booleanTwo) -> booleanOne || booleanTwo)
      .orElse(false);
  }

  /**
   * Saves the given Bean to the data store.
   * <p>
   * If the Bean is new (usually indicated by the absence of an ID), then the Bean is inserted in the data store,
   * otherwise the data store is updated with the Bean's current state.
   *
   * @param bean Bean object whose state is persisted to (inserted/updated, or stored in) the data store.
   * @return the Bean object in a persisted, saved state. The Bean will also transition to a non-new,
   * non-modified state.
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
  default @NotNull Set<T> saveAll(@NotNull Set<T> beans) {

    return CollectionUtils.nullSafeSet(beans).stream()
      .filter(Objects::nonNull)
      .map(this::save)
      .collect(Collectors.toSet());
  }
}
