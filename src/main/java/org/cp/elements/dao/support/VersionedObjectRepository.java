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

import java.util.Optional;

import org.cp.elements.lang.Integers;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.Versioned;
import org.cp.elements.lang.annotation.NotNull;

/**
 * Data Access Object (DAO) and Repository interface defining a single query method used to determine
 * if a {@link Versioned POJO} is present in the data store with the given {@literal version}.
 * <p>
 * {@literal Versioning} can be used for {@literal Optimistic Concurrency Control}, or {@literal Optimistic Locking}
 * in place of {@link org.cp.elements.lang.Auditable audited metadata}, to determine if a record for a persistent entity
 * has been change concurrently in the backend data store by another program (application process).
 * <p>
 * A typical {@literal SQL} query implementation used to determine existence or presence of a persistent entity,
 * or {@link Versioned object} with the given {@literal version} corresponds to a record in the database, would be:
 *
 * <pre>
 * <code>
 *   SELECT [DISTINCT] 1 FROM tableName WHERE version = ?
 * </code>
 * </pre>
 *
 * The use of the {@literal DISTINCT} keyword in the {@literal SQL} query is optional and unnecessary when
 * the {@literal version} is unique, which should be the case for {@literal Optimistic Concurrency Control}.
 * <p>
 * Subsequently, an {@literal Optimistic Lock} is typically enforced using the following {@literal SQL UPDATE}
 * statement:
 *
 * <pre>
 * <code>
 *   UPDATE tableName SET fields WHERE version = currentVersion
 * </code>
 * </pre>
 *
 * The {@literal currentVersion} in this case would be the {@literal version} sourced from the {@link Versioned object}
 * that the caller is currently processing.
 *
 * @author John Blum
 * @param <VERSION> {@link Class type} of the {@link Versioned POJO's} version property.
 * @see java.lang.FunctionalInterface
 * @see org.cp.elements.lang.Versioned
 * @see <a href="https://en.wikipedia.org/wiki/Optimistic_concurrency_control">Optimistic Concurrency Control (aka Optimistic Locking)</a>
 * @since 1.0.0
 */
@FunctionalInterface
@SuppressWarnings("unused")
public interface VersionedObjectRepository<VERSION> {

  /**
   * Query method used to query the backend data store and determine whether a record for a persistent entity
   * with the given {@literal version} exists, or is present.
   * <p>
   * The {@link Integer value} may indicate a count of the number of records in the backend data store that have the
   * given {@literal version}. However, when the {@literal version} is used for {@literal optimistic locking}, then the
   * returned {@link Integer value} will be equal to {@literal 1} when a record with the given {@literal version} exists
   * in the backend data store and {@literal null} will be returned when no record with the given {@literal version}
   * exists in the backend data store, likely because the record was changed and the {@literal version} was updated.
   *
   * @param version {@link VERSION version} to query for existence.
   * @return an {@link Optional} {@link Integer value} indicating whether a record for a persistent entity
   * with the given {@literal version} exists, or is present in the backend data store.
   * @see java.util.Optional
   * @see java.lang.Integer
   */
  Optional<Integer> existsByVersion(VERSION version);

  /**
   * Query method used to query the backend data store and determine whether a record for the given
   * {@link Versioned persistent entity} with the given {@literal version} exists, or is present.
   *
   * @param object {@link Versioned persistent entity} to query; must not be {@literal null}.
   * @return a boolean value indicating whether a record for the given {@link Versioned persistent entity}
   * with the given {@literal version} exists, or is present in the backend data store.
   * @throws IllegalArgumentException if the {@link Versioned object} is {@literal null}.
   * @see org.cp.elements.lang.Versioned
   * @see #existsByVersion(Object)
   */
  default boolean existsByVersion(@NotNull Versioned<VERSION> object) {

    return existsByVersion(ObjectUtils.requireObject(object, "Versioned object is required").getVersion())
      .filter(Integers::isGreaterThanZero)
      .isPresent();
  }
}
