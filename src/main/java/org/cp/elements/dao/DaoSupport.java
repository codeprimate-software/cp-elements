/*
 * Copyright 2016 Author or Authors.
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

/**
 * The Data Access Object Support interface defines the basic contract and functionality for the data CRUD (Create,
 * Retrieve, Update, Delete) operations.
 *
 * @author John J. Blum
 * @param <T> the class type of the Bean being persisted.
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface DaoSupport<T> {

  /**
   * Loads the specified Bean from the data store.
   *
   * @param bean the Bean object specifying the ID of the entity to load from the data store.
   * @return the Bean object with information loaded from the data store and corresponding Bean properties set.
   * @see org.cp.elements.lang.Identifiable#getId()
   */
  T load(T bean);

  /**
   * Removes the specified Bean from the data store.
   *
   * @param bean the Bean object specifying the ID of the entity to remove from the data store.
   * @return a boolean value indicating whether a Bean with ID was successfully removed from the data store.
   * @see org.cp.elements.lang.Identifiable#getId()
   */
  boolean remove(T bean);

  /**
   * Saves the specified Bean to the data store.  If the Bean is new (usually indicated by the absence of an ID),
   * then the Bean is inserted in the data store, otherwise the data store is updated with the Bean's current state.
   *
   * @param bean the Bean object who's stated is persisted (inserted/updated) to the data store.
   * @return the Bean object in a persisted, saved state.  This is also transaction indicating the Bean is no longer
   * in a modified state.
   * @see org.cp.elements.lang.Auditable#isModified()
   * @see org.cp.elements.lang.Identifiable#isNew()
   */
  T save(T bean);

}
