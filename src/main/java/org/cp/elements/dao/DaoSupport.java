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

package org.cp.elements.dao;

/**
 * The Data Access Object Support interface defines the basic contract and functionality for the data CRUD (Create,
 * Retrieve, Update, Delete) operations.
 * <p/>
 * @author John J. Blum
 * @param <T> the class type of the Bean being persisted.
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface DaoSupport<T> {

  /**
   * Loads the specified Bean from the data store.
   * <p/>
   * @param bean the Bean object specifying the ID of the entity to load from the data store.
   * @return the Bean object with information loaded from the data store and corresponding Bean properties set.
   * @see org.cp.elements.lang.Identifiable#getId()
   */
  T load(T bean);

  /**
   * Removes the specified Bean from the data store.
   * <p/>
   * @param bean the Bean object specifying the ID of the entity to remove from the data store.
   * @return a boolean value indicating whether a Bean with ID was successfully removed from the data store.
   * @see org.cp.elements.lang.Identifiable#getId()
   */
  boolean remove(T bean);

  /**
   * Saves the specified Bean to the data store.  If the Bean is new (usually indicated by the absence of an ID),
   * then the Bean is inserted in the data store, otherwise the data store is updated with the Bean's current state.
   * <p/>
   * @param bean the Bean object who's stated is persisted (inserted/updated) to the data store.
   * @return the Bean object in a persisted, saved state.  This is also transaction indicating the Bean is no longer
   * in a modified state.
   * @see org.cp.elements.lang.Auditable#isModified()
   * @see org.cp.elements.lang.Identifiable#isNew()
   */
  T save(T bean);

}
