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

package org.cp.elements.lang.factory;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.concurrent.ThreadSafe;

/**
 * The ObjectFactoryReferenceHolder class is reference holder to an ObjectFactory instance.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.lang.concurrent.ThreadSafe
 * @see org.cp.elements.lang.factory.ObjectFactory
 * @since 1.0.0
 */
@SuppressWarnings("unused")
@ThreadSafe
public final class ObjectFactoryReferenceHolder {

  private static ObjectFactory objectFactoryReference;

  /**
   * Determines whether this reference holder holds a reference to an ObjectFactory.
   * <p/>
   * @return a boolean value indicating whether this reference holder holds an ObjectFactory reference.
   */
  public static synchronized boolean hasReference() {
    return (objectFactoryReference != null);
  }

  /**
   * Gets the reference to the current ObjectFactory in use by the application.
   * <p/>
   * @return a reference to the current ObjectFactory.
   * @throws IllegalStateException if the ObjectFactory reference is not set.
   */
  public static synchronized ObjectFactory get() {
    Assert.state(objectFactoryReference != null, "The ObjectFactory reference was not properly initialized!");
    return objectFactoryReference;
  }

  /**
   * Sets a reference to the ObjectFactory used by the application in this holder.
   * <p/>
   * @param objectFactory the ObjectFactory reference to hold in this reference holder.
   * @throws IllegalStateException if the reference holder already holds a reference to an ObjectFactory.
   * @see #compareAndSet(ObjectFactory, ObjectFactory)
   */
  public static synchronized void set(final ObjectFactory objectFactory) {
    Assert.state(objectFactoryReference == null, "The ObjectFactory reference is already set to ({0})!",
      objectFactoryReference);
    objectFactoryReference = objectFactory;
  }

  /**
   * CAS operation allowing the ObjectFactory reference to be changed providing the Thread knows, or has a reference
   * to the current ObjectFactory reference.
   * <p/>
   * @param currentObjectFactory a reference to the current ObjectFactory held by this reference holder.
   * @param objectFactory the new ObjectFactory reference to set on this reference holder providing the comparison
   * succeeds.
   * @see #get()
   * @see #set(ObjectFactory)
   */
  public static synchronized void compareAndSet(final ObjectFactory currentObjectFactory, final ObjectFactory objectFactory) {
    if (currentObjectFactory == objectFactoryReference) {
      objectFactoryReference = objectFactory;
    }
  }

  /**
   * Clears this reference holders ObjectFactory reference.
   */
  public static synchronized void clear() {
    objectFactoryReference = null;
  }

}
