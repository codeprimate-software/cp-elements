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

package org.cp.elements.lang.factory;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.concurrent.ThreadSafe;

/**
 * The ObjectFactoryReferenceHolder class is reference holder to an ObjectFactory instance.
 *
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
   *
   * @return a boolean value indicating whether this reference holder holds an ObjectFactory reference.
   */
  public static synchronized boolean hasReference() {
    return (objectFactoryReference != null);
  }

  /**
   * Gets the reference to the current ObjectFactory in use by the application.
   *
   * @return a reference to the current ObjectFactory.
   * @throws IllegalStateException if the ObjectFactory reference is not set.
   */
  public static synchronized ObjectFactory get() {
    Assert.state(objectFactoryReference != null, "The ObjectFactory reference was not properly initialized!");
    return objectFactoryReference;
  }

  /**
   * Sets a reference to the ObjectFactory used by the application in this holder.
   *
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
   *
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
