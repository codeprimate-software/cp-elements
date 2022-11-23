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
package org.cp.elements.lang.factory;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.lang.concurrent.ThreadSafe;

/**
 * Reference holder to an {@link ObjectFactory} instance.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.concurrent.ThreadSafe
 * @see org.cp.elements.lang.factory.ObjectFactory
 * @since 1.0.0
 */
@ThreadSafe
@SuppressWarnings("unused")
public final class ObjectFactoryReferenceHolder {

  private static ObjectFactory objectFactoryReference;

  /**
   * Default, private constructor to prevent instantiation of the {@link ObjectFactoryReferenceHolder}.
   */
  private ObjectFactoryReferenceHolder() { }

  /**
   * Determines whether {@literal this} reference holder holds a reference to an {@link ObjectFactory}.
   *
   * @return a boolean value indicating whether {@literal this} reference holder holds a reference to
   * an {@link ObjectFactory}.
   */
  @NullSafe
  public static synchronized boolean hasReference() {
    return objectFactoryReference != null;
  }

  /**
   * Gets the reference to the current {@link ObjectFactory}.
   *
   * @return a reference to the current {@link ObjectFactory}.
   * @throws IllegalStateException if the {@link ObjectFactory} reference is not set.
   * @see org.cp.elements.lang.factory.ObjectFactory
   * @see #set(ObjectFactory)
   */
  public static synchronized @NotNull ObjectFactory get() {

    Assert.state(objectFactoryReference != null,
      "An ObjectFactory was not properly initialized");

    return objectFactoryReference;
  }

  /**
   * Sets the reference to an {@link ObjectFactory}.
   *
   * @param objectFactory reference to an {@link ObjectFactory} to be held by {@literal this} reference holder.
   * @throws IllegalStateException if {@literal this} reference holder is already holding a reference to
   * an {@link ObjectFactory}.
   * @see #compareAndSet(ObjectFactory, ObjectFactory)
   * @see #get()
   */
  public static synchronized void set(@Nullable ObjectFactory objectFactory) {

    Assert.state(objectFactoryReference == null,
      "An ObjectFactory reference is already set to [%s]", objectFactoryReference);

    objectFactoryReference = objectFactory;
  }

  /**
   * {@literal CAS operation} allowing the reference to the {@link ObjectFactory} to be changed providing the current
   * {@link Thread} knows, or has a reference to, the current {@link ObjectFactory}.
   *
   * @param currentObjectFactory reference to the {@link ObjectFactory} currently held by
   * {@literal this} reference holder.
   * @param newObjectFactory new {@link ObjectFactory} to set as the reference held by {@literal this} reference holder.
   * @see #set(ObjectFactory)
   */
  public static synchronized void compareAndSet(@Nullable ObjectFactory currentObjectFactory,
      @Nullable ObjectFactory newObjectFactory) {

    if (currentObjectFactory == objectFactoryReference) {
      objectFactoryReference = newObjectFactory;
    }
  }

  /**
   * Clears the reference to the current {@link ObjectFactory}.
   */
  public static synchronized void clear() {
    objectFactoryReference = null;
  }
}
