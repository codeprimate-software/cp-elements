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

/**
 * The ObjectFactoryReferenceHolder class is reference holder to an ObjectFactory instance.
 * <p/>
 * @author John J. Blum
 * @see java.util.concurrent.atomic.AtomicReference
 * @see org.cp.elements.lang.factory.ObjectFactory
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ObjectFactoryReferenceHolder {

  private static ObjectFactory objectFactoryReference;

  public static synchronized boolean hasReference() {
    return (objectFactoryReference != null);
  }

  public static synchronized ObjectFactory get() {
    Assert.state(objectFactoryReference != null, "The ObjectFactory reference was not properly initialized!");
    return objectFactoryReference;
  }

  public static synchronized void set(final ObjectFactory objectFactory) {
    Assert.state(objectFactoryReference == null, "The ObjectFactory reference is already set to ({0})!",
      objectFactoryReference);
    objectFactoryReference = objectFactory;
  }

  public static synchronized void compareAndSet(final ObjectFactory currentObjectFactory, final ObjectFactory objectFactory) {
    if (currentObjectFactory == objectFactoryReference) {
      objectFactoryReference = objectFactory;
    }
  }

  public static synchronized void clear() {
    objectFactoryReference = null;
  }

}
