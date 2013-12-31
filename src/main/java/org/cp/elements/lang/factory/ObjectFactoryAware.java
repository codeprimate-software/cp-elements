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

/**
 * The ObjectFactoryAware interface is a contract for implementing objects that require an instance of an ObjectFactory.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.lang.factory.ObjectFactory
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface ObjectFactoryAware {

  /**
   * Sets a reference to an ObjectFactory.
   * <p/>
   * @param objectFactory the reference to the ObjectFactory.
   */
  public void setObjectFactory(ObjectFactory objectFactory);

}
