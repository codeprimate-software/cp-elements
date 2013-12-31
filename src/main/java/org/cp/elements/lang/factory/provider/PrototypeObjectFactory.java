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

package org.cp.elements.lang.factory.provider;

import org.cp.elements.lang.Configurable;
import org.cp.elements.lang.Initable;
import org.cp.elements.lang.ParameterizedInitable;
import org.cp.elements.lang.factory.AbstractObjectFactory;
import org.cp.elements.lang.factory.ObjectFactoryReferenceHolder;

/**
 * The PrototypeObjectFactory class creates instance of JavaBean compliant objects.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.lang.factory.AbstractObjectFactory
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class PrototypeObjectFactory extends AbstractObjectFactory {

  public PrototypeObjectFactory() {
    ObjectFactoryReferenceHolder.compareAndSet(null, this);
  }

  @Override
  protected <T> T postConstruct(T object, final Object... args) {
    object = super.postConstruct(object, args);
    object = configure(object);
    object = initialize(object, args);

    return object;
  }

  @SuppressWarnings("unchecked")
  protected <T> T configure(final T object) {
    if (object instanceof Configurable && isConfigurationAvailable()) {
      ((Configurable) object).configure(getConfiguration());
    }

    return object;
  }

  protected <T> T initialize(final T object, final Object... args) {
    if (object instanceof ParameterizedInitable) {
      ((ParameterizedInitable) object).init(args);
    }
    else if (object instanceof Initable) {
      ((Initable) object).init();
    }

    return object;
  }

}
