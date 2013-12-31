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

import org.cp.elements.context.configure.ConfigurationAware;
import org.cp.elements.util.convert.ConversionServiceAware;

/**
 * The ObjectFactory interface defines a contract for components that create instances of other objects.
 * <p/>
 * @author John J. Blum
 * @see java.lang.Object
 * @see org.cp.elements.context.configure.ConfigurationAware
 * @see org.cp.elements.lang.factory.AbstractObjectFactory
 * @see org.cp.elements.util.convert.ConversionServiceAware
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface ObjectFactory extends ConfigurationAware, ConversionServiceAware {

  public <T> T create(String objectTypeName, Object... args);

  public <T> T create(String objectTypeName, Class[] parameterTypes, Object... args);

  public <T> T create(Class<T> objectType, Object... args);

  public <T> T create(Class<T> objectType, Class[] parameterTypes, Object... args);

}
