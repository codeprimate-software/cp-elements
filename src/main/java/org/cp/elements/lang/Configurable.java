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

package org.cp.elements.lang;

/**
 * The Configurable interface defines a contract for configuring an object who's class implements this interface.
 * This interface allows implementers to define parameters under which their objects will operate.  Configuration
 * meta-data can be specific to environment, runtime or other contextual information.
 * <p/>
 * @author John J. Blum
 * @param <T> the type of configuration meta-data and settings.
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Configurable<T> {

  /**
   * Gets the configuration meta-data of type T used to configure this object.
   * <p/>
   * @return the configuration meta-data used to configure this object.
   */
  public T getConfiguration();

  /**
   * Determines whether this object has been properly configured.  An object is configured when it's configure method
   * is invoked with configuration parameter.
   * <p/>
   * @return a boolean value indicating whether this object has been properly configured.
   */
  public boolean isConfigured();

  /**
   * Configures this object with the given configuration meta-data of type T.
   * <p/>
   * @param configuration the configuration meta-data of type T used to configure this object.
   */
  public void configure(T configuration);

}
