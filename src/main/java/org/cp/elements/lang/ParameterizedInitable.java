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

import java.util.Map;

/**
 * The ParameterizedInitale interface extends Initable and defines a contract for implementing classes whose objects
 * can be initialized.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.lang.Initable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface ParameterizedInitable extends Initable {

  /**
   * Initializes this object using the provided arguments.
   * <p/>
   * @param args an array of Object arguments used to initialize this object.
   * @see org.cp.elements.lang.Initable#init()
   * @see #init(java.util.Map)
   */
  public void init(Object... args);

  /**
   * Initialized this object using the provide parameters, a mapping of key-value pairs.
   * <p/>
   * @param parameters a Map of initialization parameters to initialize this object.
   * @see org.cp.elements.lang.Initable#init()
   * @see #init(Object...)
   * @see java.util.Map
   */
  public void init(Map<?, ?> parameters);

}
