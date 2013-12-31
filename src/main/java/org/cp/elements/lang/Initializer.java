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
 * The Initializer class is a utility class for performing Object initializations.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.lang.Initable
 * @see org.cp.elements.lang.ParameterizedInitable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class Initializer {

  /**
   * Initializes an Object by calling it's init method if the Object is an instance of the Initable interface.
   * <p/>
   * @param initableObj the Object to be initialized.
   * @return a boolean value indicating whether the Object has been initialized.
   * @see org.cp.elements.lang.Initable#init()
   */
  public static boolean init(final Object initableObj) {
    if (initableObj instanceof Initable) {
      ((Initable) initableObj).init();
      return true;
    }

    return false;
  }

  /**
   * Initializes an Object by calling it's init method with the array of Object arguments if the Object is an instance
   * of the ParameterizedInitable interface.  Calls the init(:Object) method with no arguments/parameters if the Object
   * is not an instance of the ParameterizedInitable interface.
   * <p/>
   * @param initableObj the Object to be initialized.
   * @param args an array of Object arguments used to initialize the Object.
   * @return a boolean value indicating whether the Object has been initialized.
   * @see #init(Object)
   */
  public static boolean init(final Object initableObj, final Object... args) {
    if (initableObj instanceof ParameterizedInitable) {
      ((ParameterizedInitable) initableObj).init(args);
      return true;
    }

    return init(initableObj);
  }

  /**
   * Initializes an Object by calling it's init method with a Map of parameters if the Object is an instance of the
   * ParameterizedInitable interface.  Calls the init(:Object) method with no arguments/parameters if the Object is not
   * an instance of the ParameterizedInitable interface.
   * <p/>
   * @param initableObj the Object to be initialized.
   * @param parameters a Map of parameters used to initialize the Object.
   * @return a boolean value indicating whether the Object has been initialized.
   * @see java.util.Map
   * @see #init(Object)
   */
  public static boolean init(final Object initableObj, final Map<?, ?> parameters) {
    if (initableObj instanceof ParameterizedInitable) {
      ((ParameterizedInitable) initableObj).init(parameters);
      return true;
    }

    return init(initableObj);
  }

}
