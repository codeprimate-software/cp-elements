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

package org.cp.elements.lang;

import java.util.Map;

/**
 * The Initializer class is a utility class for performing Object initializations.
 * 
 * @author John J. Blum
 * @see org.cp.elements.lang.Initable
 * @see org.cp.elements.lang.ParameterizedInitable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class Initializer {

  /**
   * Initializes an Object by calling it's init method if the Object is an instance of the Initable interface.
   * 
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
   * 
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
   * 
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
