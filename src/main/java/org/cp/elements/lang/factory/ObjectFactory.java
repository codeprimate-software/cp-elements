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

  /**
   * Creates an object given the fully qualified class name, initialized with the specified constructor arguments.
   * The parameter types of the constructor used to construct the object are determined from the arguments.
   * <p/>
   * @param <T> the Class type of the created object.
   * @param objectTypeName a String indicating the fully qualified class name for the type of object to create.
   * @param args an array of Objects used as constructor arguments to initialize the object.
   * @return a newly created object of the given class type initialized with the specified arguments.
   * @see #create(String, Class[], Object...)
   */
  public <T> T create(String objectTypeName, Object... args);

  /**
   * Creates an object given the fully qualified class name, initialized with the specified constructor arguments
   * corresponding to the parameter types that specifies the exact signature of the constructor used to construct
   * the object.
   * <p/>
   * @param <T> the Class type of the created object.
   * @param objectTypeName a String indicating the fully qualified class name for the type of object to create.
   * @param parameterTypes an array of Class types indicating the signature of the constructor used to create
   * the object.
   * @param args an array of Objects used as constructor arguments to initialize the object.
   * @return a newly created object of the given class type initialized with the specified arguments.
   * @see #create(String, Object...)
   * @see java.lang.Class
   */
  public <T> T create(String objectTypeName, Class[] parameterTypes, Object... args);

  /**
   * Creates an object given the class type, initialized with the specified constructor arguments. The parameter types
   * of the constructor used to construct the object are determined from the arguments.
   * <p/>
   * @param <T> the Class type of the created object.
   * @param objectType the Class type from which the instance is created.
   * @param args an array of Objects used as constructor arguments to initialize the object.
   * @return a newly created object of the given class type initialized with the specified arguments.
   * @see #create(Class, Class[], Object...)
   * @see java.lang.Class
   */
  public <T> T create(Class<T> objectType, Object... args);

  /**
   * Creates an object given the fully qualified class name, initialized with the specified constructor arguments
   * corresponding to the parameter types that specifies the exact signature of the constructor used to construct
   * the object.
   * <p/>
   * @param <T> the Class type of the created object.
   * @param objectType the Class type from which the instance is created.
   * @param parameterTypes an array of Class types indicating the signature of the constructor used to create
   * the object.
   * @param args an array of Objects used as constructor arguments to initialize the object.
   * @return a newly created object of the given class type initialized with the specified arguments.
   * @see #create(Class, Object...)
   * @see java.lang.Class
   */
  public <T> T create(Class<T> objectType, Class[] parameterTypes, Object... args);

}
