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

package org.cp.elements.lang.factory;

import org.cp.elements.context.configure.ConfigurationAware;
import org.cp.elements.data.convert.ConversionServiceAware;

/**
 * The ObjectFactory interface defines a contract for components that create instances of other objects.
 *
 * @author John J. Blum
 * @see java.lang.Object
 * @see org.cp.elements.context.configure.ConfigurationAware
 * @see org.cp.elements.lang.factory.AbstractObjectFactory
 * @see org.cp.elements.data.convert.ConversionServiceAware
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface ObjectFactory extends ConfigurationAware, ConversionServiceAware {

  /**
   * Creates an object given the fully qualified class name, initialized with the specified constructor arguments.
   * The parameter types of the constructor used to construct the object are determined from the arguments.
   *
   * @param <T> the Class type of the created object.
   * @param objectTypeName a String indicating the fully qualified class name for the type of object to create.
   * @param args an array of Objects used as constructor arguments to initialize the object.
   * @return a newly created object of the given class type initialized with the specified arguments.
   * @see #create(String, Class[], Object...)
   */
  <T> T create(String objectTypeName, Object... args);

  /**
   * Creates an object given the fully qualified class name, initialized with the specified constructor arguments
   * corresponding to the parameter types that specifies the exact signature of the constructor used to construct
   * the object.
   *
   * @param <T> the Class type of the created object.
   * @param objectTypeName a String indicating the fully qualified class name for the type of object to create.
   * @param parameterTypes an array of Class types indicating the signature of the constructor used to create
   * the object.
   * @param args an array of Objects used as constructor arguments to initialize the object.
   * @return a newly created object of the given class type initialized with the specified arguments.
   * @see #create(String, Object...)
   * @see java.lang.Class
   */
  <T> T create(String objectTypeName, Class[] parameterTypes, Object... args);

  /**
   * Creates an object given the class type, initialized with the specified constructor arguments. The parameter types
   * of the constructor used to construct the object are determined from the arguments.
   *
   * @param <T> the Class type of the created object.
   * @param objectType the Class type from which the instance is created.
   * @param args an array of Objects used as constructor arguments to initialize the object.
   * @return a newly created object of the given class type initialized with the specified arguments.
   * @see #create(Class, Class[], Object...)
   * @see java.lang.Class
   */
  <T> T create(Class<T> objectType, Object... args);

  /**
   * Creates an object given the fully qualified class name, initialized with the specified constructor arguments
   * corresponding to the parameter types that specifies the exact signature of the constructor used to construct
   * the object.
   *
   * @param <T> the Class type of the created object.
   * @param objectType the Class type from which the instance is created.
   * @param parameterTypes an array of Class types indicating the signature of the constructor used to create
   * the object.
   * @param args an array of Objects used as constructor arguments to initialize the object.
   * @return a newly created object of the given class type initialized with the specified arguments.
   * @see #create(Class, Object...)
   * @see java.lang.Class
   */
  <T> T create(Class<T> objectType, Class[] parameterTypes, Object... args);

}
