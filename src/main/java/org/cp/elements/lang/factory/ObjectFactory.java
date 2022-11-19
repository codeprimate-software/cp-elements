/*
 * Copyright 2011-Present Author or Authors.
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
import org.cp.elements.data.conversion.ConversionServiceAware;
import org.cp.elements.lang.ClassUtils;
import org.cp.elements.lang.reflect.ReflectionUtils;

/**
 * Interface that defines a contract for components capable of creating instances of other {@link Object Objects}.
 *
 * @author John J. Blum
 * @see java.lang.Object
 * @see org.cp.elements.context.configure.ConfigurationAware
 * @see org.cp.elements.data.conversion.ConversionServiceAware
 * @see org.cp.elements.lang.factory.AbstractObjectFactory
 * @see <a href="https://en.wikipedia.org/wiki/Abstract_factory_pattern">Abstract Factory Software Design Pattern</a>
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface ObjectFactory extends ConfigurationAware, ConversionServiceAware {

  /**
   * Creates an {@link Object} from the given {@link String fully-qualified class name}, initialized with
   * the given array of constructor {@link Object arguments}.
   *
   * The array {@link Class parameter types} in the constructor used to construct the {@link Object}
   * are determined from the array of {@link Object arguments}.
   *
   * @param <T> {@link Class type} of {@link Object} to create.
   * @param objectTypeName {@link String} containing the {@literal fully-qualified class name}
   * for the {@link Class type} of {@link Object} to create.
   * @param args array of {@link Object arguments} passed to the constructor used to initialize the {@link Object}.
   * @return a new {@link Object} of the given {@link Class type} initialized with
   * the given array of {@link Object arguments}.
   * @see #create(Class, Class[], Object...)
   */
  default <T> T create(String objectTypeName, Object... args) {
    return create(ClassUtils.loadClass(objectTypeName), ReflectionUtils.getArgumentTypes(args), args);
  }

  /**
   * Creates an {@link Object} from the given {@link String fully-qualified class name}, initialized with
   * the given array of constructor {@link Object arguments} corresponding to the given array of constructor
   * {@link Class parameter types}, which specify the exact signature of the constructor used to construct
   * the {@link Object}.
   *
   * @param <T> {@link Class type} of {@link Object} to create.
   * @param objectTypeName {@link String} containing the {@literal fully-qualified class name}
   * for the {@link Class type} of {@link Object} to create.
   * @param parameterTypes array of {@link Class types} specifying the signature of the constructor
   * used to construct the {@link Object}.
   * @param args array of {@link Object arguments} passed to the constructor used to initialize the {@link Object}.
   * @return a new {@link Object} of the given {@link Class type} initialized with
   * the given array of {@link Object arguments}.
   * @see #create(Class, Class[], Object...)
   */
  default <T> T create(String objectTypeName, Class<?>[] parameterTypes, Object... args) {
    return create(ClassUtils.loadClass(objectTypeName), parameterTypes, args);
  }

  /**
   * Creates an {@link Object} from the given {@link Class type} initialized with
   * the given array of constructor {@link Object arguments}.
   *
   * The array of {@link Class parameter types} of the constructor used to construct the {@link Object}
   * are determined from the array of {@link Object arguments}.
   *
   * @param <T> {@link Class type} of {@link Object} to create.
   * @param objectType {@link Class type} from which the {@link Object }instance will be created.
   * @param args array of {@link Object arguments} passed to the constructor used to initialize the {@link Object}.
   * @return a new {@link Object} of the given {@link Class type} initialized with
   * the given array of {@link Object arguments}.
   * @see #create(Class, Class[], Object...)
   * @see java.lang.Class
   */
  default <T> T create(Class<T> objectType, Object... args) {
    return create(objectType, ReflectionUtils.getArgumentTypes(args), args);
  }

  /**
   * Creates an {@link Object} from the given {@link Class type} initialized with
   * the given array of constructor {@link Object arguments} corresponding to the given array of constructor
   * {@link Class parameter types}, which specify the exact signature of the constructor used to construct
   * the {@link Object}.
   *
   * @param <T> {@link Class type} of {@link Object} to create.
   * @param objectType {@link Class type} from which the {@link Object }instance will be created.
   * @param parameterTypes array of {@link Class types} specifying the signature of the constructor
   * used to construct the {@link Object}.
   * @param args array of {@link Object arguments} passed to the constructor used to initialize the {@link Object}.
   * @return a new {@link Object} of the given {@link Class type} initialized with
   * the given array of {@link Object arguments}.
   * @see java.lang.Class
   */
  <T> T create(Class<T> objectType, Class<?>[] parameterTypes, Object... args);

}
