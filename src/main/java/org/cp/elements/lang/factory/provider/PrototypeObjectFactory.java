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
package org.cp.elements.lang.factory.provider;

import java.util.Map;

import org.cp.elements.context.configure.Configuration;
import org.cp.elements.lang.Configurable;
import org.cp.elements.lang.Initable;
import org.cp.elements.lang.ParameterizedInitable;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.lang.factory.AbstractObjectFactory;
import org.cp.elements.lang.factory.ObjectFactory;
import org.cp.elements.lang.factory.ObjectFactoryReferenceHolder;

/**
 * {@link AbstractObjectFactory} {@literal service provider implementation (SPI)} that Constructs a new
 * {@literal JavaBean} compliant {@link Object Objects} for every invocation of {@literal create(..)}.
 *
 * That is, this {@link AbstractObjectFactory} creates {@literal prototype} {@link Object POJOs}.
 *
 * @author John J. Blum
 * @see org.cp.elements.context.configure.Configuration
 * @see org.cp.elements.lang.Configurable
 * @see org.cp.elements.lang.Initable
 * @see org.cp.elements.lang.ParameterizedInitable
 * @see org.cp.elements.lang.factory.AbstractObjectFactory
 * @see org.cp.elements.lang.factory.ObjectFactory
 * @see org.cp.elements.lang.factory.ObjectFactoryReferenceHolder
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class PrototypeObjectFactory extends AbstractObjectFactory {

  /**
   * Constructs a new {@link PrototypeObjectFactory} setting a reference to {@literal this}
   * {@link ObjectFactory} using the {@link ObjectFactoryReferenceHolder} providing a reference
   * has not already been set.
   *
   * @see org.cp.elements.lang.factory.ObjectFactoryReferenceHolder#compareAndSet(org.cp.elements.lang.factory.ObjectFactory, org.cp.elements.lang.factory.ObjectFactory)
   */
  public PrototypeObjectFactory() {
    ObjectFactoryReferenceHolder.compareAndSet(null, this);
  }

  /**
   * Overridden {@code postConstruct} method used to perform post construction configuration and initialization of
   * the newly constructed {@link Object}.
   *
   * @param <T> {@link Class type} of {@link Object} created by {@literal this} {@link ObjectFactory}.
   * @param object {@link Object} created by {@literal this} {@link ObjectFactory}.
   * @param args array of {@link Object arguments} used during post construction to configure and initialize
   * the created {@link Object} if no constructor could be found with a signature
   * matching the argument {@link Class types}.
   * @return the fully configured and initialized {@link Object}.
   * @see #initialize(Object, Object...)
   * @see #configure(Object)
   */
  @Override
  protected <T> T postConstruct(T object, Object... args) {

    T result = object;

    result = super.postConstruct(result, args);
    result = configure(result);
    result = initialize(result, args);

    return result;
  }

  /**
   * Configures the given {@link Object} if {@link Configurable} and {@link Configuration} is available.
   *
   * @param <T> {@link Class type} of {@link Object} to configure.
   * @param object {@link Object} to configure.
   * @return the given {@link Object} after configuration.
   * @see org.cp.elements.lang.Configurable#configure(Object)
   * @see #isConfigurationAvailable()
   * @see #getConfiguration()
   */
  @SuppressWarnings("unchecked")
  protected @Nullable <T> T configure(@Nullable T object) {

    if (object instanceof Configurable && isConfigurationAvailable()) {
      ((Configurable<Configuration>) object).configure(getConfiguration());
    }

    return object;
  }

  /**
   * Initializes the given {@link Object} with the array of {@link Object arguments} providing the {@link Object}
   * implements the {@link ParameterizedInitable} interface. Alternatively, this method will call the no argument
   * {@link Initable#init()} method if the {@link Object} implements the {@link Initable} interface.
   *
   * This method does nothing if the {@link Object} cannot be initialized in any capacity.
   *
   * @param <T> {@link Class type} of {@link Object} to initialize.
   * @param object {@link Object} to initialize.
   * @param args array of {@link Object arguments} used to initialize the {@link Object}.
   * @return the given {@link Object} after initialization.
   * @see org.cp.elements.lang.ParameterizedInitable#init(Object...)
   * @see org.cp.elements.lang.Initable#init()
   * @see #initialize(Object, java.util.Map)
   */
  protected @Nullable <T> T initialize(@Nullable T object, Object... args) {

    if (object instanceof ParameterizedInitable) {
      ((ParameterizedInitable) object).init(args);
    }
    else if (object instanceof Initable) {
      ((Initable) object).init();
    }

    return object;
  }

  /**
   * Initializes the given {@link Object} with the {@link Map} of named parameters providing the {@link Object}
   * implements the {@link ParameterizedInitable} interface, otherwise delegates to
   * the {@link #initialize(Object, Object...)} method by passing the {@link Map#values() values} of the {@link Map}
   * as an array of {@link Object arguments}.
   *
   * @param <T> {@link Class type} of {@link Object} to initialize.
   * @param object {@link Object} to initialize.
   * @param parameters {@link Map} of named parameters used to initialize the {@link Object}.
   * @return the given {@link Object} after initialization.
   * @see org.cp.elements.lang.ParameterizedInitable#init(java.util.Map)
   * @see #initialize(Object, Object...)
   */
  protected @Nullable <T> T initialize(@Nullable T object, Map<?, ?> parameters) {

    if (object instanceof ParameterizedInitable) {
      ((ParameterizedInitable) object).init(parameters);
      return object;
    }
    else {
      return initialize(object, parameters.values());
    }
  }
}
