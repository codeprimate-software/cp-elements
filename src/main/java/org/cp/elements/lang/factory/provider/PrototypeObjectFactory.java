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

import org.cp.elements.lang.Configurable;
import org.cp.elements.lang.Initable;
import org.cp.elements.lang.ParameterizedInitable;
import org.cp.elements.lang.factory.AbstractObjectFactory;
import org.cp.elements.lang.factory.ObjectFactoryReferenceHolder;

/**
 * The PrototypeObjectFactory class creates a new instance of JavaBean compliant objects for every inovcation of create.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.factory.AbstractObjectFactory
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class PrototypeObjectFactory extends AbstractObjectFactory {

  /**
   * Constructs an instance of the PrototypeObjectFactory class, setting a reference to this ObjectFactory using the
   * ObjectFactoryReferenceHolder providing a reference has not already been set.
   *
   * @see org.cp.elements.lang.factory.ObjectFactoryReferenceHolder#compareAndSet(
   *  org.cp.elements.lang.factory.ObjectFactory, org.cp.elements.lang.factory.ObjectFactory)
   */
  public PrototypeObjectFactory() {
    ObjectFactoryReferenceHolder.compareAndSet(null, this);
  }

  /**
   * Overridden postConstruct method to perform post constructor (instantiation) configuration and initialization
   * actions on the newly constructed object.
   *
   * @param <T> the Class type of created object.
   * @param object the object created by this factory.
   * @param args an array of Objects arguments used for post construction initialization and configuration if no
   * constructor could be found with a signature matching the argument types.
   * @return the object fully configured and initialized.
   * @see #configure(Object)
   * @see #initialize(Object, Object...)
   */
  @Override
  protected <T> T postConstruct(T object, final Object... args) {
    object = super.postConstruct(object, args);
    object = configure(object);
    object = initialize(object, args);

    return object;
  }

  /**
   * Configures the object with an available Configuration if the object implements Configurable.
   *
   * @param <T> the Class type of the created object.
   * @param object the object/bean to configure.
   * @return the object after configuration.
   * @see #getConfiguration()
   * @see #isConfigurationAvailable()
   * @see org.cp.elements.lang.Configurable#configure(Object)
   */
  @SuppressWarnings("unchecked")
  protected <T> T configure(final T object) {
    if (object instanceof Configurable && isConfigurationAvailable()) {
      ((Configurable) object).configure(getConfiguration());
    }

    return object;
  }

  /**
   * Initializes the object with the specified Map of named parameters providing the object implements the
   * ParameterizedInitable interface, otherwise delegates to the initialize method accepting an array of arguments
   * by passing the values of the Map as the argument array.
   *
   * @param <T> the Class type of the created object.
   * @param object the object/bean to initialize.
   * @param parameters a Map of named parameters to initialize the object/bean.
   * @return the object after initialization.
   * @see #initialize(Object, Object...)
   * @see org.cp.elements.lang.ParameterizedInitable#init(java.util.Map)
   */
  protected <T> T initialize(final T object, final Map<?, ?> parameters) {
    if (object instanceof ParameterizedInitable) {
      ((ParameterizedInitable) object).init(parameters);
      return object;
    }
    else {
      return initialize(object, parameters.values());
    }
  }

  /**
   * Initializes the object with the specified array of arguments providing the object implements the
   * ParameterizedInitable interface, or calls the no argument init method if the object implements the Initable
   * interface, and finally, does nothing if the object is not Initable.
   *
   * @param <T> the Class type of the created object.
   * @param object the object/bean to initialize.
   * @param args the array of Object arguments used to initialize the object/bean.
   * @return the object after initialization.
   * @see #initialize(Object, java.util.Map)
   * @see org.cp.elements.lang.Initable#init()
   * @see org.cp.elements.lang.ParameterizedInitable#init(Object...)
   */
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
