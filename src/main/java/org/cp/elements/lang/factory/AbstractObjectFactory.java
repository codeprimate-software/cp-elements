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

import static org.cp.elements.util.CollectionExtensions.from;

import java.lang.reflect.Constructor;

import org.cp.elements.context.configure.Configuration;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ClassUtils;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.util.ArrayUtils;
import org.cp.elements.util.convert.ConversionService;

/**
 * The AbstractObjectFactory class is a abstract base class encapsulating functionality common to all ObjectFactory
 * implementations.
 * 
 * @author John J. Blum
 * @see java.lang.reflect.Constructor
 * @see org.cp.elements.context.configure.Configuration
 * @see org.cp.elements.lang.factory.ObjectFactory
 * @see org.cp.elements.util.convert.ConversionService
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractObjectFactory implements ObjectFactory {

  private volatile Configuration configuration;

  private volatile ConversionService conversionService;

  /**
   * Determines whether the reference to the application Configuration was initialized.
   * 
   * @return a boolean value indicating whether the application Configuration reference was initialized.
   */
  protected boolean isConfigurationAvailable() {
    return (configuration != null);
  }

  /**
   * Gets the reference to the application Configuration used by the ObjectFactory to perform object configuration.
   * 
   * @return a reference to the application Configuration.
   * @throws IllegalStateException if the reference to the application Configuration has not been initialized.
   */
  protected Configuration getConfiguration() {
    Assert.state(configuration != null, "The reference to the Configuration was not properly initialized!");
    return configuration;
  }

  /**
   * Sets a reference to an instance of the application Configuration in use.
   * 
   * @param configuration the Configuration reference in use by the application.
   */
  public final void setConfiguration(final Configuration configuration) {
    this.configuration = configuration;
  }

  /**
   * Determines whether the reference to the ConversionService was initialized.
   * 
   * @return a boolean value indicating whether the ConversionService reference was initialized.
   */
  protected boolean isConversionServiceAvailable() {
    return (conversionService != null);
  }

  /**
   * Gets the reference to the ConversionService used by the ObjectFactory to perform type conversions.
   * 
   * @return a reference to the ConversionService.
   * @throws IllegalStateException if the reference to the ConversionService has not been initialized.
   */
  protected ConversionService getConversionService() {
    Assert.state(conversionService != null, "The ConversionService was not properly initialized!");
    return conversionService;
  }

  /**
   * Sets a reference to the specified ConversionService.
   * 
   * @param conversionService the reference to the ConversionService.
   */
  public final void setConversionService(final ConversionService conversionService) {
    this.conversionService = conversionService;
  }

  /**
   * Determines the class types of the array of object arguments.
   * 
   * @param arguments the array of Object arguments to determine the class types for.
   * @return an array of Class types for each of the arguments in the array.
   * @see org.cp.elements.lang.ClassUtils#getClass(Object)
   */
  @SuppressWarnings("unchecked")
  protected Class[] getArgumentTypes(final Object... arguments) {
    Class[] argumentTypes = new Class[arguments.length];
    int index = 0;

    for (Object argument : arguments) {
      argumentTypes[index++] = ObjectUtils.defaultIfNull(ClassUtils.getClass(argument), Object.class);
    }

    return argumentTypes;
  }

  /**
   * Resolves the Class constructor with the given signature as determined by the parameter types.
   * 
   * @param objectType the Class from which the constructor is resolved.
   * @param parameterTypes the array of Class types determining the resolved constructor's signature.
   * @return a Constructor from the specified class with a matching signature based on the parameter types.
   * @throws NullPointerException if either the objectType or parameterTypes are null.
   * @see #resolveCompatibleConstructor(Class, Class[])
   * @see java.lang.Class
   * @see java.lang.reflect.Constructor
   */
  protected Constructor resolveConstructor(final Class<?> objectType, final Class... parameterTypes) {
    try {
      return objectType.getConstructor(parameterTypes);
    }
    catch (NoSuchMethodException e) {
      if (!ArrayUtils.isEmpty(parameterTypes)) {
        Constructor constructor = resolveCompatibleConstructor(objectType, parameterTypes);
        // if the "compatible" constructor is null, resolve to finding the public, default no-arg constructor
        return (constructor != null ? constructor : resolveConstructor(objectType));
      }

      throw new NoSuchConstructorException(String.format(
        "Failed to find a constructor with signature (%1$s) in Class (%2$s)!", from(parameterTypes).toString(),
          objectType.getName()), e);
    }
  }

  /**
   * Resolves the matching constructor for the specified Class type who's actual public constructor argument types
   * are assignment compatible with the expected parameter types.
   * 
   * @param objectType the Class from which the constructor is resolved.
   * @param parameterTypes the array of Class types determining the resolved constructor's signature.
   * @return a matching constructor from the specified Class type who's actual public constructor argument types
   * are assignment compatible with the expected parameter types.
   * @throws NullPointerException if either the objectType or parameterTypes are null.
   * @see #resolveConstructor(Class, Class[])
   * @see java.lang.Class
   * @see java.lang.reflect.Constructor
   */
  @SuppressWarnings("unchecked")
  protected Constructor resolveCompatibleConstructor(final Class<?> objectType, final Class<?>[] parameterTypes) {
    for (Constructor constructor : objectType.getConstructors()) {
      Class[] constructorParameterTypes = constructor.getParameterTypes();

      if (parameterTypes.length == constructorParameterTypes.length) {
        boolean match = true;

        for (int index = 0; index < constructorParameterTypes.length; index++) {
          match &= constructorParameterTypes[index].isAssignableFrom(parameterTypes[index]);
        }

        if (match) {
          return constructor;
        }
      }
    }

    return null;
  }

  /**
   * Creates an object given the fully qualified class name, initialized with the specified constructor arguments.
   * The parameter types of the constructor used to construct the object are determined from the arguments.
   * 
   * @param <T> the Class type of the created object.
   * @param objectTypeName a String indicating the fully qualified class name for the type of object to create.
   * @param args an array of Objects used as constructor arguments to initialize the object.
   * @return a newly created object of the given class type initialized with the specified arguments.
   * @see #create(String, Class[], Object...)
   * @see #getArgumentTypes(Object...)
   * @see org.cp.elements.lang.ClassUtils#loadClass(String)
   */
  @Override
  @SuppressWarnings("unchecked")
  public <T> T create(final String objectTypeName, final Object... args) {
    return (T) create(ClassUtils.loadClass(objectTypeName), getArgumentTypes(args), args);
  }

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
   * @see org.cp.elements.lang.ClassUtils#loadClass(String)
   * @see java.lang.Class
   */
  @Override
  @SuppressWarnings("unchecked")
  public <T> T create(final String objectTypeName, final Class[] parameterTypes, final Object... args) {
    return (T) create(ClassUtils.loadClass(objectTypeName), parameterTypes, args);
  }

  /**
   * Creates an object given the class type, initialized with the specified constructor arguments. The parameter types
   * of the constructor used to construct the object are determined from the arguments.
   * 
   * @param <T> the Class type of the created object.
   * @param objectType the Class type from which the instance is created.
   * @param args an array of Objects used as constructor arguments to initialize the object.
   * @return a newly created object of the given class type initialized with the specified arguments.
   * @throws ObjectInstantiationException if an error occurs during object creation.
   * @see #create(Class, Class[], Object...)
   * @see #getArgumentTypes(Object...)
   * @see java.lang.Class
   */
  @Override
  public <T> T create(final Class<T> objectType, final Object... args) {
    return create(objectType, getArgumentTypes(args), args);
  }

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
   * @throws ObjectInstantiationException if an error occurs during object creation.
   * @see #create(Class, Object...)
   * @see #resolveConstructor
   * @see java.lang.Class
   * @see java.lang.reflect.Constructor
   */
  @Override
  public <T> T create(final Class<T> objectType, final Class[] parameterTypes, final Object... args) {
    try {
      Constructor constructor = resolveConstructor(objectType, parameterTypes);

      T object;

      if (!ArrayUtils.isEmpty(constructor.getParameterTypes())) {
        Assert.equals(ArrayUtils.nullSafeLength(constructor.getParameterTypes()), ArrayUtils.nullSafeLength(args),
          "The number of arguments ({0,number,integer}) does not match the number of parameters ({1,number,integer}) for constructor ({2}) in Class ({3})!",
            ArrayUtils.nullSafeLength(args), ArrayUtils.nullSafeLength(constructor.getParameterTypes()), constructor, objectType);

        object = postConstruct(objectType.cast(constructor.newInstance(args)));
      }
      else {
        object = postConstruct(objectType.cast(constructor.newInstance()), args);
      }

      return object;
    }
    catch (Exception e) {
      throw new ObjectInstantiationException(String.format(
        "Failed to instantiate and instance of class (%1$s) with constructor having signature (%2$s) using arguments (%3$s)!",
          ClassUtils.getName(objectType), from(parameterTypes).toString(), from(args).toString()), e);
    }
  }

  /**
   * Method called after construction of the object allowing custom ObjectFactory implementations to perform post
   * construction initialization and additional configuration.  The default implementation is to do nothing.
   * 
   * @param <T> the Class type of object created.
   * @param object the object created by this factory.
   * @param args the array of Objects arguments used for post construction initialization and configuration if no
   * constructor could be found with a signature matching the argument types.
   * @return the object after post construction initialization and configuration.
   */
  protected <T> T postConstruct(final T object, final Object... args) {
    return object;
  }
}
