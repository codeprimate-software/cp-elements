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

import java.lang.reflect.Constructor;
import java.util.function.Function;

import org.cp.elements.context.configure.Configuration;
import org.cp.elements.data.conversion.ConversionService;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ClassUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.util.ArrayUtils;
import org.cp.elements.util.CollectionExtensions;

/**
 * Abstract base class implementing the {@link ObjectFactory} interface to encapsulate functionality
 * common to all {@link ObjectFactory} implementations.
 *
 * @author John J. Blum
 * @see java.lang.reflect.Constructor
 * @see java.util.function.Function
 * @see org.cp.elements.context.configure.Configuration
 * @see org.cp.elements.data.conversion.ConversionService
 * @see org.cp.elements.lang.factory.ObjectFactory
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractObjectFactory implements ObjectFactory {

  private volatile Configuration configuration;

  private volatile ConversionService conversionService;

  @SuppressWarnings("rawtypes")
  private volatile Function objectPostProcessor = Function.identity();

  /**
   * Determines whether the reference to an application {@link Configuration} was initialized.
   *
   * @return a boolean value indicating whether reference to a application {@link Configuration} was initialized.
   * @see org.cp.elements.context.configure.Configuration
   */
  @NullSafe
  protected boolean isConfigurationAvailable() {
    return this.configuration != null;
  }

  /**
   * Gets the reference to an application {@link Configuration} used by {@literal this} {@link ObjectFactory}
   * to perform {@link Object} configuration.
   *
   * @return a reference to the application {@link Configuration}.
   * @throws IllegalStateException if a reference to the application {@link Configuration} was not initialized.
   * @see org.cp.elements.context.configure.Configuration
   */
  @NullSafe
  protected @NotNull Configuration getConfiguration() {

    Configuration configuration = this.configuration;

    Assert.state(configuration != null,
      "A Configuration for this ObjectFactory [%s] was not properly initialized", getClass().getName());

    return configuration;
  }

  /**
   * Sets the reference to an application {@link Configuration}.
   *
   * @param configuration {@link Configuration} used by the application.
   * @see org.cp.elements.context.configure.Configuration
   */
  public final void setConfiguration(@NotNull Configuration configuration) {
    this.configuration = configuration;
  }

  /**
   * Determines whether the reference to a {@link ConversionService} was initialized.
   *
   * @return a boolean value indicating whether the reference to a {@link ConversionService} was initialized.
   * @see org.cp.elements.data.conversion.ConversionService
   */
  protected boolean isConversionServiceAvailable() {
    return this.conversionService != null;
  }

  /**
   * Gets the reference to a {@link ConversionService} used by {@literal this} {@link ObjectFactory}
   * to perform {@link Class type} conversions.
   *
   * @return the reference to a {@link ConversionService} used by {@literal this} {@link ObjectFactory}
   * to perform {@link Class type} conversions.
   * @throws IllegalStateException if the reference to a {@link ConversionService} was not initialized.
   * @see org.cp.elements.data.conversion.ConversionService
   */
  protected @NotNull ConversionService getConversionService() {

    ConversionService conversionService = this.conversionService;

    Assert.state(conversionService != null,
      "The ConversionService used by this ObjectFactory [%s] was not properly initialized",
        getClass().getName());

    return conversionService;
  }

  /**
   * Sets the reference to a {@link ConversionService} used by {@literal this} {@link ObjectFactory}
   * to perform {@link Class type} conversions.
   *
   * @param conversionService {@link ConversionService} that will be used by {@literal this} {@link ObjectFactory}
   * to perform {@link Class type} conversions.
   * @see org.cp.elements.data.conversion.ConversionService
   */
  public final void setConversionService(@NotNull ConversionService conversionService) {
    this.conversionService = conversionService;
  }

  /**
   * Gets the configured {@link Function} used to post process the {@link Object} after creation.
   *
   * @param <T> {@link Class type} of the {@link Function} argument.
   * @param <R> {@link Class type} of the {@link Function} return value.
   * @return a {@link Function} configured to post process the {@link Object} after creation.
   * @see #registerObjectPostProcessor(Function)
   * @see java.util.function.Function
   */
  @SuppressWarnings("unchecked")
  protected @NotNull <T, R> Function<T, R> getObjectPostProcessor() {
    return (Function<T, R>) this.objectPostProcessor;
  }

  /**
   * Registers a {@link Function} used to post process the {@link Object} after it is created by
   * {@literal this} {@link ObjectFactory}.
   *
   * @param <T> {@link Class type} of {@literal this} {@link ObjectFactory}.
   * @param objectPostProcessor {@link Function} used to post process the {@link Object} after it is created by
   * {@literal this} {@link ObjectFactory}.
   * @return {@literal this} {@link ObjectFactory}.
   * @throws IllegalArgumentException if the {@link Function} is {@literal null}.
   * @see #getObjectPostProcessor()
   * @see java.util.function.Function
   */
  @SuppressWarnings("unchecked")
  public synchronized @NotNull <T extends AbstractObjectFactory> T registerObjectPostProcessor(
      @NotNull Function<?, ?> objectPostProcessor) {

    Assert.notNull(objectPostProcessor, "The Function used to post process the object is required");

    this.objectPostProcessor = this.objectPostProcessor.andThen(objectPostProcessor);

    return (T) this;
  }

  /**
   * Resolves the {@link Class} {@link Constructor} with the given signature as determined by
   * the given array of {@link Class parameter types}.
   *
   * @param <T> {@link Class type} in which the {@link Constructor} is declared.
   * @param objectType {@link Class} from which the {@link Constructor} is resolved.
   * @param parameterTypes array of {@link Class types} used to determine the signature of the resolved constructor.
   * @return a {@link Constructor} from the given {@link Class} with a matching signature based on
   * the array of {@link Class parameter types}.
   * @throws NullPointerException if either the {@literal Class objectType} or array of {@link Class parameterTypes}
   * are {@literal null}.
   * @see #resolveCompatibleConstructor(Class, Class[])
   * @see java.lang.reflect.Constructor
   * @see java.lang.Class
   */
  protected <T> Constructor<T> resolveConstructor(@NotNull Class<T> objectType, Class<?>... parameterTypes) {

    try {
      return objectType.getConstructor(parameterTypes);
    }
    catch (NoSuchMethodException cause) {

      if (ArrayUtils.isNotEmpty(parameterTypes)) {

        Constructor<T> constructor = resolveCompatibleConstructor(objectType, parameterTypes);

        // if the "compatible" constructor is null, resolve to finding the default, public no-arg constructor
        try {
          return constructor != null ? constructor
            : resolveConstructor(objectType);
        }
        catch (NoSuchConstructorException ignore) { }
      }

      String message = String.format("Failed to find a constructor with signature [%1$s] in Class [%2$s]",
        CollectionExtensions.from(parameterTypes), objectType.getName());

      throw new NoSuchConstructorException(message, cause);
    }
  }

  /**
   * Resolves a matching {@link Constructor} for the given {@link Class} whose actual array of {@literal public}
   * {@link Constructor} {@link Object argument types} are assignment compatible with the expected
   * array of {@link Class parameter types}.
   *
   * @param <T> {@link Class type} in which the {@link Constructor} is declared.
   * @param objectType {@link Class} from which the {@link Constructor} is resolved.
   * @param parameterTypes array of {@link Class types} used to determine the signature of the resolved constructor.
   * @return a matching {@link Constructor} from the given {@link Class} whose actual array of {@literal public}
   * {@link Constructor} {@link Object argument types} are assignment compatible with the expected
   * array of {@link Class parameter types}.
   * @throws NullPointerException if either the {@link Class objectType} or the array of {@link Class parameterTypes}
   * are {@literal null}.
   * @see #resolveConstructor(Class, Class[])
   * @see java.lang.reflect.Constructor
   * @see java.lang.Class
   */
  @SuppressWarnings("unchecked")
  protected @Nullable <T> Constructor<T> resolveCompatibleConstructor(@NotNull Class<T> objectType,
      Class<?>... parameterTypes) {

    for (Constructor<?> constructor : objectType.getConstructors()) {

      Class<?>[] constructorParameterTypes = constructor.getParameterTypes();

      if (parameterTypes.length == constructorParameterTypes.length) {

        boolean match = true;

        for (int index = 0; match && index < constructorParameterTypes.length; index++) {
          match = constructorParameterTypes[index].isAssignableFrom(parameterTypes[index]);
        }

        if (match) {
          return (Constructor<T>) constructor;
        }
      }
    }

    return null;
  }

  /**
   * Method called after construction of the {@link Object} allowing custom {@link ObjectFactory} implementations
   * to perform post construction initialization and additional configuration as required.
   *
   * The default implementation applies the registered {@link Function object post processors}.
   *
   * @param <T> {@link Class type} of {@link Object} to create.
   * @param object {@link Object} created by {@literal this} {@link ObjectFactory}.
   * @param args array of {@link Object arguments} used during post construction initialization and configuration
   * if no {@link Constructor} could be found with a signature matching the array of {@link Object argument types}.
   * @return the {@link Object} after post construction initialization and configuration.
   * @see java.util.function.Function#apply(Object)
   * @see #getObjectPostProcessor()
   * @see java.lang.Object
   */
  protected <T> T postConstruct(T object, Object... args) {
    return this.<T, T>getObjectPostProcessor().apply(object);
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
  @Override
  public <T> T create(@NotNull Class<T> objectType, Class<?>[] parameterTypes, Object... args) {

    try {

      Constructor<T> constructor = resolveConstructor(objectType, parameterTypes);

      T object;

      if (ArrayUtils.isNotEmpty(constructor.getParameterTypes())) {

        int numberOfParameters = ArrayUtils.nullSafeLength(constructor.getParameterTypes());
        int numberOfArguments = ArrayUtils.nullSafeLength(args);

        String message = "The number of arguments [{0,number,integer}] does not match the number of parameters"
          + " [{1,number,integer}] for Constructor [{2}] in Class [{3}]!";

        Assert.equals(numberOfParameters, numberOfArguments, message, numberOfArguments, numberOfParameters,
          constructor, objectType);

        object = postConstruct(objectType.cast(constructor.newInstance(args)));
      }
      else {
        object = postConstruct(objectType.cast(constructor.newInstance()), args);
      }

      return object;
    }
    catch (Exception cause) {

      String message =  String.format( "Failed to instantiate an instance of class [%1$s] with constructor"
          + " having signature [%2$s] using arguments [%3$s]!", ClassUtils.getName(objectType),
        CollectionExtensions.from(parameterTypes), CollectionExtensions.from(args));

      throw new ObjectInstantiationException(message, cause);
    }
  }
}
