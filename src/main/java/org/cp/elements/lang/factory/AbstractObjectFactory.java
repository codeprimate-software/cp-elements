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

import static org.cp.elements.lang.OperatorUtils.from;

import java.lang.reflect.Constructor;

import org.cp.elements.context.configure.Configuration;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ClassUtils;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.util.ArrayUtils;
import org.cp.elements.util.convert.ConversionService;

/**
 * The AbstractObjectFactory class is a abstract base class encapsulating common functionality amongst all ObjectFactory
 * implementations.
 * <p/>
 * @author John J. Blum
 * @see java.lang.reflect.Constructor
 * @see org.cp.elements.lang.factory.ObjectFactory
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractObjectFactory implements ObjectFactory {

  private volatile Configuration configuration;

  private volatile ConversionService conversionService;

  protected boolean isConfigurationAvailable() {
    return (configuration != null);
  }

  protected Configuration getConfiguration() {
    Assert.state(configuration != null, "The reference to the Configuration was not properly initialized!");
    return configuration;
  }

  public final void setConfiguration(final Configuration configuration) {
    this.configuration = configuration;
  }

  protected boolean isConversionServiceAvailable() {
    return (conversionService != null);
  }

  protected ConversionService getConversionService() {
    Assert.state(conversionService != null, "The ConversionService was not properly initialized!");
    return conversionService;
  }

  public final void setConversionService(final ConversionService conversionService) {
    this.conversionService = conversionService;
  }

  protected Constructor findConstructor(final Class objectType, final Class... parameterTypes) {
    try {
      return objectType.getDeclaredConstructor(parameterTypes);
    }
    catch (NoSuchMethodException e) {
      if (!ArrayUtils.isEmpty(parameterTypes)) {
        // NOTE find the default, public no-arg constructor
        return findConstructor(objectType);
      }

      throw new NoSuchConstructorException(String.format(
        "Failed to find a constructor with signature ($1%s) in Class (%2$s)!", from(parameterTypes).toString(),
          objectType), e);
    }
  }

  @SuppressWarnings("unchecked")
  protected Class[] getArgumentTypes(final Object... arguments) {
    Class[] argumentTypes = new Class[arguments.length];
    int index = 0;

    for (Object argument : arguments) {
      argumentTypes[index++] = ObjectUtils.defaultIfNull(ClassUtils.getClass(argument), Object.class);
    }

    return argumentTypes;
  }

  @Override
  @SuppressWarnings("unchecked")
  public <T> T create(final String objectTypeName, final Object... args) {
    return (T) create(ClassUtils.loadClass(objectTypeName), getArgumentTypes(args), args);
  }

  @Override
  @SuppressWarnings("unchecked")
  public <T> T create(final String objectTypeName, final Class[] parameterTypes, final Object... args) {
    return (T) create(ClassUtils.loadClass(objectTypeName), parameterTypes, args);
  }

  @Override
  public <T> T create(final Class<T> objectType, final Object... args) {
    return create(objectType, getArgumentTypes(args), args);
  }

  @Override
  public <T> T create(final Class<T> objectType, final Class[] parameterTypes, final Object... args) {
    try {
      Constructor constructor = findConstructor(objectType, parameterTypes);

      T object;

      if (!ArrayUtils.isEmpty(constructor.getParameterTypes())) {
        Assert.equals(ArrayUtils.length(constructor.getParameterTypes()), ArrayUtils.length(args),
          "The number of arguments ({0,number,integer}) does not match the number of parameters ({1,number,integer}) for constructor ({2}) in Class ({3})!",
            ArrayUtils.length(args), ArrayUtils.length(constructor.getParameterTypes()), constructor, objectType);

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
          objectType, from(parameterTypes).toString(), from(args).toString()), e);
    }
  }

  protected <T> T postConstruct(final T object, final Object... args) {
    return object;
  }

}
