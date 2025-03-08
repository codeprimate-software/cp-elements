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
package org.cp.elements.lang;

import static org.cp.elements.text.FormatUtils.format;

import org.cp.elements.beans.BeanIntrospectionException;
import org.cp.elements.beans.BeansException;
import org.cp.elements.beans.IllegalPropertyValueException;
import org.cp.elements.beans.PropertyNotFoundException;
import org.cp.elements.beans.PropertyNotSetException;
import org.cp.elements.beans.ReadPropertyException;
import org.cp.elements.beans.WritePropertyException;
import org.cp.elements.biz.rules.RuleException;
import org.cp.elements.context.configure.ConfigurationException;
import org.cp.elements.dao.DataAccessException;
import org.cp.elements.data.caching.CacheEntryException;
import org.cp.elements.data.caching.CacheEntryNotFoundException;
import org.cp.elements.data.caching.CacheException;
import org.cp.elements.data.caching.CacheNotFoundException;
import org.cp.elements.data.compression.CompressionException;
import org.cp.elements.data.compression.DecompressionException;
import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.data.mapping.MappingException;
import org.cp.elements.data.serialization.DeserializationException;
import org.cp.elements.data.serialization.SerializationException;
import org.cp.elements.function.FunctionException;
import org.cp.elements.function.FunctionExecutionException;
import org.cp.elements.io.NoSuchDirectoryException;
import org.cp.elements.io.NoSuchFileException;
import org.cp.elements.lang.factory.NoSuchConstructorException;
import org.cp.elements.lang.factory.ObjectInstantiationException;
import org.cp.elements.lang.reflect.ConstructorNotFoundException;
import org.cp.elements.lang.reflect.FieldAccessException;
import org.cp.elements.lang.reflect.FieldNotFoundException;
import org.cp.elements.lang.reflect.MethodInvocationException;
import org.cp.elements.lang.reflect.MethodNotFoundException;
import org.cp.elements.lang.reflect.UnhandledMethodInvocationException;
import org.cp.elements.management.ManagementException;
import org.cp.elements.management.MonitoringException;
import org.cp.elements.net.NetworkException;
import org.cp.elements.net.NoAvailablePortException;
import org.cp.elements.process.EmbeddedProcessExecutionException;
import org.cp.elements.process.PidUnknownException;
import org.cp.elements.process.ProcessException;
import org.cp.elements.process.ProcessExecutionException;
import org.cp.elements.process.ProcessNotRespondingException;
import org.cp.elements.security.AbstractSecurityException;
import org.cp.elements.security.AuthenticationException;
import org.cp.elements.security.AuthorizationException;
import org.cp.elements.security.model.UserNotFoundException;
import org.cp.elements.service.ServiceException;
import org.cp.elements.service.ServiceInvocationException;
import org.cp.elements.service.ServiceUnavailableException;
import org.cp.elements.service.provider.ServiceNotProvidedException;
import org.cp.elements.test.FailedTestException;
import org.cp.elements.test.HungTestException;
import org.cp.elements.test.TestException;
import org.cp.elements.text.FormatException;
import org.cp.elements.text.ParseException;
import org.cp.elements.util.ApplicationException;
import org.cp.elements.util.LoserException;
import org.cp.elements.util.ReadOnlyException;
import org.cp.elements.util.SystemException;
import org.cp.elements.util.UndeclaredPropertyException;
import org.cp.elements.util.UndefinedPropertyException;
import org.cp.elements.util.UserException;
import org.cp.elements.util.WriteOnlyException;
import org.cp.elements.util.paging.PageNotFoundException;
import org.cp.elements.util.search.SearchException;
import org.cp.elements.util.sort.SortException;

/**
 * Abstract object factory used to construct and initialize common and useful Elements API
 * {@link RuntimeException RuntimeExceptions}.
 *
 * @author John Blum
 * @see org.cp.elements.beans.BeansException
 * @see org.cp.elements.beans.BeanIntrospectionException
 * @see org.cp.elements.beans.IllegalPropertyValueException
 * @see org.cp.elements.beans.PropertyNotFoundException
 * @see org.cp.elements.beans.PropertyNotSetException
 * @see org.cp.elements.beans.ReadPropertyException
 * @see org.cp.elements.beans.WritePropertyException
 * @see org.cp.elements.biz.rules.RuleException
 * @see org.cp.elements.context.configure.ConfigurationException
 * @see org.cp.elements.dao.DataAccessException
 * @see org.cp.elements.data.caching.CacheException
 * @see org.cp.elements.data.caching.CacheEntryException
 * @see org.cp.elements.data.caching.CacheEntryNotFoundException
 * @see org.cp.elements.data.caching.CacheNotFoundException
 * @see org.cp.elements.data.compression.CompressionException
 * @see org.cp.elements.data.compression.DecompressionException
 * @see org.cp.elements.data.conversion.ConversionException
 * @see org.cp.elements.data.serialization.DeserializationException
 * @see org.cp.elements.data.serialization.SerializationException
 * @see org.cp.elements.data.mapping.MappingException
 * @see org.cp.elements.function.FunctionException
 * @see org.cp.elements.function.FunctionExecutionException
 * @see org.cp.elements.io.NoSuchDirectoryException
 * @see org.cp.elements.io.NoSuchFileException
 * @see org.cp.elements.lang.AssertionException
 * @see org.cp.elements.lang.CloneException
 * @see org.cp.elements.lang.ComparisonException
 * @see org.cp.elements.lang.EqualityException
 * @see org.cp.elements.lang.ExpectedException
 * @see org.cp.elements.lang.IdentityException
 * @see org.cp.elements.lang.IllegalTypeException
 * @see org.cp.elements.lang.ImmutableObjectException
 * @see org.cp.elements.lang.InitializationException
 * @see org.cp.elements.lang.ObjectNotFoundException
 * @see org.cp.elements.lang.ResourceNotFoundException
 * @see org.cp.elements.lang.ThrowableOperationException
 * @see org.cp.elements.lang.TypeNotFoundException
 * @see org.cp.elements.lang.factory.NoSuchConstructorException
 * @see org.cp.elements.lang.factory.ObjectInstantiationException
 * @see org.cp.elements.lang.reflect.ConstructorNotFoundException
 * @see org.cp.elements.lang.reflect.FieldAccessException
 * @see org.cp.elements.lang.reflect.FieldNotFoundException
 * @see org.cp.elements.lang.reflect.MethodInvocationException
 * @see org.cp.elements.lang.reflect.MethodNotFoundException
 * @see org.cp.elements.lang.reflect.UnhandledMethodInvocationException
 * @see org.cp.elements.management.ManagementException
 * @see org.cp.elements.management.MonitoringException
 * @see org.cp.elements.net.NetworkException
 * @see org.cp.elements.net.NoAvailablePortException
 * @see org.cp.elements.process.EmbeddedProcessExecutionException
 * @see org.cp.elements.process.PidUnknownException
 * @see org.cp.elements.process.ProcessException
 * @see org.cp.elements.process.ProcessExecutionException
 * @see org.cp.elements.process.ProcessNotRespondingException
 * @see org.cp.elements.security.AuthenticationException
 * @see org.cp.elements.security.AuthorizationException
 * @see org.cp.elements.security.AbstractSecurityException
 * @see org.cp.elements.security.model.UserNotFoundException
 * @see org.cp.elements.service.ServiceException
 * @see org.cp.elements.service.ServiceInvocationException
 * @see org.cp.elements.service.ServiceUnavailableException
 * @see org.cp.elements.service.provider.ServiceNotProvidedException
 * @see org.cp.elements.test.FailedTestException
 * @see org.cp.elements.test.HungTestException
 * @see org.cp.elements.test.TestException
 * @see org.cp.elements.text.FormatException
 * @see org.cp.elements.text.ParseException
 * @see org.cp.elements.util.ApplicationException
 * @see org.cp.elements.util.LoserException
 * @see org.cp.elements.util.ReadOnlyException
 * @see org.cp.elements.util.SystemException
 * @see org.cp.elements.util.UndeclaredPropertyException
 * @see org.cp.elements.util.UndefinedPropertyException
 * @see org.cp.elements.util.UserException
 * @see org.cp.elements.util.WriteOnlyException
 * @see org.cp.elements.util.paging.PageNotFoundException
 * @see org.cp.elements.util.search.SearchException
 * @see org.cp.elements.util.sort.SortException
 * @since 1.0.0
 */
@SuppressWarnings({ "ClassDataAbstractionCoupling", "ClassFanOutComplexity", "unused" })
public abstract class ElementsExceptionsFactory {

  // package org.cp.elements.beans

  /**
   * Constructs a new {@link BeansException} initialized with the given {@link String message}
   * to describe the exception.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link BeansException}.
   * @see #newBeansException(Throwable, String, Object...)
   * @see org.cp.elements.beans.BeansException
   */
  public static BeansException newBeansException(String message, Object... args) {
    return newBeansException(null, message, args);
  }

  /**
   * Constructs a new {@link BeansException} initialized with the given {@link String message}
   * to describe the exception along with a {@link Throwable cause} used as the reason why the exception was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link BeansException}.
   * @see org.cp.elements.beans.BeansException
   */
  public static BeansException newBeansException(Throwable cause, String message, Object... args) {
    return new BeansException(format(message, args), cause) { };
  }

  /**
   * Constructs a new {@link BeanIntrospectionException} initialized with the given {@link String message}
   * to describe the exception.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link BeanIntrospectionException}.
   * @see #newBeanIntrospectionException(Throwable, String, Object...)
   * @see org.cp.elements.beans.BeanIntrospectionException
   */
  public static BeanIntrospectionException newBeanIntrospectionException(String message, Object... args) {
    return newBeanIntrospectionException(null, message, args);
  }

  /**
   * Constructs a new {@link BeanIntrospectionException} initialized with the given {@link String message}
   * to describe the exception along with a {@link Throwable cause} used as the reason why the exception was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link BeanIntrospectionException}.
   * @see org.cp.elements.beans.BeanIntrospectionException
   */
  public static BeanIntrospectionException newBeanIntrospectionException(Throwable cause, String message,
      Object... args) {

    return new BeanIntrospectionException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link IllegalPropertyValueException} initialized with
   * the given {@link String message} to describe the {@link IllegalPropertyValueException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link IllegalPropertyValueException}.
   * @see #newIllegalPropertyValueException(Throwable, String, Object...)
   * @see org.cp.elements.beans.IllegalPropertyValueException
   */
  public static IllegalPropertyValueException newIllegalPropertyValueException(String message, Object... args) {
    return newIllegalPropertyValueException(null, message, args);
  }

  /**
   * Constructs a new {@link IllegalPropertyValueException} initialized with
   * the given {@link String message} to describe the {@link IllegalPropertyValueException} along with
   * a {@link Throwable cause} used as the reason why the {@link IllegalPropertyValueException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link IllegalPropertyValueException}.
   * @see org.cp.elements.beans.IllegalPropertyValueException
   */
  public static IllegalPropertyValueException newIllegalPropertyValueException(Throwable cause,
      String message, Object... args) {

    return new IllegalPropertyValueException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link PropertyNotFoundException} initialized with the given {@link String message}
   * to describe the {@link PropertyNotFoundException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link PropertyNotFoundException}.
   * @see #newPropertyNotFoundException(Throwable, String, Object...)
   * @see org.cp.elements.beans.PropertyNotFoundException
   */
  public static PropertyNotFoundException newPropertyNotFoundException(String message, Object... args) {
    return newPropertyNotFoundException(null, message, args);
  }

  /**
   * Constructs a new {@link PropertyNotFoundException} initialized with the given {@link String message}
   * to describe the {@link PropertyNotFoundException} along with a {@link Throwable cause} used as the reason
   * why the {@link PropertyNotFoundException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link PropertyNotFoundException}.
   * @see org.cp.elements.beans.PropertyNotFoundException
   */
  public static PropertyNotFoundException newPropertyNotFoundException(Throwable cause,
      String message, Object... args) {

    return new PropertyNotFoundException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link PropertyNotSetException} initialized with
   * the given {@link String message} to describe the {@link PropertyNotSetException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link PropertyNotSetException}.
   * @see #newPropertyNotSetException(Throwable, String, Object...)
   * @see org.cp.elements.beans.PropertyNotSetException
   */
  public static PropertyNotSetException newPropertyNotSetException(String message, Object... args) {
    return newPropertyNotSetException(null, message, args);
  }

  /**
   * Constructs a new {@link PropertyNotSetException} initialized with
   * the given {@link String message} to describe the {@link PropertyNotSetException} along with
   * a {@link Throwable cause} used as the reason why the {@link PropertyNotSetException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link PropertyNotSetException}.
   * @see org.cp.elements.beans.PropertyNotSetException
   */
  public static PropertyNotSetException newPropertyNotSetException(Throwable cause,
      String message, Object... args) {

    return new PropertyNotSetException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link ReadPropertyException} initialized with the given {@link String message}
   * to describe the {@link ReadPropertyException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ReadPropertyException}.
   * @see #newReadPropertyException(Throwable, String, Object...)
   * @see org.cp.elements.beans.ReadPropertyException
   */
  public static ReadPropertyException newReadPropertyException(String message, Object... args) {
    return newReadPropertyException(null, message, args);
  }

  /**
   * Constructs a new {@link ReadPropertyException} initialized with the given {@link String message}
   * to describe the {@link ReadPropertyException} along with a {@link Throwable cause} used as the reason
   * why the {@link ReadPropertyException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ReadPropertyException}.
   * @see org.cp.elements.beans.ReadPropertyException
   */
  public static ReadPropertyException newReadPropertyException(Throwable cause, String message, Object... args) {
    return new ReadPropertyException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link WritePropertyException} initialized with the given {@link String message}
   * to describe the {@link WritePropertyException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link WritePropertyException}.
   * @see #newWritePropertyException(Throwable, String, Object...)
   * @see org.cp.elements.beans.WritePropertyException
   */
  public static WritePropertyException newWritePropertyException(String message, Object... args) {
    return newWritePropertyException(null, message, args);
  }

  /**
   * Constructs a new {@link WritePropertyException} initialized with the given {@link String message}
   * to describe the {@link WritePropertyException} along with a {@link Throwable cause} used as the reason
   * why the {@link WritePropertyException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link WritePropertyException}.
   * @see org.cp.elements.beans.WritePropertyException
   */
  public static WritePropertyException newWritePropertyException(Throwable cause, String message, Object... args) {
    return new WritePropertyException(format(message, args), cause);
  }

  // package org.cp.elements.biz.rules

  /**
   * Constructs a new {@link RuleException} initialized with the given {@link String message}
   * to describe the {@link RuleException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link RuleException}.
   * @see #newRuleException(Throwable, String, Object...)
   * @see org.cp.elements.biz.rules.RuleException
   */
  public static RuleException newRuleException(String message, Object... args) {
    return newRuleException(null, message, args);
  }

  /**
   * Constructs a new {@link RuleException} initialized with the given {@link String message}
   * to describe the {@link RuleException} along with a {@link Throwable cause} used as the reason
   * why the {@link RuleException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link RuleException}.
   * @see org.cp.elements.biz.rules.RuleException
   */
  public static RuleException newRuleException(Throwable cause, String message, Object... args) {
    return new RuleException(format(message, args), cause);
  }

  // package org.cp.elements.context.configure

  /**
   * Constructs a new {@link ConfigurationException} initialized with the given {@link String message}
   * to describe the {@link ConfigurationException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ConfigurationException}.
   * @see #newConfigurationException(Throwable, String, Object...)
   * @see org.cp.elements.context.configure.ConfigurationException
   */
  public static ConfigurationException newConfigurationException(String message, Object... args) {
    return newConfigurationException(null, message, args);
  }

  /**
   * Constructs a new {@link ConfigurationException} initialized with the given {@link String message}
   * to describe the {@link ConfigurationException} along with a {@link Throwable cause} used as the reason
   * why the {@link ConfigurationException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ConfigurationException}.
   * @see org.cp.elements.context.configure.ConfigurationException
   */
  public static ConfigurationException newConfigurationException(Throwable cause, String message, Object... args) {
    return new ConfigurationException(format(message, args), cause);
  }

  // package org.cp.elements.dao

  /**
   * Constructs a new {@link DataAccessException} initialized with the given {@link String message}
   * to describe the {@link DataAccessException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link DataAccessException}.
   * @see #newDataAccessException(Throwable, String, Object...)
   * @see org.cp.elements.dao.DataAccessException
   */
  public static DataAccessException newDataAccessException(String message, Object... args) {
    return newDataAccessException(null, message, args);
  }

  /**
   * Constructs a new {@link DataAccessException} initialized with the given {@link String message}
   * to describe the {@link DataAccessException} along with a {@link Throwable cause} used as the reason
   * why the {@link DataAccessException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link DataAccessException}.
   * @see org.cp.elements.dao.DataAccessException
   */
  public static DataAccessException newDataAccessException(Throwable cause, String message, Object... args) {
    return new DataAccessException(format(message, args), cause);
  }

  // package org.cp.elements.data

  /**
   * Constructs a new {@link CacheException} initialized with the given {@link String message}
   * to describe the {@link CacheException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link CacheException}.
   * @see #newCacheException(Throwable, String, Object...)
   * @see org.cp.elements.data.caching.CacheException
   */
  public static CacheException newCacheException(String message, Object... args) {
    return newCacheException(null, message, args);
  }

  /**
   * Constructs a new {@link CacheException} initialized with the given {@link String message}
   * to describe the {@link CacheException} along with a {@link Throwable cause} used as the reason
   * why the {@link CacheException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link CacheException}.
   * @see org.cp.elements.data.caching.CacheException
   */
  public static CacheException newCacheException(Throwable cause, String message, Object... args) {
    return new CacheException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link CacheEntryException} initialized with the given {@link String message}
   * to describe the exception.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link CacheEntryException}.
   * @see #newCacheEntryException(Throwable, String, Object...)
   * @see org.cp.elements.data.caching.CacheEntryException
   */
  public static CacheEntryException newCacheEntryException(String message, Object... args) {
    return newCacheEntryException(null, message, args);
  }

  /**
   * Constructs a new {@link CacheEntryException} initialized with the given {@link String message}
   * to describe the exception along with a {@link Throwable cause} used as the reason why the exception was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link CacheEntryException}.
   * @see org.cp.elements.data.caching.CacheEntryException
   */
  public static CacheEntryException newCacheEntryException(Throwable cause, String message, Object... args) {
    return new CacheEntryException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link CacheEntryNotFoundException} initialized with the given {@link String message}
   * to describe the exception.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link CacheEntryNotFoundException}.
   * @see #newCacheEntryNotFoundException(Throwable, String, Object...)
   * @see org.cp.elements.data.caching.CacheEntryNotFoundException
   */
  public static CacheEntryNotFoundException newCacheEntryNotFoundException(String message, Object... args) {
    return newCacheEntryNotFoundException(null, message, args);
  }

  /**
   * Constructs a new {@link CacheEntryNotFoundException} initialized with the given {@link String message}
   * to describe the exception along with a {@link Throwable cause} used as the reason why the exception was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link CacheEntryNotFoundException}.
   * @see org.cp.elements.data.caching.CacheEntryNotFoundException
   */
  public static CacheEntryNotFoundException newCacheEntryNotFoundException(Throwable cause, String message,
      Object... args) {

    return new CacheEntryNotFoundException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link CacheNotFoundException} initialized with the given {@link String message}
   * to describe the {@link CacheNotFoundException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link CacheNotFoundException}.
   * @see #newCacheException(Throwable, String, Object...)
   * @see org.cp.elements.data.caching.CacheNotFoundException
   */
  public static CacheNotFoundException newCacheNotFoundException(String message, Object... args) {
    return newCacheNotFoundException(null, message, args);
  }

  /**
   * Constructs a new {@link CacheNotFoundException} initialized with the given {@link String message}
   * to describe the {@link CacheNotFoundException} along with a {@link Throwable cause} used as the reason
   * why the {@link CacheNotFoundException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link CacheNotFoundException}.
   * @see org.cp.elements.data.caching.CacheNotFoundException
   */
  public static CacheNotFoundException newCacheNotFoundException(Throwable cause, String message, Object... args) {
    return new CacheNotFoundException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link CompressionException} initialized with the given {@link String message}
   * to describe the {@link CompressionException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders
   * in the {@link String message}.
   * @return a new {@link CompressionException}.
   * @see #newCompressionException(Throwable, String, Object...)
   * @see org.cp.elements.data.compression.CompressionException
   */
  public static CompressionException newCompressionException(String message, Object... args) {
    return newCompressionException(null, message, args);
  }

  /**
   * Constructs a new {@link CompressionException} initialized with the given {@link String message}
   * to describe the {@link CompressionException} along with a {@link Throwable cause} used as the reason
   * why the {@link CompressionException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders
   * in the {@link String message}.
   * @return a new {@link CompressionException}.
   * @see org.cp.elements.data.compression.CompressionException
   */
  public static CompressionException newCompressionException(Throwable cause, String message, Object... args) {
    return new CompressionException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link DecompressionException} initialized with the given {@link String message}
   * to describe the {@link DecompressionException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders
   * in the {@link String message}.
   * @return a new {@link DecompressionException}.
   * @see #newDecompressionException(Throwable, String, Object...)
   * @see org.cp.elements.data.compression.DecompressionException
   */
  public static DecompressionException newDecompressionException(String message, Object... args) {
    return newDecompressionException(null, message, args);
  }

  /**
   * Constructs a new {@link DecompressionException} initialized with the given {@link String message}
   * to describe the {@link DecompressionException} along with a {@link Throwable cause} used as the reason
   * why the {@link DecompressionException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders
   * in the {@link String message}.
   * @return a new {@link DecompressionException}.
   * @see org.cp.elements.data.compression.DecompressionException
   */
  public static DecompressionException newDecompressionException(Throwable cause, String message, Object... args) {
    return new DecompressionException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link ConversionException} initialized with the given {@link String message}
   * to describe the {@link ConversionException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ConversionException}.
   * @see #newConversionException(Throwable, String, Object...)
   * @see org.cp.elements.data.conversion.ConversionException
   */
  public static ConversionException newConversionException(String message, Object... args) {
    return newConversionException(null, message, args);
  }

  /**
   * Constructs a new {@link ConversionException} initialized with the given {@link String message}
   * to describe the {@link ConversionException} along with a {@link Throwable cause} used as the reason
   * why the {@link ConversionException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ConversionException}.
   * @see org.cp.elements.data.conversion.ConversionException
   */
  public static ConversionException newConversionException(Throwable cause, String message, Object... args) {
    return new ConversionException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link DeserializationException} initialized with the given {@link String message}
   * to describe the {@link DeserializationException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders
   * in the {@link String message}.
   * @return a new {@link DeserializationException}.
   * @see #newDeserializationException(Throwable, String, Object...)
   * @see org.cp.elements.data.serialization.DeserializationException
   */
  public static DeserializationException newDeserializationException(String message, Object... args) {
    return newDeserializationException(null, message, args);
  }

  /**
   * Constructs a new {@link DeserializationException} initialized with the given {@link String message}
   * to describe the {@link DeserializationException} along with a {@link Throwable cause} used as the reason
   * why the {@link DeserializationException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders
   * in the {@link String message}.
   * @return a new {@link DeserializationException}.
   * @see org.cp.elements.data.serialization.DeserializationException
   */
  public static DeserializationException newDeserializationException(Throwable cause, String message, Object... args) {
    return new DeserializationException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link SerializationException} initialized with the given {@link String message}
   * to describe the {@link SerializationException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders
   * in the {@link String message}.
   * @return a new {@link SerializationException}.
   * @see #newSerializationException(Throwable, String, Object...)
   * @see org.cp.elements.data.serialization.SerializationException
   */
  public static SerializationException newSerializationException(String message, Object... args) {
    return newSerializationException(null, message, args);
  }

  /**
   * Constructs a new {@link SerializationException} initialized with the given {@link String message}
   * to describe the {@link SerializationException} along with a {@link Throwable cause} used as the reason
   * why the {@link SerializationException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders
   * in the {@link String message}.
   * @return a new {@link SerializationException}.
   * @see org.cp.elements.data.serialization.SerializationException
   */
  public static SerializationException newSerializationException(Throwable cause, String message, Object... args) {
    return new SerializationException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link MappingException} initialized with the given {@link String message}
   * to describe the {@link MappingException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link MappingException}.
   * @see #newMappingException(Throwable, String, Object...)
   * @see org.cp.elements.data.mapping.MappingException
   */
  public static MappingException newMappingException(String message, Object... args) {
    return newMappingException(null, message, args);
  }

  /**
   * Constructs a new {@link MappingException} initialized with the given {@link String message}
   * to describe the {@link MappingException} along with a {@link Throwable cause} used as the reason
   * why the {@link MappingException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link MappingException}.
   * @see org.cp.elements.data.mapping.MappingException
   */
  public static MappingException newMappingException(Throwable cause, String message, Object... args) {
    return new MappingException(format(message, args), cause);
  }

  // package org.cp.elements.function

  /**
   * Constructs a new {@link FunctionException} initialized with the given {@link String message}
   * to describe the exception.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link FunctionException}.
   * @see #newFunctionException(Throwable, String, Object...)
   * @see org.cp.elements.function.FunctionException
   */
  public static FunctionException newFunctionException(String message, Object... args) {
    return newFunctionException(null, message, args);
  }

  /**
   * Constructs a new {@link FunctionException} initialized with the given {@link String message}
   * to describe the exception along with a {@link Throwable cause} used as the reason the exception was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link FunctionException}.
   * @see org.cp.elements.function.FunctionException
   */
  public static FunctionException newFunctionException(Throwable cause, String message, Object... args) {
    return new FunctionException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link FunctionExecutionException} initialized with the given {@link String message}
   * to describe the exception.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link FunctionExecutionException}.
   * @see #newFunctionExecutionException(Throwable, String, Object...)
   * @see org.cp.elements.function.FunctionExecutionException
   */
  public static FunctionExecutionException newFunctionExecutionException(String message, Object... args) {
    return newFunctionExecutionException(null, message, args);
  }

  /**
   * Constructs a new {@link FunctionExecutionException} initialized with the given {@link String message}
   * to describe the exception along with a {@link Throwable cause} used as the reason the exception was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link FunctionExecutionException}.
   * @see org.cp.elements.function.FunctionExecutionException
   */
  public static FunctionExecutionException newFunctionExecutionException(Throwable cause, String message,
      Object... args) {

    return new FunctionExecutionException(format(message, args), cause);
  }

  // package org.cp.elements.io

  /**
   * Constructs a new {@link NoSuchDirectoryException} initialized with the given {@link String message}
   * describing the {@link NoSuchDirectoryException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link NoSuchDirectoryException}.
   * @see #newNoSuchDirectoryException(String, Object...)
   * @see org.cp.elements.io.NoSuchDirectoryException
   */
  public static NoSuchDirectoryException newNoSuchDirectoryException(String message, Object... args) {
    return newNoSuchDirectoryException(null, message, args);
  }

  /**
   * Constructs a new {@link NoSuchDirectoryException} initialized with the given {@link String message}
   * describing the {@link NoSuchDirectoryException} along with a {@link Throwable cause} used as the reason
   * why the {@link NoSuchDirectoryException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link NoSuchDirectoryException}.
   * @see org.cp.elements.io.NoSuchDirectoryException
   */
  public static NoSuchDirectoryException newNoSuchDirectoryException(Throwable cause, String message, Object... args) {
    return new NoSuchDirectoryException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link NoSuchFileException} initialized with the given {@link String message}
   * describing the {@link NoSuchFileException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link NoSuchFileException}.
   * @see #newNoSuchFileException(Throwable, String, Object...)
   * @see org.cp.elements.io.NoSuchFileException
   */
  public static NoSuchFileException newNoSuchFileException(String message, Object... args) {
    return newNoSuchFileException(null, message, args);
  }

  /**
   * Constructs a new {@link NoSuchFileException} initialized with the given {@link String message}
   * describing the {@link NoSuchFileException} along with a {@link Throwable cause} used as the reason
   * why the {@link NoSuchFileException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link NoSuchFileException}.
   * @see org.cp.elements.io.NoSuchFileException
   */
  public static NoSuchFileException newNoSuchFileException(Throwable cause, String message, Object... args) {
    return new NoSuchFileException(format(message, args), cause);
  }

  // package org.cp.elements.lang

  /**
   * Constructs a new {@link AssertionException} initialized with the given {@link String message}
   * to describe the {@link AssertionException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link AssertionException}.
   * @see #newAssertionException(Throwable, String, Object...)
   * @see org.cp.elements.lang.AssertionException
   */
  public static AssertionException newAssertionException(String message, Object... args) {
    return newAssertionException(null, message, args);
  }

  /**
   * Constructs a new {@link AssertionException} initialized with the given {@link String message}
   * to describe the {@link AssertionException} along with a {@link Throwable cause} used as the reason
   * why the {@link AssertionException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link AssertionException}.
   * @see org.cp.elements.lang.AssertionException
   */
  public static AssertionException newAssertionException(Throwable cause, String message, Object... args) {
    return new AssertionException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link CloneException} initialized with the given {@link String message}
   * to describe the {@link CloneException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link CloneException}.
   * @see #newCloneException(Throwable, String, Object...)
   * @see org.cp.elements.lang.CloneException
   */
  public static CloneException newCloneException(String message, Object... args) {
    return newCloneException(null, message, args);
  }

  /**
   * Constructs a new {@link CloneException} initialized with the given {@link String message}
   * to describe the {@link CloneException} along with a {@link Throwable cause} used as the reason
   * why the {@link CloneException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link CloneException}.
   * @see org.cp.elements.lang.CloneException
   */
  public static CloneException newCloneException(Throwable cause, String message, Object... args) {
    return new CloneException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link ComparisonException} initialized with the given {@link String message}
   * to describe the {@link ComparisonException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ComparisonException}.
   * @see #newComparisonException(Throwable, String, Object...)
   * @see org.cp.elements.lang.ComparisonException
   */
  public static ComparisonException newComparisonException(String message, Object... args) {
    return newComparisonException(null, message, args);
  }

  /**
   * Constructs a new {@link ComparisonException} initialized with the given {@link String message}
   * to describe the {@link ComparisonException} along with a {@link Throwable cause} used as the reason
   * why the {@link ComparisonException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ComparisonException}.
   * @see org.cp.elements.lang.ComparisonException
   */
  public static ComparisonException newComparisonException(Throwable cause, String message, Object... args) {
    return new ComparisonException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link EqualityException} initialized with the given {@link String message}
   * to describe the {@link EqualityException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link EqualityException}.
   * @see #newEqualityException(Throwable, String, Object...)
   * @see org.cp.elements.lang.EqualityException
   */
  public static EqualityException newEqualityException(String message, Object... args) {
    return newEqualityException(null, message, args);
  }

  /**
   * Constructs a new {@link EqualityException} initialized with the given {@link String message}
   * to describe the {@link EqualityException} along with a {@link Throwable cause} used as the reason
   * why the {@link EqualityException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link EqualityException}.
   * @see org.cp.elements.lang.EqualityException
   */
  public static EqualityException newEqualityException(Throwable cause, String message, Object... args) {
    return new EqualityException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link ExpectedException} initialized with the given {@link String message}
   * to describe the {@link ExpectedException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ExpectedException}.
   * @see #newExpectedException(Throwable, String, Object...)
   * @see ExpectedException
   */
  public static ExpectedException newExpectedException(String message, Object... args) {
    return newExpectedException(null, message, args);
  }

  /**
   * Constructs a new {@link ExpectedException} initialized with the given {@link String message}
   * to describe the {@link ExpectedException} along with a {@link Throwable cause} used as the reason
   * why the {@link ExpectedException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ExpectedException}.
   * @see ExpectedException
   */
  public static ExpectedException newExpectedException(Throwable cause, String message, Object... args) {
    return new ExpectedException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link IdentityException} initialized with the given {@link String message}
   * to describe the {@link IdentityException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link IdentityException}.
   * @see #newIdentityException(Throwable, String, Object...)
   * @see org.cp.elements.lang.IdentityException
   */
  public static IdentityException newIdentityException(String message, Object... args) {
    return newIdentityException(null, message, args);
  }

  /**
   * Constructs a new {@link IdentityException} initialized with the given {@link String message}
   * to describe the {@link IdentityException} along with a {@link Throwable cause} used as the reason
   * why the {@link IdentityException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link IdentityException}.
   * @see org.cp.elements.lang.IdentityException
   */
  public static IdentityException newIdentityException(Throwable cause, String message, Object... args) {
    return new IdentityException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link IllegalTypeException} initialized with the given {@link String message}
   * to describe the {@link IllegalTypeException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link IllegalTypeException}.
   * @see #newIllegalTypeException(Throwable, String, Object...)
   * @see org.cp.elements.lang.IllegalTypeException
   */
  public static IllegalTypeException newIllegalTypeException(String message, Object... args) {
    return newIllegalTypeException(null, message, args);
  }

  /**
   * Constructs a new {@link IllegalTypeException} initialized with the given {@link String message}
   * to describe the {@link IllegalTypeException} along with a {@link Throwable cause} used as the reason
   * why the {@link IllegalTypeException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link IllegalTypeException}.
   * @see org.cp.elements.lang.IllegalTypeException
   */
  public static IllegalTypeException newIllegalTypeException(Throwable cause, String message, Object... args) {
    return new IllegalTypeException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link ImmutableObjectException} initialized with the given {@link String message}
   * to describe the {@link ImmutableObjectException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ImmutableObjectException}.
   * @see #newImmutableObjectException(Throwable, String, Object...)
   * @see org.cp.elements.lang.ImmutableObjectException
   */
  public static ImmutableObjectException newImmutableObjectException(String message, Object... args) {
    return newImmutableObjectException(null, message, args);
  }

  /**
   * Constructs a new {@link ImmutableObjectException} initialized with the given {@link String message}
   * to describe the {@link ImmutableObjectException} along with a {@link Throwable cause} used as the reason
   * why the {@link ImmutableObjectException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ImmutableObjectException}.
   * @see org.cp.elements.lang.ImmutableObjectException
   */
  public static ImmutableObjectException newImmutableObjectException(Throwable cause, String message, Object... args) {
    return new ImmutableObjectException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link InitializationException} initialized with the given {@link String message}
   * to describe the {@link InitializationException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link InitializationException}.
   * @see #newInitializationException(Throwable, String, Object...)
   * @see org.cp.elements.lang.InitializationException
   */
  public static InitializationException newInitializationException(String message, Object... args) {
    return newInitializationException(null, message, args);
  }

  /**
   * Constructs a new {@link InitializationException} initialized with the given {@link String message}
   * to describe the {@link InitializationException} along with a {@link Throwable cause} used as the reason
   * why the {@link InitializationException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link InitializationException}.
   * @see org.cp.elements.lang.InitializationException
   */
  public static InitializationException newInitializationException(Throwable cause, String message, Object... args) {
    return new InitializationException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link ObjectNotFoundException} initialized with the given {@link String message}
   * to describe the {@link ObjectNotFoundException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ObjectNotFoundException}.
   * @see #newObjectNotFoundException(Throwable, String, Object...)
   * @see org.cp.elements.lang.ObjectNotFoundException
   */
  public static ObjectNotFoundException newObjectNotFoundException(String message, Object... args) {
    return newObjectNotFoundException(null, message, args);
  }

  /**
   * Constructs a new {@link ObjectNotFoundException} initialized with the given {@link String message}
   * to describe the {@link ObjectNotFoundException} along with a {@link Throwable cause} used as the reason
   * why the {@link ObjectNotFoundException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ObjectNotFoundException}.
   * @see org.cp.elements.lang.ObjectNotFoundException
   */
  public static ObjectNotFoundException newObjectNotFoundException(Throwable cause, String message, Object... args) {
    return new ObjectNotFoundException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link ResourceNotFoundException} initialized with the given {@link String message}
   * to describe the {@link ResourceNotFoundException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ResourceNotFoundException}.
   * @see #newResourceNotFoundException(Throwable, String, Object...)
   * @see org.cp.elements.lang.ResourceNotFoundException
   */
  public static ResourceNotFoundException newResourceNotFoundException(String message, Object... args) {
    return newResourceNotFoundException(null, message, args);
  }

  /**
   * Constructs a new {@link ResourceNotFoundException} initialized with the given {@link String message}
   * to describe the {@link ResourceNotFoundException} along with a {@link Throwable cause} used as the reason
   * why the {@link ResourceNotFoundException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ResourceNotFoundException}.
   * @see org.cp.elements.lang.ResourceNotFoundException
   */
  public static ResourceNotFoundException newResourceNotFoundException(Throwable cause,
      String message, Object... args) {

    return new ResourceNotFoundException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link ThrowableOperationException} initialized with the given {@link String message}
   * to describe the {@link ThrowableOperationException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ThrowableOperationException}.
   * @see #newThrowableOperationException(Throwable, String, Object...)
   * @see org.cp.elements.lang.ThrowableOperationException
   */
  public static ThrowableOperationException newThrowableOperationException(String message, Object... args) {
    return newThrowableOperationException(null, message, args);
  }

  /**
   * Constructs a new {@link ThrowableOperationException} initialized with the given {@link String message}
   * to describe the {@link ThrowableOperationException} along with a {@link Throwable cause} used as the reason
   * why the {@link ThrowableOperationException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ThrowableOperationException}.
   * @see org.cp.elements.lang.ThrowableOperationException
   */
  public static ThrowableOperationException newThrowableOperationException(Throwable cause,
      String message, Object... args) {

    return new ThrowableOperationException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link TypeNotFoundException} initialized with the given {@link String message}
   * to describe the {@link TypeNotFoundException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link TypeNotFoundException}.
   * @see #newTypeNotFoundException(Throwable, String, Object...)
   * @see org.cp.elements.lang.TypeNotFoundException
   */
  public static TypeNotFoundException newTypeNotFoundException(String message, Object... args) {
    return newTypeNotFoundException(null, message, args);
  }

  /**
   * Constructs a new {@link TypeNotFoundException} initialized with the given {@link String message}
   * to describe the {@link TypeNotFoundException} along with a {@link Throwable cause} used as the reason
   * why the {@link TypeNotFoundException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link TypeNotFoundException}.
   * @see org.cp.elements.lang.TypeNotFoundException
   */
  public static TypeNotFoundException newTypeNotFoundException(Throwable cause, String message, Object... args) {
    return new TypeNotFoundException(format(message, args), cause);
  }

  // package org.cp.elements.lang.factory

  /**
   * Constructs a new {@link NoSuchConstructorException} initialized with the given {@link String message}
   * to describe the {@link NoSuchConstructorException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link NoSuchConstructorException}.
   * @see #newNoSuchConstructorException(Throwable, String, Object...)
   * @see org.cp.elements.lang.factory.NoSuchConstructorException
   */
  public static NoSuchConstructorException newNoSuchConstructorException(String message, Object... args) {
    return newNoSuchConstructorException(null, message, args);
  }

  /**
   * Constructs a new {@link NoSuchConstructorException} initialized with the given {@link String message}
   * to describe the {@link NoSuchConstructorException} along with a {@link Throwable cause} used as the reason
   * why the {@link NoSuchConstructorException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link NoSuchConstructorException}.
   * @see org.cp.elements.lang.factory.NoSuchConstructorException
   */
  public static NoSuchConstructorException newNoSuchConstructorException(Throwable cause,
      String message, Object... args) {

    return new NoSuchConstructorException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link ObjectInstantiationException} initialized with the given {@link String message}
   * to describe the {@link ObjectInstantiationException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ObjectInstantiationException}.
   * @see #newObjectInstantiationException(Throwable, String, Object...)
   * @see org.cp.elements.lang.factory.ObjectInstantiationException
   */
  public static ObjectInstantiationException newObjectInstantiationException(String message, Object... args) {
    return newObjectInstantiationException(null, message, args);
  }

  /**
   * Constructs a new {@link ObjectInstantiationException} initialized with the given {@link String message}
   * to describe the {@link ObjectInstantiationException} along with a {@link Throwable cause} used as the reason
   * why the {@link ObjectInstantiationException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ObjectInstantiationException}.
   * @see org.cp.elements.lang.factory.ObjectInstantiationException
   */
  public static ObjectInstantiationException newObjectInstantiationException(Throwable cause,
      String message, Object... args) {

    return new ObjectInstantiationException(format(message, args), cause);
  }

  // package org.cp.elements.lang.reflect

  /**
   * Constructs a new {@link ConstructorNotFoundException} initialized with the given {@link String message}
   * to describe the {@link ConstructorNotFoundException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ConstructorNotFoundException}.
   * @see #newObjectInstantiationException(Throwable, String, Object...)
   * @see org.cp.elements.lang.reflect.ConstructorNotFoundException
   */
  public static ConstructorNotFoundException newConstructorNotFoundException(String message, Object... args) {
    return newConstructorNotFoundException(null, message, args);
  }

  /**
   * Constructs a new {@link ConstructorNotFoundException} initialized with the given {@link String message}
   * to describe the {@link ConstructorNotFoundException} along with a {@link Throwable cause} used as the reason
   * why the {@link ConstructorNotFoundException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ConstructorNotFoundException}.
   * @see org.cp.elements.lang.reflect.ConstructorNotFoundException
   */
  public static ConstructorNotFoundException newConstructorNotFoundException(Throwable cause,
      String message, Object... args) {

    return new ConstructorNotFoundException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link FieldAccessException} initialized with the given {@link String message}
   * to describe the {@link FieldAccessException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link FieldAccessException}.
   * @see #newFieldAccessException(Throwable, String, Object...)
   * @see org.cp.elements.lang.reflect.FieldAccessException
   */
  public static FieldAccessException newFieldAccessException(String message, Object... args) {
    return newFieldAccessException(null, message, args);
  }

  /**
   * Constructs a new {@link FieldAccessException} initialized with the given {@link String message}
   * to describe the {@link FieldAccessException} along with a {@link Throwable cause} used as the reason
   * why the {@link FieldAccessException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link FieldAccessException}.
   * @see org.cp.elements.lang.reflect.FieldAccessException
   */
  public static FieldAccessException newFieldAccessException(Throwable cause, String message, Object... args) {
    return new FieldAccessException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link FieldNotFoundException} initialized with the given {@link String message}
   * to describe the {@link FieldNotFoundException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link FieldNotFoundException}.
   * @see #newFieldNotFoundException(Throwable, String, Object...)
   * @see org.cp.elements.lang.reflect.FieldNotFoundException
   */
  public static FieldNotFoundException newFieldNotFoundException(String message, Object... args) {
    return newFieldNotFoundException(null, message, args);
  }

  /**
   * Constructs a new {@link FieldNotFoundException} initialized with the given {@link String message}
   * to describe the {@link FieldNotFoundException} along with a {@link Throwable cause} used as the reason
   * why the {@link FieldNotFoundException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link FieldNotFoundException}.
   * @see org.cp.elements.lang.reflect.FieldNotFoundException
   */
  public static FieldNotFoundException newFieldNotFoundException(Throwable cause, String message, Object... args) {
    return new FieldNotFoundException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link MethodInvocationException} initialized with the given {@link String message}
   * to describe the {@link MethodInvocationException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link MethodInvocationException}.
   * @see #newMethodInvocationException(Throwable, String, Object...)
   * @see org.cp.elements.lang.reflect.MethodInvocationException
   */
  public static MethodInvocationException newMethodInvocationException(String message, Object... args) {
    return newMethodInvocationException(null, message, args);
  }

  /**
   * Constructs a new {@link MethodInvocationException} initialized with the given {@link String message}
   * to describe the {@link MethodInvocationException} along with a {@link Throwable cause} used as the reason
   * why the {@link MethodInvocationException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link MethodInvocationException}.
   * @see org.cp.elements.lang.reflect.MethodInvocationException
   */
  public static MethodInvocationException newMethodInvocationException(Throwable cause,
      String message, Object... args) {

    return new MethodInvocationException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link MethodNotFoundException} initialized with the given {@link String message}
   * to describe the {@link MethodNotFoundException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link MethodNotFoundException}.
   * @see #newMethodNotFoundException(Throwable, String, Object...)
   * @see org.cp.elements.lang.reflect.MethodNotFoundException
   */
  public static MethodNotFoundException newMethodNotFoundException(String message, Object... args) {
    return newMethodNotFoundException(null, message, args);
  }

  /**
   * Constructs a new {@link MethodNotFoundException} initialized with the given {@link String message}
   * to describe the {@link MethodNotFoundException} along with a {@link Throwable cause} used as the reason
   * why the {@link MethodNotFoundException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link MethodNotFoundException}.
   * @see org.cp.elements.lang.reflect.MethodNotFoundException
   */
  public static MethodNotFoundException newMethodNotFoundException(Throwable cause, String message, Object... args) {
    return new MethodNotFoundException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link UnhandledMethodInvocationException} initialized with
   * the given {@link String message} to describe the {@link UnhandledMethodInvocationException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link UnhandledMethodInvocationException}.
   * @see #newUnhandledMethodInvocationException(Throwable, String, Object...)
   * @see org.cp.elements.lang.reflect.UnhandledMethodInvocationException
   */
  public static UnhandledMethodInvocationException newUnhandledMethodInvocationException(String message,
      Object... args) {

    return newUnhandledMethodInvocationException(null, message, args);
  }

  /**
   * Constructs a new {@link UnhandledMethodInvocationException} initialized with
   * the given {@link String message} to describe the {@link UnhandledMethodInvocationException} along with
   * a {@link Throwable cause} used as the reason why the {@link UnhandledMethodInvocationException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link UnhandledMethodInvocationException}.
   * @see org.cp.elements.lang.reflect.UnhandledMethodInvocationException
   */
  public static UnhandledMethodInvocationException newUnhandledMethodInvocationException(Throwable cause,
      String message, Object... args) {

    return new UnhandledMethodInvocationException(format(message, args), cause);
  }

  // package org.cp.elements.management

  /**
   * Constructs a new {@link ManagementException} initialized with the given {@link String message}
   * describing the exception.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ManagementException}.
   * @see #newManagementException(Throwable, String, Object...)
   * @see org.cp.elements.management.ManagementException
   */
  public static ManagementException newManagementException(String message, Object... args) {
    return newManagementException(null, message, args);
  }

  /**
   * Constructs a new {@link ManagementException} initialized with the given {@link String message}
   * describing the exception along with a {@link Throwable cause} used as the reason the exception was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ManagementException}.
   * @see org.cp.elements.management.ManagementException
   */
  public static ManagementException newManagementException(Throwable cause, String message, Object... args) {
    return new ManagementException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link MonitoringException} initialized with the given {@link String message}
   * describing the exception.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link MonitoringException}.
   * @see #newMonitoringException(Throwable, String, Object...)
   * @see org.cp.elements.management.MonitoringException
   */
  public static MonitoringException newMonitoringException(String message, Object... args) {
    return newMonitoringException(null, message, args);
  }

  /**
   * Constructs a new {@link MonitoringException} initialized with the given {@link String message}
   * describing the exception along with a {@link Throwable cause} used as the reason the exception was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link MonitoringException}.
   * @see org.cp.elements.management.MonitoringException
   */
  public static MonitoringException newMonitoringException(Throwable cause, String message, Object... args) {
    return new MonitoringException(format(message, args), cause);
  }

  // package org.cp.elements.net

  /**
   * Constructs a new {@link NetworkException} initialized with the given {@link String message}
   * to describe the {@link NetworkException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link NetworkException}.
   * @see #newNetworkException(Throwable, String, Object...)
   * @see org.cp.elements.net.NetworkException
   */
  public static NetworkException newNetworkException(String message, Object... args) {
    return newNetworkException(null, message, args);
  }

  /**
   * Constructs a new {@link NetworkException} initialized with the given {@link String message}
   * to describe the {@link NetworkException} along with a {@link Throwable cause} used as the reason
   * why the {@link NetworkException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link NetworkException}.
   * @see org.cp.elements.net.NetworkException
   */
  public static NetworkException newNetworkException(Throwable cause, String message, Object... args) {
    return new NetworkException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link NoAvailablePortException} initialized with the given {@link String message}
   * to describe the {@link NoAvailablePortException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link NoAvailablePortException}.
   * @see #newNoAvailablePortException(Throwable, String, Object...)
   * @see org.cp.elements.net.NoAvailablePortException
   */
  public static NoAvailablePortException newNoAvailablePortException(String message, Object... args) {
    return newNoAvailablePortException(null, message, args);
  }

  /**
   * Constructs a new {@link NoAvailablePortException} initialized with the given {@link String message}
   * to describe the {@link NoAvailablePortException} along with a {@link Throwable cause} used as the reason
   * why the {@link NoAvailablePortException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link NoAvailablePortException}.
   * @see org.cp.elements.net.NoAvailablePortException
   */
  public static NoAvailablePortException newNoAvailablePortException(Throwable cause, String message, Object... args) {
    return new NoAvailablePortException(format(message, args), cause);
  }

  // package org.cp.elements.process

  /**
   * Constructs a new {@link EmbeddedProcessExecutionException} initialized with
   * the given {@link String message} to describe the {@link EmbeddedProcessExecutionException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link EmbeddedProcessExecutionException}.
   * @see #newEmbeddedProcessExecutionException(Throwable, String, Object...)
   * @see org.cp.elements.process.EmbeddedProcessExecutionException
   */
  public static EmbeddedProcessExecutionException newEmbeddedProcessExecutionException(String message, Object... args) {
    return newEmbeddedProcessExecutionException(null, message, args);
  }

  /**
   * Constructs a new {@link EmbeddedProcessExecutionException} initialized with
   * the given {@link String message} to describe the {@link EmbeddedProcessExecutionException} along with
   * a {@link Throwable cause} used as the reason why the {@link EmbeddedProcessExecutionException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link EmbeddedProcessExecutionException}.
   * @see org.cp.elements.process.EmbeddedProcessExecutionException
   */
  public static EmbeddedProcessExecutionException newEmbeddedProcessExecutionException(Throwable cause,
      String message, Object... args) {

    return new EmbeddedProcessExecutionException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link PidUnknownException} initialized with the given {@link String message}
   * to describe the {@link PidUnknownException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link PidUnknownException}.
   * @see #newPidUnknownException(Throwable, String, Object...)
   * @see org.cp.elements.process.PidUnknownException
   */
  public static PidUnknownException newPidUnknownException(String message, Object... args) {
    return newPidUnknownException(null, message, args);
  }

  /**
   * Constructs a new {@link PidUnknownException} initialized with the given {@link String message}
   * to describe the {@link PidUnknownException} along with a {@link Throwable cause} used as the reason
   * why the {@link PidUnknownException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link PidUnknownException}.
   * @see org.cp.elements.process.PidUnknownException
   */
  public static PidUnknownException newPidUnknownException(Throwable cause, String message, Object... args) {
    return new PidUnknownException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link ProcessException} initialized with the given {@link String message}
   * to describe the {@link ProcessException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ProcessException}.
   * @see #newProcessException(Throwable, String, Object...)
   * @see org.cp.elements.process.ProcessException
   */
  public static ProcessException newProcessException(String message, Object... args) {
    return newProcessException(null, message, args);
  }

  /**
   * Constructs a new {@link ProcessException} initialized with the given {@link String message}
   * to describe the {@link ProcessException} along with a {@link Throwable cause} used as the reason
   * why the {@link ProcessException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ProcessException}.
   * @see org.cp.elements.process.ProcessException
   */
  public static ProcessException newProcessException(Throwable cause, String message, Object... args) {
    return new ProcessException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link ProcessExecutionException} initialized with the given {@link String message}
   * to describe the {@link ProcessExecutionException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ProcessExecutionException}.
   * @see #newProcessExecutionException(Throwable, String, Object...)
   * @see org.cp.elements.process.ProcessExecutionException
   */
  public static ProcessExecutionException newProcessExecutionException(String message, Object... args) {
    return newProcessExecutionException(null, message, args);
  }

  /**
   * Constructs a new {@link ProcessExecutionException} initialized with the given {@link String message}
   * to describe the {@link ProcessExecutionException} along with a {@link Throwable cause} used as the reason
   * why the {@link ProcessExecutionException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ProcessExecutionException}.
   * @see org.cp.elements.process.ProcessExecutionException
   */
  public static ProcessExecutionException newProcessExecutionException(Throwable cause,
      String message, Object... args) {

    return new ProcessExecutionException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link ProcessNotRespondingException} initialized with
   * the given {@link String message} to describe the {@link ProcessNotRespondingException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ProcessNotRespondingException}.
   * @see #newProcessNotRespondingException(Throwable, String, Object...)
   * @see org.cp.elements.process.ProcessNotRespondingException
   */
  public static ProcessNotRespondingException newProcessNotRespondingException(String message, Object... args) {
    return newProcessNotRespondingException(null, message, args);
  }

  /**
   * Constructs a new {@link ProcessNotRespondingException} initialized with
   * the given {@link String message} to describe the {@link ProcessNotRespondingException} along with
   * a {@link Throwable cause} used as the reason why the {@link ProcessNotRespondingException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ProcessNotRespondingException}.
   * @see org.cp.elements.process.ProcessNotRespondingException
   */
  public static ProcessNotRespondingException newProcessNotRespondingException(Throwable cause,
      String message, Object... args) {

    return new ProcessNotRespondingException(format(message, args), cause);
  }

  // package org.cp.elements.security

  /**
   * Constructs a new {@link AuthenticationException} initialized with the given {@link String message}
   * to describe the {@link AuthenticationException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link AuthenticationException}.
   * @see #newAuthenticationException(Throwable, String, Object...)
   * @see org.cp.elements.security.AuthenticationException
   */
  public static AuthenticationException newAuthenticationException(String message, Object... args) {
    return newAuthenticationException(null, message, args);
  }

  /**
   * Constructs a new {@link AuthenticationException} initialized with the given {@link String message}
   * to describe the {@link AuthenticationException} along with a {@link Throwable cause} used as the reason
   * why the {@link AuthenticationException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link AuthenticationException}.
   * @see org.cp.elements.security.AuthenticationException
   */
  public static AuthenticationException newAuthenticationException(Throwable cause, String message, Object... args) {
    return new AuthenticationException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link AuthorizationException} initialized with the given {@link String message}
   * to describe the {@link AuthorizationException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link AuthorizationException}.
   * @see #newAuthorizationException(Throwable, String, Object...)
   * @see org.cp.elements.security.AuthorizationException
   */
  public static AuthorizationException newAuthorizationException(String message, Object... args) {
    return newAuthorizationException(null, message, args);
  }

  /**
   * Constructs a new {@link AuthorizationException} initialized with the given {@link String message}
   * to describe the {@link AuthorizationException} along with a {@link Throwable cause} used as the reason
   * why the {@link AuthorizationException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link AuthorizationException}.
   * @see org.cp.elements.security.AuthorizationException
   */
  public static AuthorizationException newAuthorizationException(Throwable cause, String message, Object... args) {
    return new AuthorizationException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link AbstractSecurityException} initialized with the given {@link String message}
   * to describe the {@link AbstractSecurityException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link AbstractSecurityException}.
   * @see #newSecurityException(Throwable, String, Object...)
   * @see org.cp.elements.security.AbstractSecurityException
   */
  public static AbstractSecurityException newSecurityException(String message, Object... args) {
    return newSecurityException(null, message, args);
  }

  /**
   * Constructs a new {@link AbstractSecurityException} initialized with the given {@link String message}
   * to describe the {@link AbstractSecurityException} along with a {@link Throwable cause} used as the reason
   * why the {@link AbstractSecurityException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link AbstractSecurityException}.
   * @see org.cp.elements.security.AbstractSecurityException
   */
  public static AbstractSecurityException newSecurityException(Throwable cause, String message, Object... args) {
    return new AbstractSecurityException(format(message, args), cause) { };
  }

  // org.cp.elements.security.model

  /**
   * Constructs a new {@link UserNotFoundException} initialized with the given {@link String message}
   * to describe the {@link UserNotFoundException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link UserNotFoundException}.
   * @see #newUserNotFoundException(Throwable, String, Object...)
   * @see org.cp.elements.security.model.UserNotFoundException
   */
  public static UserNotFoundException newUserNotFoundException(String message, Object... args) {
    return newUserNotFoundException(null, message, args);
  }

  /**
   * Constructs a new {@link UserNotFoundException} initialized with the given {@link String message}
   * to describe the {@link UserNotFoundException} along with a {@link Throwable cause} used as the reason
   * why the {@link UserNotFoundException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link UserNotFoundException}.
   * @see org.cp.elements.security.model.UserNotFoundException
   */
  public static UserNotFoundException newUserNotFoundException(Throwable cause, String message, Object... args) {
    return new UserNotFoundException(format(message, args), cause);
  }

  // TODO: Edit Javadoc
  // package org.cp.elements.service

  /**
   * Constructs a new {@link ServiceException} initialized with the given {@link String message}
   * to describe the {@link ServiceException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ServiceException}.
   * @see org.cp.elements.service.ServiceException
   * @see #newServiceException(String, Object...)
   */
  public static ServiceException newServiceException(String message, Object... args) {
    return newServiceException(null, message, args);
  }

  /**
   * Constructs a new {@link ServiceException} initialized with the given {@link String message}
   * to describe the {@link ServiceException} along with a {@link Throwable cause} used as the reason
   * the {@link ServiceInvocationException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ServiceException}.
   * @see org.cp.elements.service.ServiceException
   */
  public static ServiceException newServiceException(Throwable cause, String message, Object... args) {
    return new ServiceException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link ServiceInvocationException} initialized with the given {@link String message}
   * to describe the {@link ServiceInvocationException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ServiceInvocationException}.
   * @see #newServiceInvocationException(Throwable, String, Object...)
   * @see org.cp.elements.service.ServiceInvocationException
   */
  public static ServiceInvocationException newServiceInvocationException(String message, Object... args) {
    return newServiceInvocationException(null, message, args);
  }

  /**
   * Constructs a new {@link ServiceInvocationException} initialized with the given {@link String message}
   * to describe the {@link ServiceInvocationException} along with a {@link Throwable cause} used as the reason
   * the {@link ServiceInvocationException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ServiceInvocationException}.
   * @see org.cp.elements.service.ServiceInvocationException
   */
  public static ServiceInvocationException newServiceInvocationException(Throwable cause,
      String message, Object... args) {

    return new ServiceInvocationException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link ServiceNotProvidedException} initialized with the given {@link String message}
   * to describe the {@link ServiceNotProvidedException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ServiceNotProvidedException}.
   * @see #newServiceInvocationException(Throwable, String, Object...)
   * @see org.cp.elements.service.provider.ServiceNotProvidedException
   */
  public static ServiceNotProvidedException newServiceNotProvidedException(String message, Object... args) {
    return newServiceNotProvidedException(null, message, args);
  }

  /**
   * Constructs a new {@link ServiceNotProvidedException} initialized with the given {@link String message}
   * to describe the {@link ServiceNotProvidedException} along with a {@link Throwable cause} used as the reason
   * the {@link ServiceNotProvidedException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ServiceNotProvidedException}.
   * @see org.cp.elements.service.provider.ServiceNotProvidedException
   */
  public static ServiceNotProvidedException newServiceNotProvidedException(Throwable cause,
      String message, Object... args) {

    return new ServiceNotProvidedException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link ServiceUnavailableException} initialized with the given {@link String message}
   * to describe the {@link ServiceUnavailableException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ServiceUnavailableException}.
   * @see org.cp.elements.service.ServiceUnavailableException
   * @see #newServiceUnavailableException(String, Object...)
   */
  public static ServiceUnavailableException newServiceUnavailableException(String message, Object... args) {
    return newServiceUnavailableException(null, message, args);
  }

  /**
   * Constructs a new {@link ServiceUnavailableException} initialized with the given {@link String message}
   * to describe the {@link ServiceUnavailableException} along with a {@link Throwable cause} used as the reason
   * the {@link ServiceUnavailableException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ServiceUnavailableException}.
   * @see org.cp.elements.service.ServiceUnavailableException
   */
  public static ServiceUnavailableException newServiceUnavailableException(Throwable cause,
      String message, Object... args) {

    return new ServiceUnavailableException(format(message, args), cause);
  }

  // package org.cp.elements.test

  /**
   * Constructs a new {@link FailedTestException} initialized with the given {@link String message}
   * to describe the {@link FailedTestException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link FailedTestException}.
   * @see #newFailedTestException(Throwable, String, Object...)
   * @see org.cp.elements.test.FailedTestException
   */
  public static FailedTestException newFailedTestException(String message, Object... args) {
    return newFailedTestException(null, message, args);
  }

  /**
   * Constructs a new {@link FailedTestException} initialized with the given {@link String message}
   * to describe the {@link FailedTestException} along with a {@link Throwable cause} used as the reason
   * why the {@link FailedTestException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link FailedTestException}.
   * @see org.cp.elements.test.FailedTestException
   */
  public static FailedTestException newFailedTestException(Throwable cause, String message, Object... args) {
    return new FailedTestException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link HungTestException} initialized with the given {@link String message}
   * to describe the {@link HungTestException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link HungTestException}.
   * @see #newHungTestException(Throwable, String, Object...)
   * @see org.cp.elements.test.HungTestException
   */
  public static HungTestException newHungTestException(String message, Object... args) {
    return newHungTestException(null, message, args);
  }

  /**
   * Constructs a new {@link HungTestException} initialized with the given {@link String message}
   * to describe the {@link HungTestException} along with a {@link Throwable cause} used as the reason
   * why the {@link HungTestException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link HungTestException}.
   * @see org.cp.elements.test.HungTestException
   */
  public static HungTestException newHungTestException(Throwable cause, String message, Object... args) {
    return new HungTestException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link TestException} initialized with the given {@link String message}
   * to describe the {@link TestException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link TestException}.
   * @see #newTestException(Throwable, String, Object...)
   * @see org.cp.elements.test.TestException
   */
  public static TestException newTestException(String message, Object... args) {
    return newTestException(null, message, args);
  }

  /**
   * Constructs a new {@link TestException} initialized with the given {@link String message}
   * to describe the {@link TestException} along with a {@link Throwable cause} used as the reason
   * why the {@link TestException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link TestException}.
   * @see org.cp.elements.test.TestException
   */
  public static TestException newTestException(Throwable cause, String message, Object... args) {
    return new TestException(format(message, args), cause);
  }

  // package org.cp.elements.text

  /**
   * Constructs a new {@link FormatException} initialized with the given {@link String message}
   * to describe the {@link FormatException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link FormatException}.
   * @see #newFormatException(Throwable, String, Object...)
   * @see org.cp.elements.text.FormatException
   */
  public static FormatException newFormatException(String message, Object... args) {
    return newFormatException(null, message, args);
  }

  /**
   * Constructs a new {@link FormatException} initialized with the given {@link String message}
   * to describe the {@link FormatException} along with a {@link Throwable cause} used as the reason
   * why the {@link FormatException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link FormatException}.
   * @see org.cp.elements.text.FormatException
   */
  public static FormatException newFormatException(Throwable cause, String message, Object... args) {
    return new FormatException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link ParseException} initialized with the given {@link String message}
   * to describe the {@link ParseException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ParseException}.
   * @see #newParseException(Throwable, String, Object...)
   * @see org.cp.elements.text.ParseException
   */
  public static ParseException newParseException(String message, Object... args) {
    return newParseException(null, message, args);
  }

  /**
   * Constructs a new {@link ParseException} initialized with the given {@link String message}
   * to describe the {@link ParseException} along with a {@link Throwable cause} used as the reason
   * why the {@link ParseException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ParseException}.
   * @see org.cp.elements.text.ParseException
   */
  public static ParseException newParseException(Throwable cause, String message, Object... args) {
    return new ParseException(format(message, args), cause);
  }

  // package org.cp.elements.util

  /**
   * Constructs a new {@link ApplicationException} initialized with the given {@link String message}
   * to describe the {@link ApplicationException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ApplicationException}.
   * @see #newApplicationException(Throwable, String, Object...)
   * @see org.cp.elements.util.ApplicationException
   */
  public static ApplicationException newApplicationException(String message, Object... args) {
    return newApplicationException(null, message, args);
  }

  /**
   * Constructs a new {@link ApplicationException} initialized with the given {@link String message}
   * to describe the {@link ApplicationException} along with a {@link Throwable cause} used as the reason
   * why the {@link ApplicationException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ApplicationException}.
   * @see org.cp.elements.util.ApplicationException
   */
  public static ApplicationException newApplicationException(Throwable cause, String message, Object... args) {
    return new ApplicationException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link LoserException} initialized with the given {@link String message}
   * to describe the {@link LoserException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link LoserException}.
   * @see #newLoserException(Throwable, String, Object...)
   * @see org.cp.elements.util.LoserException
   */
  public static LoserException newLoserException(String message, Object... args) {
    return newLoserException(null, message, args);
  }

  /**
   * Constructs a new {@link LoserException} initialized with the given {@link String message}
   * to describe the {@link LoserException} along with a {@link Throwable cause} used as the reason
   * why the {@link LoserException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link LoserException}.
   * @see org.cp.elements.util.LoserException
   */
  public static LoserException newLoserException(Throwable cause, String message, Object... args) {
    return new LoserException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link ReadOnlyException} initialized with the given {@link String message}
   * to describe the {@link ReadOnlyException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ReadOnlyException}.
   * @see #newReadOnlyException(Throwable, String, Object...)
   * @see org.cp.elements.util.ReadOnlyException
   */
  public static ReadOnlyException newReadOnlyException(String message, Object... args) {
    return newReadOnlyException(null, message, args);
  }

  /**
   * Constructs a new {@link ReadOnlyException} initialized with the given {@link String message}
   * to describe the {@link ReadOnlyException} along with a {@link Throwable cause} used as the reason
   * why the {@link ReadOnlyException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link ReadOnlyException}.
   * @see org.cp.elements.util.ReadOnlyException
   */
  public static ReadOnlyException newReadOnlyException(Throwable cause, String message, Object... args) {
    return new ReadOnlyException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link SystemException} initialized with the given {@link String message}
   * to describe the {@link SystemException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link SystemException}.
   * @see #newReadOnlyException(Throwable, String, Object...)
   * @see org.cp.elements.util.SystemException
   */
  public static SystemException newSystemException(String message, Object... args) {
    return newSystemException(null, message, args);
  }

  /**
   * Constructs a new {@link SystemException} initialized with the given {@link String message}
   * to describe the {@link SystemException} along with a {@link Throwable cause} used as the reason
   * why the {@link SystemException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link SystemException}.
   * @see org.cp.elements.util.SystemException
   */
  public static SystemException newSystemException(Throwable cause, String message, Object... args) {
    return new SystemException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link UndeclaredPropertyException} initialized with the given {@link String message}
   * to describe the {@link UndeclaredPropertyException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link UndeclaredPropertyException}.
   * @see #newUndeclaredPropertyException(String, Object...)
   * @see org.cp.elements.util.UndefinedPropertyException
   */
  public static UndeclaredPropertyException newUndeclaredPropertyException(String message, Object... args) {
    return newUndeclaredPropertyException(null, message, args);
  }

  /**
   * Constructs a new {@link UndeclaredPropertyException} initialized with the given {@link String message}
   * to describe the {@link UndeclaredPropertyException} along with a {@link Throwable cause} used as the reason
   * why the {@link UndeclaredPropertyException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link UndeclaredPropertyException}.
   * @see org.cp.elements.util.UndeclaredPropertyException
   */
  public static UndeclaredPropertyException newUndeclaredPropertyException(Throwable cause, String message,
      Object... args) {

    return new UndeclaredPropertyException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link UndefinedPropertyException} initialized with the given {@link String message}
   * to describe the {@link UndefinedPropertyException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link UndefinedPropertyException}.
   * @see #newUndefinedPropertyException(Throwable, String, Object...)
   * @see org.cp.elements.util.UndefinedPropertyException
   */
  public static UndefinedPropertyException newUndefinedPropertyException(String message, Object... args) {
    return newUndefinedPropertyException(null, message, args);
  }

  /**
   * Constructs a new {@link UndefinedPropertyException} initialized with the given {@link String message}
   * to describe the {@link UndefinedPropertyException} along with a {@link Throwable cause} used as the reason
   * why the {@link UndefinedPropertyException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link UndefinedPropertyException}.
   * @see org.cp.elements.util.UndefinedPropertyException
   */
  public static UndefinedPropertyException newUndefinedPropertyException(Throwable cause, String message,
      Object... args) {

    return new UndefinedPropertyException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link UserException} initialized with the given {@link String message}
   * to describe the {@link UserException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link UserException}.
   * @see #newUserException(Throwable, String, Object...)
   * @see org.cp.elements.util.UserException
   */
  public static UserException newUserException(String message, Object... args) {
    return newUserException(null, message, args);
  }

  /**
   * Constructs a new {@link UserException} initialized with the given {@link String message}
   * to describe the {@link UserException} along with a {@link Throwable cause} used as the reason
   * why the {@link UserException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link UserException}.
   * @see org.cp.elements.util.UserException
   */
  public static UserException newUserException(Throwable cause, String message, Object... args) {
    return new UserException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link WriteOnlyException} initialized with the given {@link String message}
   * to describe the {@link WriteOnlyException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link WriteOnlyException}.
   * @see #newWriteOnlyException(Throwable, String, Object...)
   * @see org.cp.elements.util.WriteOnlyException
   */
  public static WriteOnlyException newWriteOnlyException(String message, Object... args) {
    return newWriteOnlyException(null, message, args);
  }

  /**
   * Constructs a new {@link WriteOnlyException} initialized with the given {@link String message}
   * to describe the {@link WriteOnlyException} along with a {@link Throwable cause} used as the reason
   * why the {@link WriteOnlyException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link WriteOnlyException}.
   * @see org.cp.elements.util.WriteOnlyException
   */
  public static WriteOnlyException newWriteOnlyException(Throwable cause, String message, Object... args) {
    return new WriteOnlyException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link PageNotFoundException} initialized with the given {@link String message}
   * to describe the {@link PageNotFoundException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link PageNotFoundException}.
   * @see #newPageNotFoundException(Throwable, String, Object...)
   * @see org.cp.elements.util.paging.PageNotFoundException
   */
  public static PageNotFoundException newPageNotFoundException(String message, Object... args) {
    return newPageNotFoundException(null, message, args);
  }

  /**
   * Constructs a new {@link PageNotFoundException} initialized with the given {@link String message}
   * to describe the {@link PageNotFoundException} along with a {@link Throwable cause} used as the reason
   * why the {@link PageNotFoundException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link PageNotFoundException}.
   * @see org.cp.elements.util.paging.PageNotFoundException
   */
  public static PageNotFoundException newPageNotFoundException(Throwable cause, String message, Object... args) {
    return new PageNotFoundException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link SearchException} initialized with the given {@link String message}
   * to describe the {@link SearchException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link SearchException}.
   * @see #newSearchException(Throwable, String, Object...)
   * @see org.cp.elements.util.search.SearchException
   */
  public static SearchException newSearchException(String message, Object... args) {
    return newSearchException(null, message, args);
  }

  /**
   * Constructs a new {@link SearchException} initialized with the given {@link String message}
   * to describe the {@link SearchException} along with a {@link Throwable cause} used as the reason
   * why the {@link SearchException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link SearchException}.
   * @see org.cp.elements.util.search.SearchException
   */
  public static SearchException newSearchException(Throwable cause, String message, Object... args) {
    return new SearchException(format(message, args), cause);
  }

  /**
   * Constructs a new {@link SortException} initialized with the given {@link String message}
   * to describe the {@link SortException}.
   *
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link SortException}.
   * @see #newSortException(Throwable, String, Object...)
   * @see org.cp.elements.util.sort.SortException
   */
  public static SortException newSortException(String message, Object... args) {
    return newSortException(null, message, args);
  }

  /**
   * Constructs a new {@link SortException} initialized with the given {@link String message}
   * to describe the {@link SortException} along with a {@link Throwable cause} used as the reason
   * why the {@link SortException} was thrown.
   *
   * @param cause {@link Throwable} used as the cause of the exception.
   * @param message {@link String} containing a message to describe the exception.
   * @param args optional array of {@link Object arguments} used to replace the placeholders in the message.
   * @return a new {@link SortException}.
   * @see org.cp.elements.util.sort.SortException
   */
  public static SortException newSortException(Throwable cause, String message, Object... args) {
    return new SortException(format(message, args), cause);
  }
}
