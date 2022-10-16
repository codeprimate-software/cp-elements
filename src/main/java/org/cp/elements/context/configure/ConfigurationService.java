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
package org.cp.elements.context.configure;

import static org.cp.elements.lang.ElementsExceptionsFactory.newConfigurationException;

import java.util.Arrays;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BiFunction;
import java.util.function.Supplier;
import java.util.stream.StreamSupport;

import org.cp.elements.context.annotation.ActiveProfiles;
import org.cp.elements.context.configure.annotation.ConfigurationProperties;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.service.annotation.Service;
import org.cp.elements.service.loader.ServiceLoaderSupport;
import org.cp.elements.util.ArrayUtils;

/**
 * {@link Service} interface defining a contract for Java programs (applications) requiring configuration
 * at launch and runtime.
 *
 * @author John Blum
 * @see java.lang.Iterable
 * @see org.cp.elements.context.annotation.ActiveProfiles
 * @see org.cp.elements.context.annotation.Profile
 * @see org.cp.elements.context.configure.Configuration
 * @see org.cp.elements.context.configure.annotation.ConfigurationProperties
 * @see org.cp.elements.service.annotation.Service
 * @see org.cp.elements.service.loader.ServiceLoaderSupport
 * @since 1.0.0
 */
@Service
public interface ConfigurationService extends Iterable<Configuration> {

  AtomicReference<Loader> LOADER_REFERENCE = new AtomicReference<>(null);

  String ACTIVE_PROFILES_PROPERTY = "elements.configuration.profiles";

  /**
   * Gets a reference to the {@link ConfigurationService.Loader} used to load
   * the {@literal Service Provider Implementation (SPI)} of this {@link ConfigurationService}.
   *
   * @return a reference to the {@link ConfigurationService.Loader} used to load
   * the {@literal Service Provider Implementation (SPI)} of this {@link ConfigurationService}.
   * @see org.cp.elements.context.configure.ConfigurationService.Loader
   */
  static @NotNull Loader getLoader() {
    return LOADER_REFERENCE.updateAndGet(it -> it != null ? it : new ConfigurationService.Loader() { });
  }

  /**
   * Determines whether a configuration property identified by the given {@link String name} is declared
   * in the composite {@link Configuration} aggregated by this {@link ConfigurationService}.
   *
   * A configuration property may be declared (present) but not defined (set) with a {@link String value}.
   *
   * By default, this method simply determines whether the configuration property has been {@link #isSet(String)}.
   *
   * @param propertyName {@link String} containing the {@literal name} of the configuration property to evaluate.
   * @return a boolean value indicating whether the configuration property identified by the given {@link String name}
   * is declared in the composite {@link Configuration} aggregated by this {@link ConfigurationService}.
   * @see #isSet(String)
   */
  default boolean isPresent(@Nullable String propertyName) {
    return isSet(propertyName);
  }

  /**
   * Determines whether a configuration property identified by the given {@link String name} is defined (set)
   * in the composite {@link Configuration} aggregated by this {@link ConfigurationService}.
   *
   * If a configuration property is defined (set) then it also implies that the configuration property
   * is also declared (present) in the composite {@link Configuration} aggregated by this {@link ConfigurationService}.
   *
   * @param propertyName {@link String} containing the {@literal name} of the confiugration property to evaluate.
   * @return a boolean value indicating whether the configuration property identified by the given {@link String name}
   * is declared in the composite {@link Configuration} aggregated by this {@link ConfigurationService}.
   * @see org.cp.elements.context.configure.Configuration#isSet(String)
   * @see #spliterator()
   */
  default boolean isSet(@Nullable String propertyName) {

    return StringUtils.hasText(propertyName)
      && StreamSupport.stream(this.spliterator(), false)
      .filter(Objects::nonNull)
      .anyMatch(configuration -> configuration.isSet(propertyName));
  }

  /**
   * Gets an array of {@link String profile names} that are active for this {@link ConfigurationService}.
   *
   * @return an array of {@link String profile names} that are active for this {@link ConfigurationService}.
   * @see org.cp.elements.context.annotation.ActiveProfiles
   */
  default String[] getActiveProfiles() {

    if (isPresent(ACTIVE_PROFILES_PROPERTY)) {
      return StringUtils.toStringArray(getPropertyValue(ACTIVE_PROFILES_PROPERTY, Configuration.NOT_REQUIRED));
    }
    else  {

      Class<?> type = getClass();

      return type.isAnnotationPresent(ActiveProfiles.class)
        ? Arrays.stream(ArrayUtils.nullSafeArray(type.getAnnotation(ActiveProfiles.class).names(), String.class))
        .filter(StringUtils::hasText)
        .toArray(String[]::new)
        : StringUtils.EMPTY_STRING_ARRAY;
    }
  }

  /**
   * Returns a {@link BiFunction} determining the behavior of the {@link #getPropertyValue(String, boolean)}
   * and {@link #getPropertyValueAs(String, Class, boolean)} methods when a configuration property
   * is neither set nor present, but required.
   *
   * @param <T> {@link Class type} of the configuration property {@link Object value}.
   * @return a {@link BiFunction} determining the behavior of the {@link #getPropertyValue(String, boolean)}
   * and {@link #getPropertyValueAs(String, Class, boolean)} methods when a configuration property
   * is neither set nor present, but required.
   * @see java.util.function.BiFunction
   *
   */
  default @NotNull <T> BiFunction<String, Boolean, T> getNonPresentNonSetPropertyHandler() {

    return (propertyName, required) -> {
      Assert.isFalse(required, newConfigurationException("Property [%s] not found", propertyName));
      return null;
    };
  }

  /**
   * Gets the {@link String value} of the configuration property identified by the given {@link String name}.
   *
   * @param propertyName {@link String} containing the {@literal name} of the configuration property.
   * @return the {@link String value} of the {@link String named} configuration property.
   * @throws ConfigurationException if the configuration property is undeclared (not present) and undefined (not set).
   * @see #getPropertyValue(String, boolean)
   */
  default @NotNull String getPropertyValue(String propertyName) {
    return getPropertyValue(propertyName, Configuration.REQUIRED);
  }

  /**
   * Gets the {@link String value} of the configuration property identified by the given {@link String name}.
   *
   * The {@code required} parameter is used to specify whether the {@link String named} configuration property
   * is {@literal required} to be declared (present) and defined (set) in at least one of the {@link Configuration}
   * objects aggregated by this {@link ConfigurationService}.
   *
   * The {@link String value} from the first {@link Configuration} object to declare (present) and define (set)
   * the {@link String name} configuration property is returned.
   *
   * @param propertyName {@link String} containing the {@literal name} of the configuration property.
   * @param required boolean value specifying whether the configuration property must be declared (present)
   * and defined (set) in the aggregate configuration.
   * @return the {@link String value} of the {@link String named} configuration property; May return {@literal null}
   * if the {@link String named} configuration property is not {@literal required}.
   * @throws ConfigurationException when the configuration property is {@literal required}
   * and the property is undeclared (not present) and undefined (not set).
   * @see org.cp.elements.context.configure.Configuration#getPropertyValue(String, boolean)
   * @see org.cp.elements.context.configure.Configuration#isPresent(String)
   * @see #getNonPresentNonSetPropertyHandler()
   * @see #spliterator()
   */
  default String getPropertyValue(String propertyName, boolean required) {

    return StreamSupport.stream(this.spliterator(), false)
      .filter(Objects::nonNull)
      .filter(configuration -> configuration.isPresent(propertyName))
      .findFirst()
      .map(configuration -> configuration.getPropertyValue(propertyName, required))
      .orElseGet(() -> this.<String>getNonPresentNonSetPropertyHandler().apply(propertyName, required));
  }

  /**
   * Gets the {@link String value} of the configuration property identified by the given {@link String name}
   * or the {@link String default value} if the configuration property is not declared (present) or not defined (set).
   *
   * When given a {@link String default value}, even if {@literal null}, then the configuration property
   * is not {@literal required}, and therefore, this method has the same outcome as calling
   * {@link #getPropertyValue(String, boolean)} with a {@literal false} argument
   * for the {@literal required} parameter.
   *
   * @param propertyName {@link String} containing the {@literal name} of the configuration property.
   * @param defaultValue {@link String} containing the {@literal default value} returned if the configuration property
   * is not declared (present) or not defined (set).
   * @return the {@link String value} of the configuration property identified by the given {@link String name}
   * or the {@link String default value} if the configuration property is not declared (present) or not defined (set).
   * @see #getPropertyValue(String, boolean)
   */
  default @Nullable String getPropertyValue(String propertyName, @Nullable String defaultValue) {

    return Optional.ofNullable(getPropertyValue(propertyName, Configuration.NOT_REQUIRED))
      .filter(StringUtils::hasText)
      .orElse(defaultValue);
  }

  /**
   * Gets the {@link String value} of the configuration property identified by the given {@link String name}
   * or the {@link Supplier default value} if the configuration property is not declared (present) or not defined (set).
   *
   * When given a {@link Supplier default value}, even if {@literal null}, then the configuration property
   * is not {@literal required}, and therefore, this method has the same outcome as calling
   * {@link #getPropertyValue(String, boolean)} with a {@literal false} argument
   * for the {@literal required} parameter.
   *
   * @param propertyName {@link String} containing the {@literal name} of the configuration property.
   * @param defaultValue {@link Supplier} containing the {@literal default value} returned if the configuration property
   * is not declared (present) or not defined (set); must not be {@literal null}.
   * @return the {@link String value} of the configuration property identified by the given {@link String name}
   * or the {@link Supplier default value} if the configuration property is not declared (present) or not defined (set).
   * @see #getPropertyValue(String, boolean)
   */
  default @Nullable String getPropertyValue(String propertyName, @NotNull Supplier<String> defaultValue) {

    return Optional.ofNullable(getPropertyValue(propertyName, Configuration.NOT_REQUIRED))
      .filter(StringUtils::hasText)
      .orElseGet(defaultValue);
  }

  /**
   * Gets the {@link Object value} of the configuration property identified by the given {@link String name}
   * as an instance of the given {@link Class type T}.
   *
   * @param <T> converted {@link Class type} of the {@link String returned value} for the configuration property.
   * @param propertyName {@link String} containing the {@literal name} of the configuration property.
   * @param type converted {@link Class type} expected for the {@link String value} of the configuration property.
   * @return the {@link Object value} of the {@link String named} configuration property.
   * @throws ConfigurationException if the configuration property is undeclared (not present) and undefined (not set).
   * @see #getPropertyValueAs(String, Class, boolean)
   */
  default @NotNull <T> T getPropertyValueAs(String propertyName, Class<T> type) {
    return getPropertyValueAs(propertyName, type, Configuration.REQUIRED);
  }

  /**
   * Gets the {@link Object value} of the configuration property identified by the given {@link String name}
   * as an instance of {@link Class type T}.
   *
   * The {@code required} parameter is used to specify whether the {@link String named} configuration property
   * is {@literal required} to be declared (present) and defined (set) in at least one of the {@link Configuration}
   * objects aggregated by this {@link ConfigurationService}.
   *
   * The {@link Object value} from the first {@link Configuration} object to declare (present) and define (set)
   * the {@link String name} configuration property is returned.
   *
   * @param <T> converted {@link Class type} of the {@link String returned value} for the configuration property.
   * @param propertyName {@link String} containing the {@literal name} of the configuration property.
   * @param type converted {@link Class type} expected for the {@link String value} of the configuration property.
   * @param required boolean value specifying whether the configuration property must be declared (present)
   * and defined (set) in the aggregate configuration.
   * @return the {@link Object value} of the {@link String named} configuration property as an instance of
   * {@link Class type T}; May return {@literal null} if the {@link String named} configuration property
   * is not {@literal required}.
   * @throws ConfigurationException when the configuration property is {@literal required}
   * and the property is undeclared (not present) and undefined (not set).
   * @see org.cp.elements.context.configure.Configuration#getPropertyValueAs(String, Class, boolean)
   * @see org.cp.elements.context.configure.Configuration#isPresent(String)
   * @see #getNonPresentNonSetPropertyHandler()
   * @see #spliterator()
   */
  default <T> T getPropertyValueAs(String propertyName, Class<T> type, boolean required) {

    return StreamSupport.stream(this.spliterator(), false)
      .filter(Objects::nonNull)
      .filter(configuration -> configuration.isPresent(propertyName))
      .findFirst()
      .map(configuration -> configuration.getPropertyValueAs(propertyName, type, required))
      .orElseGet(() -> this.<T>getNonPresentNonSetPropertyHandler().apply(propertyName, required));
  }

  /**
   * Gets the {@link Object value} of the configuration property identified by the given {@link String name}
   * as an instance of {@link Class type T} or gets the {@link Object default value} if the configuration property
   * is not declared (present) or not defined (set).
   *
   * When given a {@link Object default value}, even if {@literal null}, then the configuration property
   * is not {@literal required}, and therefore, this method has the same outcome as calling
   * {@link #getPropertyValueAs(String, Class, boolean)} with a {@literal false} argument
   * for the {@literal required} parameter.
   *
   * @param <T> converted {@link Class type} of the {@link Object returned value} for the configuration property.
   * @param propertyName {@link String} containing the {@literal name} of the configuration property.
   * @param type converted {@link Class type} expected for the {@link Object value} of the configuration property.
   * @param defaultValue {@link Object} containing the {@literal default value} returned if the configuration property
   * is not declared (present) or not defined (set).
   * @return the {@link Object value} of the {@link String named} configuration property as an instance of
   * {@link Class type T} or the {@link Object default value} if the configuration property
   * is not declared (present) or not defined (set).
   * @see #getPropertyValueAs(String, Class, boolean)
   */
  default <T> T getPropertyValueAs(String propertyName, Class<T> type, T defaultValue) {
    return Optional.ofNullable(getPropertyValueAs(propertyName, type, Configuration.NOT_REQUIRED)).orElse(defaultValue);
  }

  /**
   * Gets the {@link Object value} of the configuration property identified by the given {@link String name}
   * as an instance of {@link Class type T} or gets the {@link Supplier default value} if the configuration property
   * is not declared (present) or not defined (set).
   *
   * When given a {@link Supplier default value}, even if {@literal null}, then the configuration property
   * is not {@literal required}, and therefore, this method has the same outcome as calling
   * {@link #getPropertyValueAs(String, Class, boolean)} with a {@literal false} argument
   * for the {@literal required} parameter.
   *
   * @param <T> converted {@link Class type} of the {@link Object returned value} for the configuration property.
   * @param propertyName {@link String} containing the {@literal name} of the configuration property.
   * @param type converted {@link Class type} expected for the {@link Object value} of the configuration property.
   * @param defaultValue {@link Supplier} containing the {@literal default value} returned if the configuration property
   * is not declared (present) or not defined (set).
   * @return the {@link Object value} of the {@link String named} configuration property as an instance of
   * {@link Class type T} or the {@link Supplier default value} if the configuration property
   * is not declared (present) or not defined (set).
   * @see #getPropertyValueAs(String, Class, boolean)
   * @see java.util.function.Supplier
   */
  default @Nullable <T> T getPropertyValueAs(String propertyName, Class<T> type, @NotNull Supplier<T> defaultValue) {
    return Optional.ofNullable(getPropertyValueAs(propertyName, type, Configuration.NOT_REQUIRED))
      .orElseGet(defaultValue);
  }

  /**
   * Proxies the given [{@link ConfigurationProperties}] {@link Class interface} used to return property values
   * sourced from the {@link Configuration} objects aggregated by this {@link ConfigurationService}.
   *
   * The {@link Class interface} based {@literal Proxy} is an object-oriented, and convenient method for accessing
   * the configuration of a Java program (application) using properties. The user-defined {@link Class interface type}
   * may optionally be annotated with Elements' {@link ConfigurationProperties} annotation declaring a property prefix
   * used to fully-qualify all property names.
   *
   * For example, a user may define the following application configuration interface:
   *
   * <pre>
   * <code>
   * &#64;ConfigurationProperties(propertyPrefix="jdbc")
   * interface JdbcConfiguration {
   *     String getDriverClassName();
   *     String getUrl();
   *     String getUsername();
   *     String getPassword();
   * }
   * </code>
   * </pre>
   *
   * This configuration interface would enable access to the following configuration properties in a Java program
   * (application) properties file:
   *
   * <pre>
   * <code>
   * # application.properties
   * jdbc.driver-class-name=com.mysql.jdbc.Driver
   * jdbc.url=jdbc:mysql://localhost:3306/AppDatabase
   * jdbc.username=jonDoe
   * jdbc.password=s3cr3t
   * </code>
   * </pre>
   *
   * Then, given a reference to this {@link ConfigurationService} the user application code would do the following:
   *
   * <pre>
   * <code>
   *   JdbcConfiguration jdbcConfiguration =
   *       configurationService.proxy(JdbcConfiguration.class);
   *
   *   assertThat(jdbcConfiguration.getUsername()).isEqualTo("jonDoe");
   * </code>
   * </pre>
   *
   * By default, this {@literal proxy(..)} method will create a {@literal JDK dynamic proxy}.
   *
   * @param <T> {@link Class type} of the {@literal interface} and {@literal Proxy} to return.
   * @param interfaceType {@link Class interface type} to proxy.
   * @return a {@literal Proxy} used to access configuration properties of the Java program (application)
   * in an Object-oriented way.
   * @see org.cp.elements.context.configure.annotation.ConfigurationProperties
   * @see java.lang.Class
   */
  <T> T proxy(Class<T> interfaceType);

  /**
   * Registers the given {@link Configuration} with this {@link ConfigurationService}.
   *
   * @param configuration {@link Configuration} to register.
   * @return a boolean indicating whether the registration of the given {@link Configuration} was successful.
   * @see org.cp.elements.context.configure.Configuration
   * @see #unregister(Configuration)
   */
  boolean register(Configuration configuration);

  /**
   * Unregisters the given {@link Configuration} from this {@link ConfigurationService}.
   *
   * @param configuration {@link Configuration} to unregister.
   * @return a boolean indicating whether the un-registration of the given {@link Configuration} was successful.
   * @see org.cp.elements.context.configure.Configuration
   * @see #register(Configuration)
   */
  boolean unregister(Configuration configuration);

  /**
   * {@link ServiceLoaderSupport} implementation used to load the {@link ConfigurationService}
   * provider implementation (SPI).
   *
   * @see org.cp.elements.service.loader.ServiceLoaderSupport
   */
  interface Loader extends ServiceLoaderSupport<ConfigurationService> {

    @Override
    default Class<ConfigurationService> getType() {
      return ConfigurationService.class;
    }
  }
}
