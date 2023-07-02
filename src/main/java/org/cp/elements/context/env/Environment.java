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
package org.cp.elements.context.env;

import static org.cp.elements.io.FileSystemUtils.newFile;

import java.io.File;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.function.Supplier;

import org.cp.elements.io.FileUtils;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.Version;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.util.MapUtils;
import org.cp.elements.util.PropertiesAdapter;
import org.cp.elements.util.PropertiesBuilder;

/**
 * The {@link Environment} class is a representation of the system environment
 * in which the software application is running.
 *
 * @author John Blum
 * @see java.lang.Iterable
 * @see java.lang.System
 * @see java.util.Map
 * @see java.util.Properties
 * @see org.cp.elements.util.MapUtils
 * @see org.cp.elements.util.PropertiesAdapter
 * @see org.cp.elements.util.PropertiesBuilder
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class Environment implements Iterable<String> {

  public static final String JAVA_CLASS_PATH = "java.class.path";
  public static final String JAVA_HOME = "java.home";
  public static final String JAVA_LIBRARY_PATH = "java.library.path";
  public static final String JAVA_VENDOR = "java.vendor";
  public static final String JAVA_VERSION = "java.version";

  public static final String JVM_NAME = "java.vm.name";
  public static final String JVM_VENDOR = "java.vm.vendor";
  public static final String JVM_VERSION = "java.vm.version";

  public static final String OS_ARCHITECTURE = "os.arch";
  public static final String OS_NAME = "os.name";
  public static final String OS_VERSION = "os.version";

  public static final String PATH = "PATH";
  public static final String PATH_SEPARATOR = "path.separator";

  public static final String USER_DIRECTORY = "user.dir";
  public static final String USER_HOME = "user.home";
  public static final String USER_NAME = "user.name";

  /**
   * Factory method used to construct a new instance of {@link Environment} initialized from the given associative array
   * containing the initial system {@literal environment variable} configuration.
   *
   * @param associativeArray {@link String array} of key/value mappings of the form:
   * {@literal [ "keyOne=valueOne", "keyTwo=valueTwo", ..., "keyN=valueN" ]}.
   * @return a new {@link Environment} initialized with the given associative array.
   * @see org.cp.elements.util.PropertiesBuilder#fromAssociativeArray(String[])
   * @see #Environment(PropertiesAdapter)
   */
  public static @NotNull Environment from(@NotNull String[] associativeArray) {
    return new Environment(PropertiesBuilder.fromAssociativeArray(associativeArray).buildPropertiesAdapter());
  }

  /**
   * Factory method used to construct a new instance of {@link Environment} initialized from the given,
   * required {@link Map} containing the initial system {@literal environment variable} configuration.
   *
   * @param map {@link Map} used to initialize the {@link Environment}; must not be {@literal null}.
   * @return a new {@link Environment} initialized with the given, required {@link Map}.
   * @throws IllegalArgumentException if the {@link Map} is {@literal null}.
   * @see org.cp.elements.util.PropertiesBuilder#from(Map)
   * @see #Environment(PropertiesAdapter)
   * @see java.util.Map
   */
  public static @NotNull Environment from(@NotNull Map<String, String> map) {
    return new Environment(PropertiesBuilder.from(map).buildPropertiesAdapter());
  }

  /**
   * Factory method used to construct a new instance of {@link Environment} initialized from the given,
   * required {@link Properties} containing the initial system {@literal environment variable} configuration.
   *
   * @param properties {@link Properties} used to initialize the {@link Environment}; must not be {@literal null}.
   * @return a new {@link Environment} initialized with the given, required {@link Properties}.
   * @throws IllegalArgumentException if the {@link Properties} are {@literal null}.
   * @see org.cp.elements.util.PropertiesAdapter#from(Properties)
   * @see #Environment(PropertiesAdapter)
   * @see java.util.Properties
   */
  public static @NotNull Environment from(@NotNull Properties properties) {
    return new Environment(PropertiesAdapter.from(properties));
  }

  /**
   * Factory method used to construct a new instance of {@link Environment} initialized from
   * the {@link System#getenv() System environment variables}.
   *
   * @return a new {@link Environment} initialized with {@link System#getenv() System environment variables}.
   * @see org.cp.elements.util.PropertiesBuilder#fromEnvironmentVariables()
   * @see #Environment(PropertiesAdapter)
   * @see java.lang.System#getenv()
   */
  public static @NotNull Environment fromEnvironmentVariables() {
    return new Environment(PropertiesBuilder.fromEnvironmentVariables().buildPropertiesAdapter());
  }

  private final PropertiesAdapter environmentVariables;
  private final PropertiesAdapter systemProperties;

  /**
   * Constructs a new instance of {@link Environment} initialized with the given, required {@link PropertiesAdapter}
   * containing the {@literal environment variable} configuration of this system.
   * <p>
   * Additionally, the {@link Environment} initializes a reference to
   * the configured Java {@link System#getProperties() System properties}.
   *
   * @param environmentVariables {@link PropertiesAdapter} containing the initial system {@literal environment variable}
   * configuration; must not be {@literal null}.
   * @throws IllegalArgumentException if the {@link PropertiesAdapter} is {@literal null}.
   * @see org.cp.elements.util.PropertiesAdapter
   */
  protected Environment(@NotNull PropertiesAdapter environmentVariables) {

    this.environmentVariables =
      ObjectUtils.requireObject(environmentVariables, "An initial environment is required");

    this.systemProperties = SystemPropertiesAdapter.INSTANCE;
  }

  /**
   * Returns a reference to the configured {@link PropertiesAdapter} containing
   * {@link System#getenv() System environment variables}.
   *
   * @return the configured instance of {@link PropertiesAdapter} containing
   * {@link System#getenv() System environment variables}.
   * @see org.cp.elements.util.PropertiesAdapter
   * @see java.lang.System#getenv()
   * @see #systemProperties()
   */
  protected @NotNull PropertiesAdapter environmentVariables() {
    return this.environmentVariables;
  }

  /**
   * Returns a reference to the configured {@link PropertiesAdapter} containing
   * Java {@link System#getProperties() System properties}.
   *
   * @return the configured instance of {@link PropertiesAdapter} containing Java
   * {@link System#getProperties() System properties}.
   * @see org.cp.elements.util.PropertiesAdapter
   * @see java.lang.System#getProperties()
   * @see #environmentVariables()
   */
  protected @NotNull PropertiesAdapter systemProperties() {
    return this.systemProperties;
  }

  /**
   * Determines whether this {@link Environment} contains any variables.
   *
   * @return a boolean value indicating whether this {@link Environment} contains any variables.
   * @see #environmentVariables()
   */
  public boolean isEmpty() {
    return environmentVariables().isEmpty();
  }

  /**
   * Determines whether the {@literal environment variable} identified by the given {@link String name}
   * is declared in this {@link Environment}.
   * <p>
   * Even though the {@link String named} {@literal environment variable} is present (exists, or is declared)
   * does not mean the {@literal environment variable} is set (defined). See {@link #isSet(String)}.
   *
   * @param environmentVariableName {@link String} containing the {@literal name} of the environment variable.
   * @return a boolean value indicating whether the {@literal environment variable} identified by
   * the given {@link String name} is present (exists; is declared) in this {@link Environment}.
   * @see org.cp.elements.util.PropertiesAdapter#isPresent(String)
   * @see #environmentVariables()
   * @see #isSet(String)
   */
  @NullSafe
  public boolean isPresent(@Nullable String environmentVariableName) {
    return StringUtils.hasText(environmentVariableName) && environmentVariables().isPresent(environmentVariableName);
  }

  /**
   * Determines whether the {@literal environment variable} identified by the given {@link String name}
   * is set (defined) in this {@link Environment}.
   *
   * @param environmentVariableName {@link String} containing the {@literal name} of the environment variable.
   * @return a boolean value indicating whether the {@literal environment variable} identified by
   * the given {@link String name} is set (defined) in this {@link Environment}.
   * @see org.cp.elements.util.PropertiesAdapter#isSet(String)
   * @see #environmentVariables()
   * @see #isPresent(String)
   */
  @NullSafe
  public boolean isSet(@Nullable String environmentVariableName) {
    return StringUtils.hasText(environmentVariableName) && environmentVariables().isSet(environmentVariableName);
  }

  /**
   * Copies the contents of this {@link Environment} into the given, required {@link Map}.
   *
   * @param map {@link Map} in which to copy the contents of this {@link Environment};
   * must not be {@literal null}.
   * @return the given {@link Map}.
   * @throws IllegalArgumentException if the {@link Map} is {@literal null}.
   * @see #environmentVariables()
   * @see #copyTo(Properties)
   * @see java.util.Map
   */
  public @NotNull Map<String, String> copyTo(@NotNull Map<String, String> map) {

    ObjectUtils.requireObject(map, "The Map to copy to is required")
      .putAll(environmentVariables().toMap());

    return map;
  }

  /**
   * Copies the contents of this {@link Environment} into the given, required {@link Properties}.
   *
   * @param properties {@link Properties} in which to copy the contents of this {@link Environment};
   * must not be {@literal null}.
   * @return the given {@link Properties}.
   * @throws IllegalArgumentException if the {@link Properties} object is {@literal null}.
   * @see #environmentVariables()
   * @see java.util.Properties
   * @see #copyTo(Map)
   */
  public @NotNull Properties copyTo(@NotNull Properties properties) {

    ObjectUtils.requireObject(properties, "The Properties object to copy to is required")
      .putAll(environmentVariables().toProperties());

    return properties;
  }

  /**
   * Get the {@link String value} of the {@link String named} {@link System#getProperty(String) System property}.
   *
   * @param propertyName {@link String} containing the {@literal name}
   * of the {@link System#getProperty(String) System property}; must not be {@literal null} or {@literal empty}.
   * @return the {@link String value} of the {@link String named} {@link System#getProperty(String) System property}.
   * @throws IllegalArgumentException if the {@link String name} of the property is {@literal null} or {@literal empty}.
   * @see org.cp.elements.util.PropertiesAdapter#get(String)
   * @see #systemProperties()
   */
  public @Nullable String getProperty(@NotNull String propertyName) {
    return systemProperties().get(propertyName);
  }

  /**
   * Get the {@link String value} of the {@link String named} {@link System#getProperty(String) System property},
   * or return the given {@link String default value} if the {@link String named}
   * {@link System#getProperty(String) System property} is not declared or defined (set).
   *
   * @param propertyName {@link String} containing the {@literal name}
   * of the {@link System#getProperty(String) System property}; must not be {@literal null} or {@literal empty}.
   * @param defaultValue {@link String value} to return if the {@link String named}
   * {@link System#getProperty(String) System property} is not declared or defined (set).
   * @return the {@link String value} of the {@link String named} {@link System#getProperty(String) System property}
   * @throws IllegalArgumentException if the {@link String name} of the property is {@literal null} or {@literal empty}.
   * or return the given {@link String default value} if the {@link String named}
   * {@link System#getProperty(String) System property} is not declared or defined (set).
   * @see org.cp.elements.util.PropertiesAdapter#get(String, String)
   * @see #systemProperties()
   */
  public @Nullable String getProperty(@NotNull String propertyName, @Nullable String defaultValue) {
    return systemProperties().get(propertyName, defaultValue);
  }

  /**
   * Get the {@link String value} of the {@link String named} {@link System#getProperty(String) System property},
   * or return the given {@link Supplier default value} if the {@link String named}
   * {@link System#getProperty(String) System property} is not declared or defined (set).
   *
   * @param propertyName {@link String} containing the {@literal name}
   * of the {@link System#getProperty(String) System property}; must not be {@literal null} or {@literal empty}.
   * @param defaultValue {@link Supplier} supplying the {@link String default value} to return
   * if the {@link String named} {@link System#getProperty(String) System property} is not declared or defined (set).
   * @return the {@link String value} of the {@link String named} {@link System#getProperty(String) System property}
   * @throws IllegalArgumentException if the {@link String name} of the property is {@literal null} or {@literal empty}.
   * or return the given {@link String default value} if the {@link String named}
   * {@link System#getProperty(String) System property} is not declared or defined (set).
   * @see org.cp.elements.util.PropertiesAdapter#get(String, Supplier)
   * @see java.util.function.Supplier
   * @see #systemProperties()
   */
  public @Nullable String getProperty(@NotNull String propertyName, @NotNull Supplier<String> defaultValue) {
    return systemProperties().get(propertyName, defaultValue);
  }

  /**
   * Get the {@link T value} of the {@link String named} {@link System#getProperty(String) System property}
   * as an instance of the given, required {@link Class type T}.
   *
   * @param <T> {@link Class type} of the returned property {@link Object value}.
   * @param propertyName {@link String} containing the {@literal name}
   * of the {@link System#getProperty(String) System property}; must not be {@literal null} or {@literal empty}.
   * @param type converted {@link Class type} of the property value; must not be {@literal null}.
   * @return the {@link T value} of the {@link String named} {@link System#getProperty(String) System property}.
   * @throws IllegalArgumentException if the {@link String name} of the property is {@literal null} or {@literal empty},
   * or the {@link Class type} is {@literal null}.
   * @see org.cp.elements.util.PropertiesAdapter#getAsType(String, Class)
   * @see #systemProperties()
   */
  public @Nullable <T> T getPropertyAsType(@NotNull String propertyName, @NotNull Class<T> type) {
    return systemProperties().getAsType(propertyName, type);
  }

  /**
   * Get the {@link T value} of the {@link String named} {@link System#getProperty(String) System property},
   * as an instance of the given, required {@link Class type T}, or return the given {@link T default value}
   * if the {@link String named} {@link System#getProperty(String) System property} is not declared or defined (set).
   *
   * @param <T> {@link Class type} of the returned property {@link Object value}.
   * @param propertyName {@link String} containing the {@literal name}
   * of the {@link System#getProperty(String) System property}; must not be {@literal null} or {@literal empty}.
   * @param type converted {@link Class type} of the property value; must not be {@literal null}.
   * @param defaultValue value of {@link Class type T} to return if the {@link String named}
   * {@link System#getProperty(String) System property} is not declared or defined (set).
   * @return the {@link T value} of the {@link String named} {@link System#getProperty(String) System property}
   * @throws IllegalArgumentException if the {@link String name} of the property is {@literal null} or {@literal empty},
   * or the {@link Class type} is {@literal null}.
   * or return the given {@link T default value} if the {@link String named}
   * {@link System#getProperty(String) System property} is not declared or defined (set).
   * @see org.cp.elements.util.PropertiesAdapter#getAsType(String, Class, Object)
   * @see #systemProperties()
   */
  public @Nullable <T> T getPropertyAsType(@NotNull String propertyName, @NotNull Class<T> type,
      @Nullable T defaultValue) {

    return systemProperties().getAsType(propertyName, type, defaultValue);
  }

  /**
   * Get the {@link T value} of the {@link String named} {@link System#getProperty(String) System property},
   * as an instance of the given, required {@link Class type T}, or return the given, required
   * {@link Supplier supplied default value} if the {@link String named}
   * {@link System#getProperty(String) System property} is not declared or defined (set).
   *
   * @param <T> {@link Class type} of the returned property {@link Object value}.
   * @param propertyName {@link String} containing the {@literal name}
   * of the {@link System#getProperty(String) System property}; must not be {@literal null} or {@literal empty}.
   * @param type converted {@link Class type} of the property value; must not be {@literal null}.
   * @param defaultValue {@link Supplier supplied default value} of {@link Class type T} to return
   * if the {@link String named} {@link System#getProperty(String) System property} is not declared or defined (set).
   * @return the {@link T value} of the {@link String named} {@link System#getProperty(String) System property}
   * @throws IllegalArgumentException if the {@link String name} of the property is {@literal null} or {@literal empty},
   * the {@link Class type} is {@literal null}, or the {@link Supplier} is {@literal null}.
   * or return the given {@link Supplier supplied default value}
   * if the {@link String named} {@link System#getProperty(String) System property} is not declared or defined (set).
   * @see org.cp.elements.util.PropertiesAdapter#getAsType(String, Class, Supplier)
   * @see #systemProperties()
   */
  public @Nullable <T> T getPropertyAsType(@NotNull String propertyName, @NotNull Class<T> type,
      @NotNull Supplier<T> defaultValue) {

    return systemProperties().getAsType(propertyName, type, defaultValue);
  }

  /**
   * Get the {@link String value} of the {@link String named} {@link System#getenv(String) Environment Variable}.
   *
   * @param environmentVariableName {@link String} containing the {@literal name}
   * of the {@link System#getenv(String) Environment Variable}; must not be {@literal null} or {@literal empty}.
   * @return the {@link String value} of the {@link String named} {@link System#getenv(String) Environment Variable}.
   * @throws IllegalArgumentException if the {@link String name} of the environment variable is {@literal null}
   * or {@literal empty}.
   * @see org.cp.elements.util.PropertiesAdapter#get(String)
   * @see #environmentVariables()
   */
  public @Nullable String getVariable(@NotNull String environmentVariableName) {
    return environmentVariables().get(environmentVariableName);
  }

  /**
   * Get the {@link String value} of the {@link String named} {@link System#getenv(String) Environment Variable},
   * or return the given {@link String default value} if the {@link String named}
   * {@link System#getenv(String) Environment Variable} is not declared or defined (set).
   *
   * @param environmentVariableName {@link String} containing the {@literal name}
   * of the {@link System#getenv(String) Environment Variable}; must not be {@literal null} or {@literal empty}.
   * @param defaultValue {@link String value} to return if the {@link String named}
   * {@link System#getenv(String) Environment Variable} is not declared or defined (set).
   * @return the {@link String value} of the {@link String named} {@link System#getenv(String) Environment Variable}
   * @throws IllegalArgumentException if the {@link String name} of the environment variable is {@literal null}
   * or {@literal empty}.
   * or return the given {@link String default value} if the {@link String named}
   * {@link System#getenv(String) Environment Variable} is not declared or defined (set).
   * @see org.cp.elements.util.PropertiesAdapter#get(String, String)
   * @see #environmentVariables()
   */
  public @Nullable String getVariable(@NotNull String environmentVariableName, @Nullable String defaultValue) {
    return environmentVariables().get(environmentVariableName, defaultValue);
  }

  /**
   * Get the {@link String value} of the {@link String named} {@link System#getenv(String) Environment Variable},
   * or return the given {@link Supplier default value} if the {@link String named}
   * {@link System#getenv(String) Environment Variable} is not declared or defined (set).
   *
   * @param environmentVariableName {@link String} containing the {@literal name}
   * of the {@link System#getenv(String) Environment Variable}; must not be {@literal null} or {@literal empty}.
   * @param defaultValue {@link Supplier} supplying the {@link String default value} to return
   * if the {@link String named} {@link System#getenv(String) Environment Variable} is not declared or defined (set).
   * @return the {@link String value} of the {@link String named} {@link System#getenv(String) Environment Variable}
   * @throws IllegalArgumentException if the {@link String name} of the environment variable is {@literal null}
   * or {@literal empty}.
   * or return the given {@link String default value} if the {@link String named}
   * {@link System#getenv(String) Environment Variable} is not declared or defined (set).
   * @see org.cp.elements.util.PropertiesAdapter#get(String, Supplier)
   * @see java.util.function.Supplier
   * @see #environmentVariables()
   */
  public @Nullable String getVariable(@NotNull String environmentVariableName, @NotNull Supplier<String> defaultValue) {
    return environmentVariables().get(environmentVariableName, defaultValue);
  }

  /**
   * Get the {@link T value} of the {@link String named} {@link System#getenv(String) Environment Variable}
   * as an instance of the given, required {@link Class type T}.
   *
   * @param <T> {@link Class type} of the returned variable {@link Object value}.
   * @param environmentVariableName {@link String} containing the {@literal name}
   * of the {@link System#getenv(String) Environment Variable}; must not be {@literal null} or {@literal empty}.
   * @param type converted {@link Class type} of the environment variable value; must not be {@literal null}.
   * @return the {@link T value} of the {@link String named} {@link System#getenv(String) Environment Variable}.
   * @throws IllegalArgumentException if the {@link String name} of the environment variable is {@literal null}
   * or {@literal empty}, or the {@link Class type} is {@literal null}.
   * @see org.cp.elements.util.PropertiesAdapter#getAsType(String, Class)
   * @see #environmentVariables()
   */
  public @Nullable <T> T getVariableAsType(@NotNull String environmentVariableName, @NotNull Class<T> type) {
    return environmentVariables().getAsType(environmentVariableName, type);
  }

  /**
   * Get the {@link T value} of the {@link String named} {@link System#getenv(String) Environment Variable}
   * as an instance of the given, required {@link Class type T}, or returns the given {@link T default value}
   * if the {@link String named} {@link System#getenv(String) Environment Variable} is not declared or defined (set).
   *
   * @param <T> {@link Class type} of the returned variable {@link Object value}.
   * @param environmentVariableName {@link String} containing the {@literal name}
   * of the {@link System#getenv(String) Environment Variable}; must not be {@literal null} or {@literal empty}.
   * @param type converted {@link Class type} of the environment variable value; must not be {@literal null}.
   * @param defaultValue value of {@link Class type T} to return if the {@link String named}
   * {@link System#getenv(String) Environment Variable} is not declared or defined (set).
   * @return the {@link T value} of the {@link String named} {@link System#getenv(String) Environment Variable}
   * @throws IllegalArgumentException if the {@link String name} of the environment variable is {@literal null}
   * or {@literal empty}, or the {@link Class type} is {@literal null}.
   * returning the given {@link T default value} if the {@link String named}
   * {@link System#getenv(String) Environment Variable} is not declared or defined (set).
   * @see org.cp.elements.util.PropertiesAdapter#getAsType(String, Class, Object)
   * @see #environmentVariables()
   */
  public @Nullable <T> T getVariableAsType(@NotNull String environmentVariableName, @NotNull Class<T> type,
      @Nullable T defaultValue) {

    return environmentVariables().getAsType(environmentVariableName, type, defaultValue);
  }

  /**
   * Get the {@link T value} of the {@link String named} {@link System#getenv(String) Environment Variable}
   * as an instance of the given, required {@link Class type T}, or returns the given, required
   * {@link Supplier suppolied default value}
   * if the {@link String named} {@link System#getenv(String) Environment Variable} is not declared or defined (set).
   *
   * @param <T> {@link Class type} of the returned variable {@link Object value}.
   * @param environmentVariableName {@link String} containing the {@literal name}
   * of the {@link System#getenv(String) Environment Variable}; must not be {@literal null} or {@literal empty}.
   * @param type converted {@link Class type} of the environment variable value; must not be {@literal null}.
   * @param defaultValue {@link Supplier supplied default value} of {@link Class type T} to return
   * if the {@link String named} {@link System#getenv(String) Environment Variable} is not declared or defined (set).
   * @return the {@link T value} of the {@link String named} {@link System#getenv(String) Environment Variable}
   * @throws IllegalArgumentException if the {@link String name} of the environment variable is {@literal null}
   * or {@literal empty}, the {@link Class type} is {@literal null}, or the {@link Supplier} is {@literal null}.
   * returning the given {@link Supplier supplied default value}
   * if the {@link String named} {@link System#getenv(String) Environment Variable} is not declared or defined (set).
   * @see org.cp.elements.util.PropertiesAdapter#getAsType(String, Class, Supplier)
   * @see #environmentVariables()
   */
  public @Nullable <T> T getVariableAsType(@NotNull String environmentVariableName, @NotNull Class<T> type,
      @NotNull Supplier<T> defaultValue) {

    return environmentVariables().getAsType(environmentVariableName, type, defaultValue);
  }

  /**
   * Gets the value of the {@literal java.class.path} {@link System#getProperty(String) System property}.
   *
   * @return the value of the {@literal java.class.path} {@link System#getProperty(String) System property}.
   * @see java.lang.System#getProperty(String)
   * @see #systemProperties()
   */
  public String getJavaClassPath() {
    return systemProperties().get(JAVA_CLASS_PATH);
  }

  /**
   * Gets the value of the {@literal java.home} {@link System#getProperty(String) System property}.
   *
   * @return the value of the {@literal java.home} {@link System#getProperty(String) System property}.
   * @see java.lang.System#getProperty(String)
   * @see #systemProperties()
   * @see java.io.File
   */
  public File getJavaHome() {
    return FileUtils.newFile(systemProperties().get(JAVA_HOME));
  }

  /**
   * Gets the value of the {@literal java.library.path} {@link System#getProperty(String) System property}.
   *
   * @return the value of the {@literal java.library.path} {@link System#getProperty(String) System property}.
   * @see java.lang.System#getProperty(String)
   * @see #systemProperties()
   */
  public String getJavaLibraryPath() {
    return systemProperties().get(JAVA_LIBRARY_PATH);
  }

  /**
   * Gets the value of the {@literal java.vendor} {@link System#getProperty(String) System property}.
   *
   * @return the value of the {@literal java.vendor} {@link System#getProperty(String) System property}.
   * @see java.lang.System#getProperty(String)
   * @see #systemProperties()
   */
  public String getJavaVendor() {
    return systemProperties().get(JAVA_VENDOR);
  }

  /**
   * Gets the value of the {@literal java.version} {@link System#getProperty(String) System property}.
   *
   * @return the value of the {@literal java.version} {@link System#getProperty(String) System property}.
   * @see java.lang.System#getProperty(String)
   * @see org.cp.elements.lang.Version
   * @see #systemProperties()
   */
  public Version getJavaVersion() {
    return Version.parse(systemProperties().get(JAVA_VERSION));
  }

  /**
   * Gets the value of the {@literal java.vm.name} {@link System#getProperty(String) System property}.
   *
   * @return the value of the {@literal java.vm.name} {@link System#getProperty(String) System property}.
   * @see java.lang.System#getProperty(String)
   * @see #systemProperties()
   */
  public String getJvmName() {
    return systemProperties().get(JVM_NAME);
  }

  /**
   * Gets the value of the {@literal java.vm.vendor} {@link System#getProperty(String) System property}.
   *
   * @return the value of the {@literal java.vm.vendor} {@link System#getProperty(String) System property}.
   * @see java.lang.System#getProperty(String)
   * @see #systemProperties()
   */
  public String getJvmVendor() {
    return systemProperties().get(JVM_VENDOR);
  }

  /**
   * Gets the value of the {@literal java.vm.version} {@link System#getProperty(String) System property}.
   *
   * @return the value of the {@literal java.vm.version} {@link System#getProperty(String) System property}.
   * @see java.lang.System#getProperty(String)
   * @see org.cp.elements.lang.Version
   * @see #systemProperties()
   */
  public Version getJvmVersion() {
    return Version.parse(systemProperties().get(JVM_VERSION));
  }

  /**
   * Gets the value of the {@literal os.arch} {@link System#getProperty(String) System property}.
   *
   * @return the value of the {@literal os.arch} {@link System#getProperty(String) System property}.
   * @see java.lang.System#getProperty(String)
   * @see #systemProperties()
   */
  public String getOperatingSystemArchitecture() {
    return systemProperties().get(OS_ARCHITECTURE);
  }

  /**
   * Gets the value of the {@literal os.name} {@link System#getProperty(String) System property}.
   *
   * @return the value of the {@literal os.name} {@link System#getProperty(String) System property}.
   * @see java.lang.System#getProperty(String)
   * @see #systemProperties()
   */
  public String getOperatingSystemName() {
    return systemProperties().get(OS_NAME);
  }

  /**
   * Gets the value of the {@literal os.version} {@link System#getProperty(String) System property}.
   *
   * @return the value of the {@literal os.version} {@link System#getProperty(String) System property}.
   * @see java.lang.System#getProperty(String)
   * @see org.cp.elements.lang.Version
   * @see #systemProperties()
   */
  public Version getOperatingSystemVersion() {
    return Version.parse(systemProperties().get(OS_VERSION));
  }

  /**
   * Gets the value of the {@literal path.separator} {@link System#getProperty(String) System property}.
   *
   * @return the value of the {@literal path.separator} {@link System#getProperty(String) System property}.
   * @see java.lang.System#getProperty(String)
   * @see #systemProperties()
   */
  public String getPathSeparator() {
    return systemProperties().get(PATH_SEPARATOR);
  }

  /**
   * Gets the value of the {@literal PATH} {@link System#getenv(String)} environment variable}.
   *
   * @return the value of the {@literal PATH} {@link System#getenv(String)} environment variable}.
   * @see java.lang.System#getenv(String)
   * @see #environmentVariables()
   */
  public String getSystemPath() {
    return environmentVariables().get(PATH);
  }

  /**
   * Gets the value of the {@literal user.dir} {@link System#getProperty(String) System property}.
   * <p>
   * The value of the {@literal user.dir} {@link System#getProperty(String) System property} is the same as
   * the current working directory.
   *
   * @return the value of the {@literal user.dir} {@link System#getProperty(String) System property}.
   * @see java.lang.System#getProperty(String)
   * @see #systemProperties()
   * @see java.io.File
   */
  public File getUserDirectory() {
    return newFile(systemProperties().get(USER_DIRECTORY));
  }

  /**
   * Gets the value of the {@literal user.home} {@link System#getProperty(String) System property}.
   *
   * @return the value of the {@literal user.home} {@link System#getProperty(String) System property}.
   * @see java.lang.System#getProperty(String)
   * @see #systemProperties()
   * @see java.io.File
   */
  public File getUserHome() {
    return newFile(systemProperties().get(USER_HOME));
  }

  /**
   * Gets the value of the {@literal user.name} {@link System#getProperty(String) System property}.
   *
   * @return the value of the {@literal user.name} {@link System#getProperty(String) System property}.
   * @see java.lang.System#getProperty(String)
   * @see #systemProperties()
   */
  public String getUserName() {
    return systemProperties().get(USER_NAME);
  }

  /**
   * Returns an {@link Iterator} over the environment variables configured in
   * the {@link System#getenv() System environment}.
   *
   * @return an {@link Iterator} over the environment variables configured in
   * the {@link System#getenv() System environment}.
   * @see #environmentVariables()
   * @see java.util.Iterator
   */
  @Override
  public Iterator<String> iterator() {
    return environmentVariables().iterator();
  }

  /**
   * Returns the {@link Integer number of variables} declared in this {@link Environment}.
   *
   * @return the {@link Integer number of variables} declared in this {@link Environment}.
   * @see #environmentVariables()
   */
  public int size() {
    return environmentVariables().size();
  }

  /**
   * Determines whether this {@link Environment} is equal to the given {@link Object}.
   *
   * @param obj {@link Object} to evaluate for equality.
   * @return a boolean value indicating whether this {@link Environment} is equal to the given {@link Object}.
   * @see java.lang.Object#equals(Object)
   */
  @Override
  public boolean equals(Object obj) {

    if (this == obj) {
      return true;
    }

    if (!(obj instanceof Environment)) {
      return false;
    }

    Environment that = (Environment) obj;

    return this.environmentVariables().equals(that.environmentVariables());
  }

  /**
   * Computes the {@link Object#hashCode() hash code} of this {@link Environment}.
   *
   * @return the computed {@link Object#hashCode() hash code} of this {@link Environment}.
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode() {
    return ObjectUtils.hashCodeOf(environmentVariables());
  }

  /**
   * Returns a {@link String} representation (view) of this {@link Environment}.
   *
   * @return a {@link String} describing this {@link Environment}.
   * @see org.cp.elements.util.PropertiesAdapter#toString()
   */
  @Override
  public String toString() {
    return environmentVariables().toString();
  }

  /**
   * Converts this {@link Environment} into an {@link String associative array}.
   * <p>
   * An associative array is of the form:
   * <p>
   * [
   *   environmentVariableOne = value,
   *   environmentVariableTwo = value,
   *   ...
   *   environmentVariableN = value
   * ]
   *
   * @return an {@link String associate array} from this {@link Environment}.
   * @see #toProperties()
   * @see #toMap()
   */
  public @NotNull String[] toAssociativeArray() {
    return MapUtils.toAssociativeArray(toMap());
  }

  /**
   * Converts this {@link Environment} into a {@link Map}.
   *
   * @return a {@link Map} from this {@link Environment}
   * @see #toAssociativeArray()
   * @see #toProperties()
   * @see java.util.Map
   */
  public @NotNull Map<String, String> toMap() {
    return environmentVariables().toMap();
  }

  /**
   * Converts this {@link Environment} into a {@link Properties} object.
   *
   * @return a {@link Properties} object from this {@link Environment}.
   * @see #toAssociativeArray()
   * @see java.util.Properties
   * @see #toMap()
   */
  public @NotNull Properties toProperties() {
    return environmentVariables().toProperties();
  }

  /**
   * {@link PropertiesAdapter} implementation that adapts Java {@link System#getProperties() System properties}
   * and accesses property values at runtime in realtime, as they change.
   *
   * @see org.cp.elements.util.PropertiesAdapter
   */
  protected static class SystemPropertiesAdapter extends PropertiesAdapter {

    protected static final SystemPropertiesAdapter INSTANCE = new SystemPropertiesAdapter();

    protected SystemPropertiesAdapter() {
      super(new Properties());
    }

    @Override
    protected @Nullable String getPropertyValue(@NotNull String propertyName, @Nullable String defaultValue) {
      return System.getProperty(propertyName, defaultValue);
    }
  }
}
