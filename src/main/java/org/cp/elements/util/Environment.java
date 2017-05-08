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

package org.cp.elements.util;

import static org.cp.elements.io.FileSystemUtils.newFile;

import java.io.File;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Version;

/**
 * The {@link Environment} class is a representation of the system environment
 * in which the software application is running.
 *
 * @author John Blum
 * @see java.io.File
 * @see java.lang.Iterable
 * @see java.lang.String
 * @see java.lang.System
 * @see java.util.Map
 * @see java.util.Properties
 * @see org.cp.elements.lang.Version
 * @see org.cp.elements.util.MapUtils
 * @see org.cp.elements.util.PropertiesAdapter
 * @see org.cp.elements.util.PropertiesBuilder
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class Environment implements Iterable<String> {

  protected static final String JAVA_CLASS_PATH = "java.class.path";
  protected static final String JAVA_HOME = "java.home";
  protected static final String JAVA_LIBRARY_PATH = "java.library.path";
  protected static final String JAVA_VENDOR = "java.vendor";
  protected static final String JAVA_VERSION = "java.version";

  protected static final String JVM_NAME = "java.vm.name";
  protected static final String JVM_VENDOR = "java.vm.vendor";
  protected static final String JVM_VERSION = "java.vm.version";

  protected static final String OS_ARCHITECTURE = "os.arch";
  protected static final String OS_NAME = "os.name";
  protected static final String OS_VERSION = "os.version";

  protected static final String PATH = "PATH";

  protected static final String USER_DIRECTORY = "user.dir";
  protected static final String USER_HOME = "user.home";
  protected static final String USER_NAME = "user.name";

  /**
   * Factory method to construct an instance of {@link Environment} initialized with the given associative array
   * containing the initial environment variable configuration.
   *
   * @param associativeArray {@link String} array containing a collection of key/value pairs (mapping).
   * @return a new instance of {@link Environment} initialized with the given associative array.
   * @see org.cp.elements.util.MapUtils#fromAssociativeArray(String[])
   * @see java.lang.String[]
   * @see #from(Map)
   */
  public static Environment from(String[] associativeArray) {
    return from(MapUtils.fromAssociativeArray(associativeArray));
  }

  /***
   * Factory method to construct an instance of {@link Environment} initialized with the given {@link Map}
   * containing the initial environment variable configuration.
   *
   * @param map {@link Map} with the initial environment variable configuration.
   * @return a new instance of {@link Environment} initialized with the given {@link Map}.
   * @throws IllegalArgumentException if {@link Map} is {@literal null}.
   * @see org.cp.elements.util.PropertiesBuilder#from(Map)
   * @see #Environment(PropertiesAdapter)
   * @see java.util.Map
   */
  public static Environment from(Map<String, String> map) {
    return new Environment(PropertiesBuilder.from(map).buildPropertiesAdapter());
  }

  /***
   * Factory method to construct an instance of {@link Environment} initialized with the given {@link Properties}
   * containing the initial environment variable configuration.
   *
   * @param properties {@link Properties} with the initial environment variable configuration.
   * @return a new instance of {@link Environment} initialized with the given {@link Properties}.
   * @throws IllegalArgumentException if {@link Properties} is {@literal null}.
   * @see org.cp.elements.util.PropertiesAdapter#from(Properties)
   * @see #Environment(PropertiesAdapter)
   * @see java.util.Properties
   */
  public static Environment from(Properties properties) {
    return new Environment(PropertiesAdapter.from(properties));
  }

  /**
   * Factory method to construct an instance of {@link Environment} initialized with the given system
   * environment variables.
   *
   * @return a new instance of {@link Environment} initialized with the current system environment.
   * @see org.cp.elements.util.PropertiesBuilder#fromEnvironmentVariables()
   * @see #Environment(PropertiesAdapter)
   * @see java.lang.System#getenv()
   */
  public static Environment fromEnvironmentVariables() {
    return new Environment(PropertiesBuilder.fromEnvironmentVariables().buildPropertiesAdapter());
  }

  private final PropertiesAdapter environmentVariables;
  private final PropertiesAdapter systemProperties;

  /**
   * Constructs an instance of the {@link Environment} class initialized with the given {@link PropertiesAdapter}
   * containing the environment variable configuration of this system.
   *
   * @param environmentVariables {@link PropertiesAdapter} containing environment variable configuration.
   * @throws IllegalArgumentException if {@link PropertiesAdapter} is {@literal null}.
   */
  protected Environment(PropertiesAdapter environmentVariables) {
    Assert.notNull(environmentVariables, "The initial environment cannot be null");

    this.environmentVariables = environmentVariables;
    this.systemProperties = PropertiesBuilder.fromSystemProperties().buildPropertiesAdapter();
  }

  /**
   * Returns a reference to the {@link PropertiesAdapter} containing a snapshot of the system environment variables.
   *
   * @return an instance of {@link PropertiesAdapter} containing the system's environment variable configuration.
   * @see org.cp.elements.util.PropertiesAdapter
   */
  protected PropertiesAdapter environmentVariables() {
    return this.environmentVariables;
  }

  /**
   * Returns a reference to the {@link PropertiesAdapter} containing a snapshot of the system properties.
   *
   * @return an instance of {@link PropertiesAdapter} containing the system properties.
   * @see org.cp.elements.util.PropertiesAdapter
   */
  protected PropertiesAdapter systemProperties() {
    return this.systemProperties;
  }

  /**
   * Determines whether this {@link Environment} contains an variables.
   *
   * @return a boolean value indicating whether this {@link Environment} contains any variables.
   * @see #environmentVariables()
   */
  public boolean isEmpty() {
    return environmentVariables().isEmpty();
  }

  /**
   * Determines whether the given environment variable identified by name is set in this {@link Environment}.
   *
   * @param environmentVariableName {@link String} name of the environment variable to evaluate.
   * @return a boolean value indicating whether the environment variable identified by name
   * is set in this {@link Environment}.
   * @see #environmentVariables()
   */
  public boolean isSet(String environmentVariableName) {
    return environmentVariables().isSet(environmentVariableName);
  }

  /**
   * Copies the contents of this {@link Environment} to the given {@link Map}.
   *
   * @param map {@link Map} in which to copy the contents of this {@link Environment}.
   * @return the given {@link Map}.
   * @throws NullPointerException if {@link Map} is {@literal null}.
   * @see #environmentVariables()
   * @see java.util.Map
   */
  public Map<String, String> copyTo(Map<String, String> map) {
    map.putAll(environmentVariables().toMap());
    return map;
  }

  /**
   * Copies the contents of this {@link Environment} to the given {@link Properties}.
   *
   * @param properties {@link Properties} in which to copy the contents of this {@link Environment}.
   * @return the given {@link Properties}.
   * @throws NullPointerException if {@link Properties} is {@literal null}.
   * @see #environmentVariables()
   * @see java.util.Properties
   */
  public Properties copyTo(Properties properties) {
    properties.putAll(environmentVariables().toMap());
    return properties;
  }

  /**
   * Returns the value set for the environment variable identified by the given name.
   *
   * @param environmentVariableName {@link String} name of the environment variable.
   * @return the value set the environment variable identified by the given name.
   * @see #environmentVariables()
   */
  public String get(String environmentVariableName) {
    return environmentVariables().get(environmentVariableName);
  }

  /**
   * Returns the value set for the environment variable identified by the given name.  If the environment variable
   * is not set, then {@code defaultValue} is returned.
   *
   * @param environmentVariableName {@link String} name of the environment variable.
   * @param defaultValue the default value to return if the specified environment variable is not set.
   * @return the value set the environment variable identified by the given name or {@code defaultValue}
   * if the named environment variable is not set.
   * @see #environmentVariables()
   */
  public String get(String environmentVariableName, String defaultValue) {
    return environmentVariables().get(environmentVariableName, defaultValue);
  }

  /**
   * Returns the value set for the environment variable identified by the given name as the given {@link Class} type.
   *
   * @param <T> {@link Class} type to convert the environment variable value to.
   * @param environmentVariableName {@link String} name of the environment variable.
   * @param type {@link Class} type to convert the environment variable value to.
   * @return the value set the environment variable identified by the given name as the given {@link Class} type.
   * @see #environmentVariables()
   */
  public <T> T getAs(String environmentVariableName, Class<T> type) {
    return environmentVariables().getAsType(environmentVariableName, type);
  }

  /**
   * Returns the value set for the environment variable identified by the given name as the given {@link Class} type.
   * Returns the {@code defaultValue} if the named environment variable is not set.
   *
   * @param <T> {@link Class} type to convert the environment variable value to.
   * @param environmentVariableName {@link String} name of the environment variable.
   * @param type {@link Class} type to convert the environment variable value to.
   * @param defaultValue the default value to return if the specified environment variable is not set.
   * @return the value set the environment variable identified by the given name as the given {@link Class} type
   * or {@code defaultValue} if the named environment variable is not set.
   * @see #environmentVariables()
   */
  public <T> T getAs(String environmentVariableName, Class<T> type, T defaultValue) {
    return environmentVariables().getAsType(environmentVariableName, type, defaultValue);
  }

  /* (non-Javadoc) */
  public String getJavaClassPath() {
    return systemProperties().get(JAVA_CLASS_PATH);
  }

  /* (non-Javadoc) */
  public File getJavaHome() {
    return newFile(systemProperties().get(JAVA_HOME));
  }

  /* (non-Javadoc) */
  public String getJavaLibraryPath() {
    return systemProperties().get(JAVA_LIBRARY_PATH);
  }

  /* (non-Javadoc) */
  public String getJavaVendor() {
    return systemProperties().get(JAVA_VENDOR);
  }

  /* (non-Javadoc) */
  public Version getJavaVersion() {
    return Version.parse(systemProperties().get(JAVA_VERSION));
  }

  /* (non-Javadoc) */
  public String getJvmName() {
    return systemProperties().get(JVM_NAME);
  }

  /* (non-Javadoc) */
  public String getJvmVendor() {
    return systemProperties().get(JVM_VENDOR);
  }

  /* (non-Javadoc) */
  public Version getJvmVersion() {
    return Version.parse(systemProperties().get(JVM_VERSION));
  }

  /* (non-Javadoc) */
  public String getOperatingSystemArchitecture() {
    return systemProperties().get(OS_ARCHITECTURE);
  }

  /* (non-Javadoc) */
  public String getOperatingSystemName() {
    return systemProperties().get(OS_NAME);
  }

  /* (non-Javadoc) */
  public Version getOperatingSystemVersion() {
    return Version.parse(systemProperties().get(OS_VERSION));
  }

  /* (non-Javadoc) */
  public String getSystemPath() {
    return environmentVariables().get(PATH);
  }

  /* (non-Javadoc) */
  public File getUserDirectory() {
    return newFile(systemProperties().get(USER_DIRECTORY));
  }

  /* (non-Javadoc) */
  public File getUserHome() {
    return newFile(systemProperties().get(USER_HOME));
  }

  /* (non-Javadoc) */
  public String getUserName() {
    return systemProperties().get(USER_NAME);
  }

  /**
   * @inheritDoc
   */
  @Override
  public Iterator<String> iterator() {
    return environmentVariables().iterator();
  }

  /**
   * Returns the number of variables set in this {@link Environment}.
   *
   * @return an integer value indicating the number of variables set in this {@link Environment}.
   * @see #environmentVariables()
   */
  public int size() {
    return environmentVariables().size();
  }

  /**
   * @inheritDoc
   */
  @Override
  public boolean equals(Object obj) {
    if (obj == this) {
      return true;
    }

    if (!(obj instanceof Environment)) {
      return false;
    }

    Environment that = (Environment) obj;

    return this.environmentVariables().equals(that.environmentVariables());
  }

  /**
   * @inheritDoc
   */
  @Override
  public int hashCode() {
    int hashValue = 17;
    hashValue = 37 * hashValue + environmentVariables().hashCode();
    return hashValue;
  }

  /**
   * @inheritDoc
   */
  @Override
  public String toString() {
    return environmentVariables().toString();
  }

  public String[] toAssociativeArray() {
    return MapUtils.toAssociativeArray(toMap());
  }

  public Map<String, String> toMap() {
    return environmentVariables().toMap();
  }

  public Properties toProperties() {
    return environmentVariables().getProperties();
  }
}
