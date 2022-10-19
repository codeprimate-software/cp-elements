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
package org.cp.elements.context.configure.provider;

import java.io.File;
import java.io.InputStream;
import java.util.Comparator;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;

import org.cp.elements.context.configure.AbstractConfigurationService;
import org.cp.elements.context.configure.Configuration;
import org.cp.elements.context.configure.ConfigurationService;
import org.cp.elements.context.configure.support.DelegatingConfiguration;
import org.cp.elements.context.configure.support.EnvironmentVariablesConfiguration;
import org.cp.elements.context.configure.support.PropertiesConfiguration;
import org.cp.elements.context.configure.support.SystemPropertiesConfiguration;
import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.Orderable;
import org.cp.elements.lang.Ordered;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.util.CollectionUtils;
import org.cp.elements.util.PropertiesBuilder;

/**
 * Elements implementation of the {@link ConfigurationService}.
 *
 * @author John Blum
 * @see java.io.File
 * @see java.io.InputStream
 * @see java.util.Comparator
 * @see java.util.Properties
 * @see java.util.function.Function
 * @see org.cp.elements.context.configure.AbstractConfigurationService
 * @see org.cp.elements.context.configure.Configuration
 * @see org.cp.elements.context.configure.ConfigurationService
 * @see org.cp.elements.context.configure.support.DelegatingConfiguration
 * @see org.cp.elements.context.configure.support.EnvironmentVariablesConfiguration
 * @see org.cp.elements.context.configure.support.PropertiesConfiguration
 * @see org.cp.elements.context.configure.support.SystemPropertiesConfiguration
 * @see org.cp.elements.util.PropertiesBuilder
 * @since 1.0.0
 */
public class ElementsConfigurationService extends AbstractConfigurationService {

  protected static final Class<?> RESOURCE_SOURCE_TYPE = ElementsConfigurationService.class;

  protected static final Function<File, Properties> FROM_FILE_TO_PROPERTIES_FUNCTION =
    propertiesFile -> PropertiesBuilder.from(propertiesFile).build();

  protected static final Function<InputStream, Properties> FROM_INPUT_STREAM_TO_PROPERTIES_FUNCTION =
    propertiesInputStream -> PropertiesBuilder.from(propertiesInputStream).build();

  protected static final String APPLICATION_PROPERTIES_FILENAME = "application.properties";
  protected static final String CONFIGURATION_ORDER_PROPERTY = "elements.configuration.order";
  protected static final String PROFILED_APPLICATION_PROPERTIES_FILENAME_TEMPLATE = "application-%s.properties";

  private static final int ENVIRONMENT_VARIABLES_CONFIGURATION_ORDER_BASE = 10_000;
  private static final int PROGRAM_CLASSPATH_CONFIGURATION_ORDER_BASE = 1_000_000;
  private static final int PROGRAM_DIRECTORY_CONFIGURATION_ORDER_BASE = 100_000;
  private static final int SYSTEM_PROPERTIES_CONFIGURATION_ORDER_BASE = Ordered.FIRST;
  private static final int USER_CONFIGURATION_ORDER_BASE = 1_000;

  /**
   * Constructs a new instance of the {@link ElementsConfigurationService} initialized with configuration metadata
   * ({@link java.util.Properties}) contained in files in the file system or the Java classpath
   * in well known locations.
   *
   * @see #getActiveProfilesList()
   * @see #registerSystemProperties()
   * @see #registerUserApplicationProperties(List)
   * @see #registerEnvironmentVariables()
   * @see #registerProgramApplicationProperties(List)
   */
  public ElementsConfigurationService() {

    List<String> activeProfiles = getActiveProfilesList();

    registerSystemProperties();
    registerUserApplicationProperties(activeProfiles);
    registerEnvironmentVariables();
    registerProgramApplicationProperties(activeProfiles);
  }

  private void registerEnvironmentVariables() {
    register(new OrderedConfiguration(new EnvironmentVariablesConfiguration(),
      ENVIRONMENT_VARIABLES_CONFIGURATION_ORDER_BASE));
  }

  private void registerProgramApplicationProperties(List<String> activeProfiles) {
    registerProgramApplicationPropertiesFromDirectory(activeProfiles);
    registerProgramApplicationPropertiesFromClasspath(activeProfiles);
  }

  private void registerProgramApplicationPropertiesFromClasspath(List<String> activeProfiles) {

    AtomicInteger orderCounter = new AtomicInteger(PROGRAM_CLASSPATH_CONFIGURATION_ORDER_BASE);

    Function<String, InputStream> fromProfileToApplicationPropertiesInputStreamFunction = profile ->
      RESOURCE_SOURCE_TYPE.getResourceAsStream(toClasspathResource(toProfiledApplicationPropertiesFilename(profile)));

    CollectionUtils.nullSafeList(activeProfiles).stream()
      .filter(StringUtils::hasText)
      .map(this::toTrimmedLowerCaseString)
      .map(fromProfileToApplicationPropertiesInputStreamFunction)
      .map(FROM_INPUT_STREAM_TO_PROPERTIES_FUNCTION)
      .map(this::newInitializedPropertiesConfiguration)
      .sorted(ConfigurationOrderComparator.INSTANCE)
      .map(configuration -> new OrderedConfiguration(configuration, orderCounter.getAndIncrement()))
      .forEach(this::register);

    InputStream programClasspathApplicationProperties =
      RESOURCE_SOURCE_TYPE.getResourceAsStream(toClasspathResource(APPLICATION_PROPERTIES_FILENAME));

    if (programClasspathApplicationProperties != null) {
      register(new OrderedConfiguration(new PropertiesConfiguration(
        PropertiesBuilder.from(programClasspathApplicationProperties).build()), orderCounter.getAndIncrement()));
    }
  }

  private void registerProgramApplicationPropertiesFromDirectory(List<String> activeProfiles) {
    registerDirectoryBasedApplicationProperties(FileSystemUtils.WORKING_DIRECTORY,
      PROGRAM_DIRECTORY_CONFIGURATION_ORDER_BASE, activeProfiles);
  }

  private void registerSystemProperties() {
    register(new OrderedConfiguration(new SystemPropertiesConfiguration(), SYSTEM_PROPERTIES_CONFIGURATION_ORDER_BASE));
  }

  private void registerUserApplicationProperties(List<String> activeProfiles) {
    registerDirectoryBasedApplicationProperties(FileSystemUtils.USER_HOME_DIRECTORY,
      USER_CONFIGURATION_ORDER_BASE, activeProfiles);
  }

  private void registerDirectoryBasedApplicationProperties(@NotNull File directory, int configurationOrderBase,
      @NotNull List<String> activeProfiles) {

    AtomicInteger configurationOrderCounter = new AtomicInteger(configurationOrderBase);

    Function<String, File> fromProfileToApplicationPropertiesFileFunction =
      profile -> toProfiledApplicationPropertiesFile(directory, profile);

    // Search for and register profile-based application.properties files from the given directory, if present.
    // (For example: ${user.home}/application-dev.properties, or ${user.dir}/application-qa.properties).
    CollectionUtils.nullSafeList(activeProfiles).stream()
      .filter(StringUtils::hasText)
      .map(this::toTrimmedLowerCaseString)
      .map(fromProfileToApplicationPropertiesFileFunction)
      .filter(FileSystemUtils::isFile)
      .map(FROM_FILE_TO_PROPERTIES_FUNCTION)
      .map(this::newInitializedPropertiesConfiguration)
      .sorted(ConfigurationOrderComparator.INSTANCE)
      .map(configuration -> new OrderedConfiguration(configuration, configurationOrderCounter.getAndIncrement()))
      .forEach(this::register);

    // Search for and register the base (non-profiled) application.properties from the given directory.
    File applicationProperties = toApplicationPropertiesFile(directory);

    if (FileSystemUtils.isFile(applicationProperties)) {
      register(new OrderedConfiguration(new PropertiesConfiguration(
        PropertiesBuilder.from(applicationProperties).build()), configurationOrderCounter.getAndIncrement()));
    }
  }

  private @NotNull PropertiesConfiguration newInitializedPropertiesConfiguration(@NotNull Properties properties) {
    return initialize(new PropertiesConfiguration(properties));
  }

  private @NotNull File toApplicationPropertiesFile(@NotNull File directory) {
    return new File(directory, APPLICATION_PROPERTIES_FILENAME);
  }

  private @NotNull String toClasspathResource(@NotNull String resource) {
    return "/".concat(resource);
  }

  private @NotNull File toProfiledApplicationPropertiesFile(@NotNull File directory, @NotNull String profile) {
    return new File(directory, toProfiledApplicationPropertiesFilename(profile));
  }

  private @NotNull String toProfiledApplicationPropertiesFilename(@NotNull String profile) {
    return String.format(PROFILED_APPLICATION_PROPERTIES_FILENAME_TEMPLATE, profile);
  }

  private @NotNull String toTrimmedLowerCaseString(@NotNull String profile) {
    return profile.toLowerCase().trim();
  }

  /**
   * {@link Comparator} implementation that orders {@link Configuration} objects according to
   * the {@link #CONFIGURATION_ORDER_PROPERTY}.
   *
   * @see org.cp.elements.context.configure.Configuration
   * @see java.util.Comparator
   */
  protected static class ConfigurationOrderComparator implements Comparator<Configuration> {

    protected static final ConfigurationOrderComparator INSTANCE = new ConfigurationOrderComparator();

    @Override
    public int compare(@NotNull Configuration one, @NotNull Configuration two) {
      return getOrder(one).compareTo(getOrder(two));
    }

    private @NotNull Integer getOrder(@NotNull Configuration configuration) {
      return configuration.getPropertyValueAs(CONFIGURATION_ORDER_PROPERTY, Integer.class, Ordered.DEFAULT);
    }
  }

  /**
   * {@link Configuration} implementation that can be {@link Orderable ordered} relative to other {@link Configuration}
   * objects aggregated in an array or {@link java.util.Collection}.
   *
   * @see org.cp.elements.context.configure.support.DelegatingConfiguration
   * @see org.cp.elements.lang.Orderable
   */
  protected static class OrderedConfiguration extends DelegatingConfiguration implements Orderable<Integer> {

    private final Integer order;

    /**
     * Constructs a new instance of {@link OrderedConfiguration} initialized with the given,
     * required {@link Configuration} object used as the delegate as well as the {@link Integer order}
     * of the {@link Configuration} relative to other {@link Configuration} objects aggregated in an array
     * or {@link java.util.Collection}.
     *
     * @param delegate {@link Configuration} used as the {@link Orderable ordered} {@literal delegate};
     * must not be {@literal null}.
     * @param order {@link Integer} value specifying the order of the {@literal delegate} {@link Configuration}
     * relative to other {@link Configuration} objects aggregated in an array or {@link java.util.Collection}.
     * @throws IllegalArgumentException if the {@link Configuration delegate} or {@link Integer order}
     * are {@literal null}.
     * @see org.cp.elements.context.configure.Configuration
     */
    public OrderedConfiguration(@NotNull Configuration delegate, @NotNull Integer order) {
      super(delegate);
      this.order = ObjectUtils.requireObject(order, "Order is required");
    }

    @Override
    public @NotNull Integer getOrder() {
      return this.order;
    }
  }
}
