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

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import org.cp.elements.beans.annotation.Required;
import org.cp.elements.context.configure.annotation.ConfigurationProperties;
import org.cp.elements.context.container.DependencyInjection;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ClassUtils;
import org.cp.elements.lang.JavaType;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.Orderable;
import org.cp.elements.lang.Ordered;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.lang.annotation.Order;
import org.cp.elements.lang.reflect.MethodInterceptor;
import org.cp.elements.lang.reflect.MethodInvocation;
import org.cp.elements.lang.reflect.ProxyFactory;
import org.cp.elements.lang.reflect.ProxyService;
import org.cp.elements.util.ArrayUtils;

/**
 * Abstract base class and implementation of the {@link ConfigurationService} interface containing operations common
 * to all {@link ConfigurationService} provider implementations.
 *
 * @author John Blum
 * @see java.lang.reflect.Method
 * @see org.cp.elements.context.configure.ConfigurationService
 * @see org.cp.elements.context.configure.annotation.ConfigurationProperties
 * @see org.cp.elements.context.container.DependencyInjection
 * @see org.cp.elements.lang.reflect.MethodInterceptor
 * @see org.cp.elements.lang.reflect.MethodInvocation
 * @see org.cp.elements.lang.reflect.ProxyFactory
 * @see org.cp.elements.lang.reflect.ProxyService
 * @since 1.0.0
 */
public abstract class AbstractConfigurationService implements ConfigurationService {

  private final AtomicReference<List<String>> activeProfilesReference = new AtomicReference<>(null);
  private final AtomicReference<Set<String>> configurationPropertyNames = new AtomicReference<>(null);

  private final DependencyInjection dependencyInjectionContainer = DependencyInjection.getLoader().getServiceInstance();

  private final Set<Configuration> configurations = new HashSet<>();

  /**
   * Gets a {@link List} of {@literal Profiles} activated by this {@link ConfigurationService}.
   *
   * @return a {@link List} of {@literal Profiles} activated by this {@link ConfigurationService}.
   * @see #getActiveProfiles()
   */
  protected List<String> getActiveProfilesList() {
    return this.activeProfilesReference.updateAndGet(it -> it != null ? it
      : Arrays.asList(getActiveProfiles()));
  }

  /**
   * Collects all {@link String property names} across all {@link Configuration} objects aggregated by
   * this {@link ConfigurationService}.
   *
   * @return a complete {@link Set} of all {@link String property names} across all {@link Configuration} objects
   * aggregated by this {@link ConfigurationService}.
   */
  protected Set<String> getConfigurationPropertyNames() {

    return this.configurationPropertyNames.updateAndGet(it -> it != null ? it
      : StreamSupport.stream(this.spliterator(), false)
      .flatMap(configuration -> StreamSupport.stream(configuration.spliterator(), false))
      .collect(Collectors.toSet()));
  }

  /**
   * Gets an unmodifiable {@link Collection} of {@link Configuration} objects aggregated by
   * this {@link ConfigurationService}.
   *
   * @return an unmodifiable {@link Collection} of {@link Configuration} objects aggregated by
   * this {@link ConfigurationService}.
   * @see org.cp.elements.context.configure.Configuration
   * @see java.util.Collection
   */
  protected Collection<Configuration> getConfigurations() {
    return asSortedList(this.configurations);
  }

  private List<Configuration> asSortedList(@NotNull Collection<Configuration> configurations) {
    List<Configuration> configurationList = new ArrayList<>(configurations);
    configurationList.sort(ConfigurationComparator.INSTANCE);
    return configurationList;
  }

  /**
   * Gets a reference to the configured {@link DependencyInjection} container used by
   * this {@link AbstractConfigurationService} to initialize {@link Configuration} objects.
   *
   * @return reference to the configured {@link DependencyInjection} container.
   * @see org.cp.elements.context.container.DependencyInjection
   */
  protected @NotNull DependencyInjection getDependencyInjectionContainer() {
    return this.dependencyInjectionContainer;
  }

  /**
   * Determines whether the {@literal Profiles} declared by the given, required {@link Configuration} are active
   * as declared and specified by this {@link ConfigurationService}.
   *
   * If the given, required {@link Configuration} did not declare any {@literal Profiles},
   * then the {@link Configuration} is part of the {@literal DEFAULT Profile} and is active by default.
   * Otherwise, the {@literal Profile} or at least one of the {@literal Profiles} declared by the {@link Configuration}
   * must be contained in the {@link List} of {@literal Active Profiles} declared by this {@link ConfigurationService}.
   *
   * @param configuration {@link Configuration} to evaluate; must not be {@literal null}.
   * @return a boolean value indicating whether the {@literal Profiles} declared by the given,
   * required {@link Configuration} are active as declared and specified by this {@link ConfigurationService}.
   * @throws IllegalArgumentException if the {@link Configuration} is {@literal null}.
   * @see org.cp.elements.context.configure.Configuration#getProfiles()
   * @see #getActiveProfilesList()
   */
  protected boolean isProfileActive(@NotNull Configuration configuration) {

    Assert.notNull(configuration, "Configuration is required");

    String[] configurationProfiles = configuration.getProfiles();

    return ArrayUtils.isEmpty(configurationProfiles)
      || Arrays.stream(configurationProfiles)
        .filter(StringUtils::hasText)
        .anyMatch(configurationProfile -> getActiveProfilesList().contains(configurationProfile));
  }

  /**
   * Returns a non-modifiable {@literal Iterator} over the {@link Configuration Configurations} aggregated by
   * this {@link ConfigurationService}.
   *
   * @return a non-modifiable {@literal Iterator} over the {@link Configuration Configurations} aggregated by
   * this {@link ConfigurationService}.
   * @see org.cp.elements.context.configure.Configuration
   * @see java.util.Iterator
   */
  @Override
  public @NotNull Iterator<Configuration> iterator() {
    return getConfigurations().iterator();
  }

  /**
   * Proxies the given, required {@link Class#isInterface() interface} to enable access to configuration properties
   * using object-oriented techniques.
   *
   * @param <T> {@link Class type} of the interface.
   * @param interfaceType {@link Class interface type} to proxy; must not be {@literal null}.
   * @return a {@link Object JDK Dynamic Proxy} enabling access to configuration properties
   * in an object-oriented manner.
   * @throws IllegalArgumentException if the {@link Class interface type} is {@literal null}.
   * @see #proxy(Class, String)
   */
  @Override
  public @NotNull <T> T proxy(@NotNull Class<T> interfaceType) {
    return proxy(ObjectUtils.requireObject(interfaceType, "The interface to proxy is required"),
      resolvePropertyPrefix(interfaceType));
  }

  /**
   * Proxies the given, required {@link Class#isInterface() interface} to enable access to configuration properties
   * using object-oriented techniques.
   *
   * @param <T> {@link Class type} of the interface.
   * @param interfaceType {@link Class interface type} to proxy; must not be {@literal null}.
   * @param propertyPrefix {@link String} containing the {@literal prefix} to append to properties on access;
   * must ot be {@literal null} or {@literal empty}.
   * @return a {@link Object JDK Dynamic Proxy} enabling access to configuration properties
   * in an object-oriented manner.
   * @throws IllegalArgumentException if the {@link Class interface type} is {@literal null},
   * or the {@link Class type} is not an {@link Class#isInterface() interface},
   * or the {@link String propertyPrefix} is {@literal null} or {@literal empty}.
   * @see #proxy(Class, String)
   */
  protected @NotNull <T> T proxy(@NotNull Class<T> interfaceType, @NotNull String propertyPrefix) {

    Assert.notNull(interfaceType, "The interface to proxy is required");

    Assert.isTrue(interfaceType.isInterface(), "The type to proxy [%s] must be an interface",
      interfaceType.getName());

    Assert.hasText(propertyPrefix, "Property prefix [%s] is required", propertyPrefix);

    return ProxyService.newProxyService().findFirstProxyFactory(null, interfaceType)
      .map(proxyFactory -> newProxy(proxyFactory, interfaceType, propertyPrefix))
      .orElseThrow(() -> newConfigurationException("Failed to proxy @ConfigurationProperties interface [%s]",
        ObjectUtils.getClassName(interfaceType)));
  }

  private <T> T newProxy(@NotNull ProxyFactory<Object> proxyFactory, @NotNull Class<T> interfaceType,
      @NotNull String propertyPrefix) {

    ConfigurationPropertiesInterfaceMethodInterceptor methodInterceptor =
      ConfigurationPropertiesInterfaceMethodInterceptor.from(this, interfaceType, propertyPrefix);

    return proxyFactory
      .adviseWith(methodInterceptor)
      .implementing(interfaceType)
      .newProxy();
  }

  private @Nullable ConfigurationProperties resolveConfigurationPropertiesAnnotation(@Nullable Class<?> interfaceType) {

    if (interfaceType != null) {

      ConfigurationProperties configurationPropertiesAnnotation =
        interfaceType.getAnnotation(ConfigurationProperties.class);

      if (configurationPropertiesAnnotation == null) {

        Class<?>[] interfaceTypeExtensions = ArrayUtils.nullSafeArray(interfaceType.getInterfaces(), Class.class);

        for (Class<?> interfaceTypeExtension : interfaceTypeExtensions) {

          configurationPropertiesAnnotation = resolveConfigurationPropertiesAnnotation(interfaceTypeExtension);

          if (configurationPropertiesAnnotation != null) {
            return configurationPropertiesAnnotation;
          }
        }
      }

      return configurationPropertiesAnnotation;
    }

    return null;
  }

  private @NotNull String resolvePropertyPrefix(@NotNull Class<?> interfaceType) {

    ConfigurationProperties configurationPropertiesAnnotation =
      resolveConfigurationPropertiesAnnotation(interfaceType);

    return configurationPropertiesAnnotation != null
      ? configurationPropertiesAnnotation.propertyPrefix()
      : interfaceType.getSimpleName().toLowerCase();
  }

  /**
   * Registers the given {@link Configuration} with this {@link ConfigurationService}.
   *
   * @param configuration {@link Configuration} to register.
   * @return a boolean indicating whether the registration of the given {@link Configuration} was successful.
   * @see org.cp.elements.context.configure.Configuration
   * @see #unregister(Configuration)
   */
  @NullSafe
  @Override
  public boolean register(@NotNull Configuration configuration) {

    return configuration != null
      && isProfileActive(configuration)
      && this.configurations.add(initialize(configuration));
  }

  /**
   * Initializes the given {@link Configuration} object with references to required services provided by Elements
   * using {@link DependencyInjection}.
   *
   * @param <T> {@link Class type} of {@link Configuration} object.
   * @param configuration {@link Configuration} object to initialize (auto-wire with collaborator dependencies).
   * @return the given {@link Configuration} object after initialization.
   * @see org.cp.elements.context.container.DependencyInjection#inject(Object)
   * @see org.cp.elements.context.configure.Configuration
   * @see #getDependencyInjectionContainer()
   */
  protected @Nullable <T extends Configuration> T initialize(@Nullable T configuration) {
    return configuration != null ? getDependencyInjectionContainer().inject(configuration) : configuration;
  }

  /**
   * Unregisters the given {@link Configuration} from this {@link ConfigurationService}.
   *
   * @param configuration {@link Configuration} to unregister.
   * @return a boolean indicating whether the un-registration of the given {@link Configuration} was successful.
   * @see org.cp.elements.context.configure.Configuration
   * @see #register(Configuration)
   */
  @NullSafe
  @Override
  public boolean unregister(@NotNull Configuration configuration) {
    return configuration != null && this.configurations.remove(configuration);
  }

  /**
   * {@link MethodInterceptor} implementation that provides access to configuration properties given
   * an {@link Class#isInterface() interface} declaring property accessors using {@literal JavaBean} style
   * property accessor methods.
   *
   * Only {@literal getter} methods are currently supported.
   *
   * @see org.cp.elements.lang.reflect.MethodInterceptor
   */
  protected static class ConfigurationPropertiesInterfaceMethodInterceptor implements MethodInterceptor<Object> {

    /**
     * Factory method used to construct a new instance of {@link ConfigurationPropertiesInterfaceMethodInterceptor}
     * initialized with a reference to the {@link AbstractConfigurationService} and the {@link ConfigurationProperties}
     * annotated {@link Class interface} containing the configuration property accessor methods, where properties
     * are derived from the {@link Method#getName() method name} prefixed with the given,
     * required {@link String property prefix}.
     *
     * @param configurationService reference to the {@link AbstractConfigurationService}; must not be {@literal null}.
     * @param configurationPropertiesInterface {@link ConfigurationProperties} annotated {@link Class interface}
     * used to access configuration properties with method invocations.
     * @param propertyPrefix {@link String} containing the prefix prepended to all {@link String property names}.
     * @return a new {@link ConfigurationPropertiesInterfaceMethodInterceptor}.
     * @throws IllegalArgumentException if the reference to the {@link AbstractConfigurationService}
     * or {@link ConfigurationProperties} {@link Class interface} or {@link String property prefix} are {@literal null}.
     */
    protected static @NotNull ConfigurationPropertiesInterfaceMethodInterceptor from(
        @NotNull AbstractConfigurationService configurationService, @NotNull Class<?> configurationPropertiesInterface,
        @NotNull String propertyPrefix) {

      return new ConfigurationPropertiesInterfaceMethodInterceptor(configurationService,
        configurationPropertiesInterface, propertyPrefix);
    }

    private final AbstractConfigurationService configurationService;

    private final Class<?> configurationPropertiesInterface;

    private final String propertyPrefix;

    /**
     * Constructs a new instance of {@link ConfigurationPropertiesInterfaceMethodInterceptor} initialized with
     * a reference to the {@link AbstractConfigurationService} and the {@link ConfigurationProperties} annotated
     * {@link Class interface} containing the configuration property accessor methods, where properties are derived
     * from the {@link Method#getName() method name} prefixed with the given, required {@link String property prefix}.
     *
     * @param configurationService reference to the {@link AbstractConfigurationService}; must not be {@literal null}.
     * @param configurationPropertiesInterface {@link ConfigurationProperties} annotated {@link Class interface}
     * used to access configuration properties with method invocations.
     * @param propertyPrefix {@link String} containing the prefix prepended to all {@link String property names}.
     * @throws IllegalArgumentException if the reference to the {@link AbstractConfigurationService}
     * or {@link ConfigurationProperties} {@link Class interface} or {@link String property prefix} are {@literal null}.
     */
    protected ConfigurationPropertiesInterfaceMethodInterceptor(
        @NotNull AbstractConfigurationService configurationService, @NotNull Class<?> configurationPropertiesInterface,
        @NotNull String propertyPrefix) {

      this.configurationService =
        ObjectUtils.requireObject(configurationService, "ConfigurationService is required");

      this.configurationPropertiesInterface =
        ObjectUtils.requireObject(configurationPropertiesInterface, "Interface is required");

      this.propertyPrefix =
        StringUtils.requireText(propertyPrefix, "Property prefix [%s] is required", propertyPrefix);
    }

    /**
     * Gets the {@link ConfigurationProperties} annotated {@link Class interface} from which the configuration
     * properties will be accessed using the generated {@literal Proxy}.
     *
     * @return the {@link ConfigurationProperties} annotated {@link Class interface} from which the configuration
     * properties will be accessed using the generated {@literal Proxy}.
     */
    @SuppressWarnings("unused")
    protected @NotNull Class<?> getConfigurationPropertiesInterface() {
      return this.configurationPropertiesInterface;
    }

    /**
     * Gets a reference to the configured {@link AbstractConfigurationService} used to access configuration properties
     * from the {@literal Proxy}.
     *
     * @return a reference to the configured {@link AbstractConfigurationService}.
     */
    protected @NotNull AbstractConfigurationService getConfigurationService() {
      return this.configurationService;
    }

    /**
     * Gets the {@link String property prefix} to qualify all property access
     * using the {@link AbstractConfigurationService}.
     *
     * @return the {@link String property prefix} to qualify all property access
     * using the {@link AbstractConfigurationService}.
     */
    protected @NotNull String getPropertyPrefix() {
      return this.propertyPrefix;
    }

    /**
     * Qualifies the given, required, relative {@link String property name} using the {@link #getPropertyPrefix()}.
     *
     * @param propertyName {@link String} containing the {@literal name} of the property to qualify;
     * must not be {@literal null}.
     * @return the qualified {@link String property name} for the given, relative {@link String property name}.
     * @throws IllegalArgumentException if the given, relative {@link String property name}
     * is {@literal null} or {@literal empty}.
     * @see #getPropertyPrefix()
     */
    protected @NotNull String getQualifiedPropertyName(@NotNull String propertyName) {

      return getPropertyPrefix()
        .concat(StringUtils.DOT_SEPARATOR)
        .concat(StringUtils.requireText(propertyName, "Property name [%s] is required"));
    }

    @Override
    public final @Nullable Object getTarget() {
      return getConfigurationService();
    }

    @Override
    @SuppressWarnings("unchecked")
    public <R> Optional<R> intercept(@NotNull MethodInvocation methodInvocation) {

      Assert.notNull(methodInvocation, "MethodInvocation is required");

      Method method = validatePropertyAccessorMethod(methodInvocation.getMethod());

      Class<R> returnType = resolveReturnType(method);

      if (isNotJavaPrimitiveType(returnType)) {
        String propertyName = resolveMethodBasedPropertyName(method);
        return Optional.of(getConfigurationService().proxy(returnType, propertyName));
      }
      else {

        if (isDefaultValuePresent(method)) {

          Object defaultValue = resolveDefaultValue(methodInvocation);
          String propertyName = resolveMethodBasedPropertyName(method, false);

          R propertyValue = defaultValue instanceof Supplier
            ? getConfigurationService().getPropertyValueAs(propertyName, returnType, (Supplier<R>) defaultValue)
            : getConfigurationService().getPropertyValueAs(propertyName, returnType,
              ClassUtils.castTo(defaultValue, returnType));

          return Optional.ofNullable(propertyValue);
        }
        else {

          boolean required = isRequired(method);

          String propertyName = resolveMethodBasedPropertyName(method, required);

          R propertyValue = getConfigurationService().getPropertyValueAs(propertyName, returnType, required);

          return Optional.ofNullable(propertyValue);
        }
      }
    }

    @NullSafe
    private boolean isJavaPrimitiveType(@NotNull Class<?> type) {
      return JavaType.isJavaType(type);
    }

    @NullSafe
    private boolean isNotJavaPrimitiveType(@NotNull Class<?> type) {
      return !isJavaPrimitiveType(type);
    }

    @NullSafe
    private boolean isDefaultValuePresent(@Nullable Method method) {
      return method != null && method.getParameterCount() > 0;
    }

    @NullSafe
    private boolean isRequired(@Nullable Method method) {
      return method != null && method.isAnnotationPresent(Required.class);
    }

    private @Nullable Object resolveDefaultValue(@NotNull MethodInvocation methodInvocation) {

      Object[] arguments = ArrayUtils.nullSafeArray(methodInvocation.getArguments(), Object.class);

      return ArrayUtils.isNotEmpty(arguments) ? arguments[0] : null;
    }

    private @NotNull String resolveMethodBasedPropertyName(@NotNull Method method) {
      return resolveMethodBasedPropertyName(method, true);
    }

    private @NotNull String resolveMethodBasedPropertyName(@NotNull Method method, boolean required) {

      String methodName = method.getName();
      String propertyName = stripAccessorMethodNamePrefix(methodName);

      Set<String> possiblePropertyNames = resolvePossiblePropertyNames(propertyName);

      Predicate<String> propertyNameMatch = qualifiedPropertyName ->
        getConfigurationService().getConfigurationPropertyNames().stream()
          .anyMatch(configurationPropertyName -> configurationPropertyName.startsWith(qualifiedPropertyName));

      String qualifiedPropertyName = null;

      for (String possiblePropertyName : possiblePropertyNames) {
        qualifiedPropertyName = getQualifiedPropertyName(possiblePropertyName);
        if (propertyNameMatch.test(qualifiedPropertyName)) {
          return qualifiedPropertyName;
        }
      }

      if (!required) {
        return qualifiedPropertyName;
      }

      String possiblePropertyNamesString =
        Arrays.toString(ArrayUtils.sort(possiblePropertyNames.toArray(new String[0])));

      throw newConfigurationException("Failed to resolve a qualified property name"
        + " in the set of possible property names [%1$s] for the given method name [%2$s]"
        + " using the base property name [%3$s]", possiblePropertyNamesString, methodName, getPropertyPrefix());
    }

    // Property Name is already in "PascalCase" after stripping the Method.getName() of the accessor method name prefix.
    private Set<String> resolvePossiblePropertyNames(@NotNull String propertyName) {

      Set<String> possiblePropertyNames = new HashSet<>();

      possiblePropertyNames.add(propertyName); // PascalCase
      possiblePropertyNames.add(propertyName.toLowerCase()); // lowercase
      possiblePropertyNames.add(Character.toLowerCase(propertyName.charAt(0)) + propertyName.substring(1)); // camelCase

      char[] propertyNameCharacters = propertyName.toCharArray();

      StringBuilder dotDelimitedPropertyNameBuilder = new StringBuilder();

      dotDelimitedPropertyNameBuilder.append(Character.toLowerCase(propertyNameCharacters[0]));

      for (int index = 1; index < propertyNameCharacters.length; index++) {

        char character = propertyNameCharacters[index];

        if (Character.isUpperCase(character)) {
          dotDelimitedPropertyNameBuilder.append(StringUtils.DOT_SEPARATOR_CHAR);
        }

        dotDelimitedPropertyNameBuilder.append(Character.toLowerCase(character));
      }

      String dotDelimitedPropertyName = dotDelimitedPropertyNameBuilder.toString();

      possiblePropertyNames.add(dotDelimitedPropertyName); // dot.delimited
      possiblePropertyNames.add(dotDelimitedPropertyName.replaceAll("\\.", "-")); // kebab-case
      possiblePropertyNames.add(dotDelimitedPropertyName.replaceAll("\\.", "_")); // snake_case

      return possiblePropertyNames;
    }

    @SuppressWarnings("unchecked")
    private @NotNull <T> Class<T> resolveReturnType(@NotNull Method method) {

      Class<T> methodReturnType = (Class<T>) method.getReturnType();

      // Void.class.equals(methodReturnType) does not work!
      Assert.isFalse(Void.class.getSimpleName().equalsIgnoreCase(methodReturnType.getName()),
        "Property accessor method [%1$s] must not have a [%2$s] return type",
        method.getName(), Void.class.getSimpleName());

      return methodReturnType;
    }

    private @NotNull String stripAccessorMethodNamePrefix(@NotNull String methodName) {

      String[] methodNamePrefixes = ArrayUtils.asArray(
        ClassUtils.GETTER_METHOD_NAME_PREFIX,
        ClassUtils.IS_METHOD_NAME_PREFIX,
        ClassUtils.SETTER_METHOD_NAME_PREFIX
      );

      for (String methodNamePrefix : methodNamePrefixes) {
        if (methodName.startsWith(methodNamePrefix)) {
          return methodName.substring(methodNamePrefix.length());
        }
      }

      return methodName;
    }

    private @NotNull Method validatePropertyAccessorMethod(@NotNull Method method) {

      String methodName = ObjectUtils.requireObject(method, "Property accessor method is required").getName();

      boolean setterMethodName = methodName.startsWith(ClassUtils.SETTER_METHOD_NAME_PREFIX);

      Assert.isFalse(setterMethodName,
        "Using property setter methods [%s] to set properties is not supported", methodName);

      boolean readAccessorMethodName = Stream.of(ClassUtils.GETTER_METHOD_NAME_PREFIX, ClassUtils.IS_METHOD_NAME_PREFIX)
          .anyMatch(methodName::startsWith);

      Assert.isTrue(readAccessorMethodName, "Property accessor method name [%1$s] must start with [%2$s]",
        methodName, Arrays.toString(ArrayUtils.asArray(ClassUtils.GETTER_METHOD_NAME_PREFIX,
          ClassUtils.IS_METHOD_NAME_PREFIX)));

      return method;
    }
  }

  /**
   * {@link Comparator} implementation used to sort and order {@link Configuration} objects.
   *
   * A {@link Configuration} is inspected. If the {@link Configuration} object implements the {@link Orderable}
   * interface and the {@link Orderable#getOrder()} is a {@link Number} object, then an {@link Integer} value
   * is computed. If the {@link Configuration} object implemented the {@link Ordered} interface, then
   * the {@link Ordered#getIndex()} method is called to determine the {@link Integer value}. Finally,
   * if the {@link Configuration} {@link Class type} is annotated with the {@link Order} annotation, then
   * the {@link Integer value} is determined from the annotation.
   *
   * @see org.cp.elements.context.configure.Configuration
   * @see java.util.Comparator
   */
  protected static class ConfigurationComparator implements Comparator<Configuration> {

    protected static final ConfigurationComparator INSTANCE = new ConfigurationComparator();

    @Override
    public int compare(@NotNull Configuration one, @NotNull Configuration two) {
      return getOrder(one).compareTo(getOrder(two));
    }

    private Integer getOrder(@NotNull Configuration configuration) {

      if (configuration instanceof Orderable) {
        Comparable<?> order = ((Orderable<?>) configuration).getOrder();
        if (order instanceof Number) {
          return ((Number) order).intValue();
        }
      }
      else if (configuration instanceof Ordered) {
        return ((Ordered) configuration).getIndex();
      }
      else if (configuration.getClass().isAnnotationPresent(Order.class)) {
        return configuration.getClass().getAnnotation(Order.class).value();
      }

      return Ordered.DEFAULT;
    }
  }
}
