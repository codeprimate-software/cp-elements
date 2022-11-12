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

import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.Charset;
import java.util.Objects;
import java.util.Optional;
import java.util.Scanner;
import java.util.stream.Stream;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.util.ArrayUtils;

/**
 * {@link SystemUtils} is an abstract utility class designed to interact with and access properties
 * of the Java {@link java.lang.System} class.
 *
 * @author John J. Blum
 * @see java.lang.System
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class SystemUtils {

  // Define $PATH separator
  public static final String PATH_SEPARATOR = System.getProperty("path.separator");

  // Java Virtual Machine (JVM) Names
  public static final String IBM_J9_JVM_NAME = "J9";
  public static final String ORACLE_HOTSPOT_JVM_NAME = "HotSpot";
  public static final String ORACLE_JROCKIT_JVM_NAME = "JRockit";

  // Java Virtual Machine (JVM) Vendors
  public static final String APPLE_JVM_VENDOR = "Apple";
  public static final String IBM_JVM_VENDOR = "IBM";
  public static final String ORACLE_JVM_VENDOR = "Oracle";

  // Operating System Names
  public static final String LINUX_OS_NAME = "Linux";
  public static final String MAC_OSX_NAME = "Mac";
  public static final String WINDOWS_OS_NAME = "Windows";

  // Operating System Vendors
  public static final String MAC_OSX_VENDOR = "Apple";
  public static final String RED_HAT_LINUX_OS_VENDOR = "Red Hat";
  public static final String SUSE_LINUX_OS_VENDOR = "Suse";
  public static final String UBUNTU_LINUX_OS_VENDOR = "Canonical";
  public static final String WINDOWS_OS_VENDOR = "Microsoft";

  // Define Common System Directories
  public static final String JAVA_HOME = System.getProperty("java.home");
  public static final String TEMPORARY_DIRECTORY = System.getProperty("java.io.tmpdir");

  // User Information
  public static final String USER_DIRECTORY = System.getProperty("user.dir");
  public static final String USER_HOME = System.getProperty("user.home");
  public static final String USERNAME = System.getProperty("user.name");

  /**
   * Utility method used to determine if the installed Java Runtime Environment (JRE)
   * meets the minimum expected version requirement.
   *
   * Java versions are typically of the form {@literal "1.8.0_66"}. The Java version is determined by
   * the {@literal "java.version"} {@link System#getProperties() System property}.
   *
   * @param expectedVersion numerical {@link String} value specifying the expected minimum Java version
   * of the installed Java Runtime Environment (JRE).
   * @return a boolean value indicating if the Java Runtime Environment (JRE) meets the expected minimum
   * version requirement.
   * @see org.cp.elements.lang.JavaVersion#current()
   * @see org.cp.elements.lang.JavaVersion
   */
  public static boolean isJavaVersionAtLeast(String expectedVersion) {
    return JavaVersion.current().isNewerThanOrEqualTo(JavaVersion.parse(expectedVersion));
  }

  /**
   * Utility method to determine if this Java application process is executing on the Apple JVM.
   *
   * @return a boolean value indicating whether the Java application process is executing and running
   * on the Apple JVM.
   * @see #isJvmMake(String)
   * @see #APPLE_JVM_VENDOR
   */
  public static boolean isAppleJvm() {
    return isJvmMake(APPLE_JVM_VENDOR);
  }

  /**
   * Utility method to determine if this Java application process is executing on the IBM JVM.
   *
   * @return a boolean value indicating whether the Java application process is executing and running
   * on the IBM JVM.
   * @see #isJvmMake(String)
   * @see #IBM_JVM_VENDOR
   */
  public static boolean isIbmJvm() {
    return isJvmMake(IBM_JVM_VENDOR);
  }

  /**
   * Utility method to determine if this Java application process is executing on the Oracle JVM.
   *
   * @return a boolean value indicating whether the Java application process is executing and running
   * on the Oracle JVM.
   * @see #isJvmMake(String)
   * @see #ORACLE_JVM_VENDOR
   */
  public static boolean isOracleJvm() {
    return isJvmMake(ORACLE_JVM_VENDOR);
  }

  /**
   * Determines whether this Java Virtual Machine (JVM) is the expected make (vendor).  The current JVM is determined
   * by the "java.vm.vendor" System property.
   *
   * @param expectedJvmVendor a String indicating the expected JVM vendor (make).
   * @return a boolean value indicating whether this JVM is the expected make.
   * @see java.lang.System#getProperty(String)
   */
  protected static boolean isJvmMake(String expectedJvmVendor) {
    return StringUtils.contains(System.getProperty("java.vm.vendor"), expectedJvmVendor);
  }

  /**
   * Utility method to determine if this Java application process is executing on either the 'client' or 'server'
   * Java HotSpot VM.
   *
   * @return a boolean value indicating whether this Java application process is executing on the Java HotSpot VM.
   * @see #isJvmModel(String)
   * @see #ORACLE_HOTSPOT_JVM_NAME
   */
  public static boolean isHotSpotJvm() {
    return isJvmModel(ORACLE_HOTSPOT_JVM_NAME);
  }

  /**
   * Utility method to determine if this Java application process is executing on the IBM J9 VM.
   *
   * @return a boolean value indicating whether this Java application process is executing on the IBM J9 VM.
   * @see #isJvmModel(String)
   * @see #IBM_J9_JVM_NAME
   */
  public static boolean isJ9Jvm() {
    return isJvmModel(IBM_J9_JVM_NAME);
  }

  /**
   * Utility method to determine if this Java application process is executing on the 'client' or 'server'
   * Oracle JRockit VM.
   *
   * @return a boolean value indicating whether this Java application process is executing on the Oracle JRockit VM.
   * @see #isJvmModel(String)
   * @see #ORACLE_JROCKIT_JVM_NAME
   */
  public static boolean isJRockitJvm() {
    return isJvmModel(ORACLE_JROCKIT_JVM_NAME);
  }

  /**
   * Determines whether this Java Virtual Machine (JVM) is the expected model.  The current JVM is determined
   * by the "java.vm.name" System property.
   *
   * @param expectedJvmName a String indicating the name of the expected JVM model.
   * @return a boolean determining whether this JVM is the expected model.
   * @see java.lang.System#getProperty(String)
   */
  protected static boolean isJvmModel(String expectedJvmName) {
    return StringUtils.contains(System.getProperty("java.vm.name"), expectedJvmName);
  }

  /**
   * Utility method to determine if this Java application process is executing in a Linux operating system environment.
   *
   * @return a boolean value indicating whether this Java application process is executing in Linux.
   * @see #isOS(String)
   * @see #LINUX_OS_NAME
   */
  public static boolean isLinux() {
    return isOS(LINUX_OS_NAME);
  }

  /**
   * Utility method to determine if this Java application process is executing in a Apple Mac OSX operating system
   * environment.
   *
   * @return a boolean value indicating whether this Java application process is executing in Mac OSX.
   * @see #isOS(String)
   * @see #MAC_OSX_NAME
   */
  public static boolean isMacOSX() {
    return isOS(MAC_OSX_NAME);
  }

  /**
   * Utility method to determine if this Java application process is executing in a Microsoft Windows-based
   * operating system environment.
   *
   * @return a boolean value indicating whether this Java application process is executing in Windows.
   * @see #isOS(String)
   * @see #WINDOWS_OS_NAME
   */
  public static boolean isWindows() {
    return isOS(WINDOWS_OS_NAME);
  }

  /**
   * Determines whether the operating system is UNIX-based.
   *
   * @return a boolean value indicating whether the operating system on localhost is UNIX-based.
   * @see #isLinux()
   * @see #isMacOSX()
   */
  public static boolean isUnixBasedOperatingSystem() {
    return (isLinux() || isMacOSX());
  }

  /**
   * Determines wheehr this Operating System is the expected Operating System environment.  The current OS is determined
   * by the "os.name" System property.
   *
   * @param expectedOsName a String indicating the name of the expected OS environment.
   * @return a boolean value indicating whether this OS is the expected OS.
   * @see java.lang.System#getProperty(String)
   */
  @NullSafe
  protected static boolean isOS(@Nullable String expectedOsName) {
    return StringUtils.contains(System.getProperty("os.name"), expectedOsName);
  }

  /**
   * Gets the configured {@link String value} for the {@literal java.class.path}
   * {@link System#getProperty(String) System property}.
   *
   * @return the value of {@literal java.class.path} {@link System#getProperty(String) System property}.
   * @see java.util.Optional
   */
  public static Optional<String> javaSystemClasspath() {

    return Optional.ofNullable(System.getProperty("java.class.path"))
      .filter(StringUtils::hasText);
  }

  /**
   * Gets the configured {@link String classpath} for the given, required {@link ClassLoader}
   * as an {@link Optional} value.
   *
   * This method only works when the Java {@link ClassLoader} is a {@link URLClassLoader}. For more powerful logic
   * determining the {@link String classpath} of a Java {@link ClassLoader} use {@literal ClassGraph}.
   *
   * @param classLoader {@link ClassLoader} from which to get the configured {@literal String classpath}.
   * @return an {@link Optional} {@link String classpath} configured for the given, required {@link ClassLoader}.
   * @throws IllegalArgumentException if the {@link ClassLoader} is {@literal null}.
   * @see <a href="https://github.com/classgraph/classgraph">ClassGraph</a>
   * @see java.net.URLClassLoader
   * @see java.lang.ClassLoader
   * @see java.util.Optional
   */
  public static Optional<String> classLoaderClassPath(@NotNull ClassLoader classLoader) {

    Assert.notNull(classLoader, "ClassLoader is required");

    String[] classpathElements = Optional.of(classLoader)
      .filter(URLClassLoader.class::isInstance)
      .map(URLClassLoader.class::cast)
      .map(URLClassLoader::getURLs)
      .map(Stream::of)
      .orElseGet(Stream::empty)
      .filter(Objects::nonNull)
      .map(URL::getFile)
      .toArray(String[]::new);

    return ArrayUtils.isNotEmpty(classpathElements)
      ? Optional.of(StringUtils.concat(classpathElements, System.getProperty("path.separator", ":")))
      : Optional.empty();
  }

  /**
   * Gets the configured {@link String classpath} for the {@link ClassLoader#getSystemClassLoader() System ClassLoader}
   * as an {@link Optional} value.
   *
   * @return the configured {@link String classpath}
   * for the {@link ClassLoader#getSystemClassLoader() System ClassLoader}.
   * @see java.lang.ClassLoader#getSystemClassLoader()
   * @see java.util.Optional
   */
  public static Optional<String> systemClassLoaderClasspath() {
    return classLoaderClassPath(ClassLoader.getSystemClassLoader());
  }

  /**
   * Gets the configured {@link String classpath} for the {@link Thread#currentThread() current Thread's}
   * {@link Thread#getContextClassLoader() context ClassLoadder} as an {@link Optional} value.
   *
   * @return the configured {@link String classpath} for the {@link Thread#currentThread() current Thread's}
   * {@link Thread#getContextClassLoader() context ClassLoadder}
   * @see java.lang.Thread#getContextClassLoader()
   * @see java.util.Optional
   */
  public static Optional<String> threadContextClassLoaderClasspath() {
    return classLoaderClassPath(Thread.currentThread().getContextClassLoader());
  }

  /**
   * Prompts the user to press the enter key to exit the JVM process.
   */
  public static void promptPressEnterToExit() {
    System.err.println("Press <enter> to exit...");
    new Scanner(System.in, Charset.defaultCharset().name()).nextLine();
  }
}
