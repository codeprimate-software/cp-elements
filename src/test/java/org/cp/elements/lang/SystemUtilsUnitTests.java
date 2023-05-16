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

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assumptions.assumeThat;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.lang.management.ManagementFactory;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;

import org.junit.jupiter.api.Test;

import org.cp.elements.util.ArrayUtils;

/**
 * Unit Tests for {@link SystemUtils}.
 *
 * @author John J. Blum
 * @see java.lang.System
 * @see java.lang.management.ManagementFactory
 * @see java.lang.management.OperatingSystemMXBean
 * @see java.lang.management.RuntimeMXBean
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.lang.SystemUtils
 * @since 1.0.0
 */
public class SystemUtilsUnitTests {

  @Test
  public void isJavaVersionAtLeast180() {
    assertThat(SystemUtils.isJavaVersionAtLeast("1.8.0_0")).isTrue();
  }

  protected boolean isJvmMake(String expectedJvmVendor) {
    return ManagementFactory.getRuntimeMXBean().getVmVendor().contains(expectedJvmVendor);
  }

  protected boolean isJvmModel(String expectedJvmModel) {
    return ManagementFactory.getRuntimeMXBean().getVmName().contains(expectedJvmModel);
  }

  protected boolean isOs(String expectedOperatingSystem) {
    return ManagementFactory.getOperatingSystemMXBean().getName().contains(expectedOperatingSystem);
  }

  @Test
  public void isAppleJvm() {
    assertThat(SystemUtils.isAppleJvm()).isEqualTo(isJvmMake(SystemUtils.APPLE_JVM_VENDOR));
  }

  @Test
  public void isIbmJvm() {
    assertThat(SystemUtils.isIbmJvm()).isEqualTo(isJvmMake(SystemUtils.IBM_JVM_VENDOR));
  }

  @Test
  public void isOracleJvm() {
    assertThat(SystemUtils.isOracleJvm()).isEqualTo(isJvmMake(SystemUtils.ORACLE_JVM_VENDOR));
  }

  @Test
  public void isHotSpotJvm() {
    assertThat(SystemUtils.isHotSpotJvm()).isEqualTo(isJvmModel(SystemUtils.ORACLE_HOTSPOT_JVM_NAME));
  }

  @Test
  public void isJ9Jvm() {
    assertThat(SystemUtils.isJ9Jvm()).isEqualTo(isJvmModel(SystemUtils.IBM_J9_JVM_NAME));
  }

  @Test
  public void isJRockitJvm() {
    assertThat(SystemUtils.isJRockitJvm()).isEqualTo(isJvmModel(SystemUtils.ORACLE_JROCKIT_JVM_NAME));
  }

  @Test
  public void isLinux() {
    assertThat(SystemUtils.isLinux()).isEqualTo(isOs(SystemUtils.LINUX_OS_NAME));
  }

  @Test
  public void isMacOSX() {
    assertThat(SystemUtils.isMacOSX()).isEqualTo(isOs(SystemUtils.MAC_OSX_NAME));
  }

  @Test
  public void isWindow() {
    assertThat(SystemUtils.isWindows()).isEqualTo(isOs(SystemUtils.WINDOWS_OS_NAME));
  }

  @Test
  public void isUnixBasedOperatingSystem() {
    assertThat(SystemUtils.isUnixBasedOperatingSystem())
      .isEqualTo(isOs(SystemUtils.LINUX_OS_NAME) || isOs(SystemUtils.MAC_OSX_NAME));
  }

  @Test
  public void javaSystemClasspathIsCorrect() {
    assertThat(SystemUtils.javaSystemClasspath().orElse(null))
      .isEqualTo(System.getProperty("java.class.path", ""));
  }

  @Test
  public void classLoaderClasspathWithNonUrlClassLoader() {

    ClassLoader mockClassLoader = mock(ClassLoader.class);

    assertThat(SystemUtils.classLoaderClassPath(mockClassLoader)).isNotPresent();

    verifyNoInteractions(mockClassLoader);
  }

  @Test
  public void systemClassloaderClasspathContainsContents() {

    assumeThat(JavaVersion.current().isJava8()).isTrue();

    String systemClassLoaderClasspath = SystemUtils.systemClassLoaderClasspath().orElse(null);

    assertThat(systemClassLoaderClasspath)
      .describedAs("Expected non-blank classpath; but was [%s]", systemClassLoaderClasspath)
      .isNotBlank();
  }

  @Test
  public void threadContextClassloaderClasspathContainsContents() {

    assumeThat(JavaVersion.current().isJava8()).isTrue();

    String threadContextClassLoaderClasspath = SystemUtils.threadContextClassLoaderClasspath().orElse(null);

    assertThat(threadContextClassLoaderClasspath)
      .describedAs("Expected non-blank classpath; but was [%s]", threadContextClassLoaderClasspath)
      .isNotBlank();
  }

  @Test
  public void urlClassLoaderClasspathIsCorrect() throws MalformedURLException {

    URLClassLoader mockClassLoader = mock(URLClassLoader.class);

    URL urlOne = URI.create("file:///path/to/first.jar"). toURL();
    URL urlTwo = URI.create("file:///path/to/second.jar").toURL();

    doReturn(ArrayUtils.asArray(urlOne, null, urlTwo, null, null)).when(mockClassLoader).getURLs();

    String classloaderClasspath = SystemUtils.classLoaderClassPath(mockClassLoader).orElse(null);

    assertThat(classloaderClasspath).isNotNull();
    assertThat(classloaderClasspath)
      .isEqualTo("/path/to/first.jar" + System.getProperty("path.separator") + "/path/to/second.jar");

    verify(mockClassLoader, times(1)).getURLs();
    verifyNoMoreInteractions(mockClassLoader);
  }

  @Test
  public void urlClassLoaderClasspathWithNoClasspath() {

    URLClassLoader mockClassLoader = mock(URLClassLoader.class);

    doReturn(ArrayUtils.asArray((URL[]) null)).when(mockClassLoader).getURLs();

    assertThat(SystemUtils.classLoaderClassPath(mockClassLoader)).isNotPresent();

    verify(mockClassLoader, times(1)).getURLs();
    verifyNoMoreInteractions(mockClassLoader);
  }

  @Test
  public void urlClassLoaderClasspathWithSingleElementClasspathIsCorrect() throws MalformedURLException {

    URLClassLoader mockClassLoader = mock(URLClassLoader.class);

    URL url = URI.create("file:///path/to/lib.jar").toURL();

    doReturn(ArrayUtils.asArray(null, null, url, null)).when(mockClassLoader).getURLs();

    String classloaderClasspath = SystemUtils.classLoaderClassPath(mockClassLoader).orElse(null);

    assertThat(classloaderClasspath).isNotNull();
    assertThat(classloaderClasspath).isEqualTo("/path/to/lib.jar");

    verify(mockClassLoader, times(1)).getURLs();
    verifyNoMoreInteractions(mockClassLoader);
  }
}
