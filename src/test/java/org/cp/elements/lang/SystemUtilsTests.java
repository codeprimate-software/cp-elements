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

import java.lang.management.ManagementFactory;

import org.junit.Test;

/**
 * Unit tests for {@link SystemUtils}.
 *
 * @author John J. Blum
 * @see java.lang.System
 * @see java.lang.management.ManagementFactory
 * @see java.lang.management.OperatingSystemMXBean
 * @see java.lang.management.RuntimeMXBean
 * @see org.junit.Test
 * @see org.cp.elements.lang.SystemUtils
 * @since 1.0.0
 */
public class SystemUtilsTests {

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
}
