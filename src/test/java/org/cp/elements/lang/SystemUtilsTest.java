/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 *
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 *
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 *
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 *
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.lang;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

import java.lang.management.ManagementFactory;

import org.junit.Test;

/**
 * The SystemUtilsTest class is a test suite of test cases testing the contract and functionality
 * of the SystemUtils class.
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
public class SystemUtilsTest {

  @Test
  public void isJavaVersionAtLeast180() {
    assertThat(SystemUtils.isJavaVersionAtLeast("1.8.0_0"), is(true));
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
    assertThat(SystemUtils.isAppleJvm(), is(equalTo(isJvmMake(SystemUtils.APPLE_JVM_VENDOR))));
  }

  @Test
  public void isIBMJvm() {
    assertThat(SystemUtils.isIBMJvm(), is(equalTo(isJvmMake(SystemUtils.IBM_JVM_VENDOR))));
  }

  @Test
  public void isOracleJvm() {
    assertThat(SystemUtils.isOracleJvm(), is(equalTo(isJvmMake(SystemUtils.ORACLE_JVM_VENDOR))));
  }

  @Test
  public void isHotSpotJvm() {
    assertThat(SystemUtils.isHotSpotJvm(), is(equalTo(isJvmModel(SystemUtils.ORACLE_HOTSPOT_JVM_NAME))));
  }

  @Test
  public void isJ9Jvm() {
    assertThat(SystemUtils.isJ9Jvm(), is(equalTo(isJvmModel(SystemUtils.IBM_J9_JVM_NAME))));
  }

  @Test
  public void isJRockitJvm() {
    assertThat(SystemUtils.isJRockitJvm(), is(equalTo(isJvmModel(SystemUtils.ORACLE_JROCKIT_JVM_NAME))));
  }

  @Test
  public void isLinux() {
    assertThat(SystemUtils.isLinux(), is(equalTo(isOs(SystemUtils.LINUX_OS_NAME))));
  }

  @Test
  public void isMacOSX() {
    assertThat(SystemUtils.isMacOSX(), is(equalTo(isOs(SystemUtils.MAC_OSX_NAME))));
  }

  @Test
  public void isWindow() {
    assertThat(SystemUtils.isWindows(), is(equalTo(isOs(SystemUtils.WINDOWS_OS_NAME))));
  }

}
