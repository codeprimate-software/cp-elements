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

/**
 * SystemUtils is an abstract utility class designed to interact with and access properties of the Java System class.
 *
 * @author John J. Blum
 * @see java.lang.System
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class SystemUtils {

  // Present Working Directory (PWD)
  public static final String CURRENT_DIRECTORY = System.getProperty("user.dir");

  // Java Virtual Machine (JVM) Names
  public static final String IBM_J9_JVM_NAME = "J9";
  public static final String ORACLE_HOTSPOT_JVM_NAME = "HotSpot";
  public static final String ORACLE_JROCKIT_JVM_NAME = "JRockit";

  // Java Virtual Machine (JVM) Vendor Names
  public static final String APPLE_JVM_VENDOR = "Apple";
  public static final String IBM_JVM_VENDOR = "IBM";
  public static final String ORACLE_JVM_VENDOR = "Oracle";

  // Operating System Names
  public static final String LINUX_OS_NAME = "Linux";
  public static final String MAC_OSX_NAME = "Mac";
  public static final String WINDOWS_OS_NAME = "Windows";

  /**
   * Utility method to determine if the installed Java Runtime Environment (JRE) meets the minimum expected version.
   * Java versions are typically of the form "1.8.0_66".  The Java version is determined by the "java.version"
   * System property.
   *
   * @param expectedVersion a numerical String value specifying the expected minimum version
   * of the installed Java Runtime Environment.
   * @return a boolean value indicating if the Java Runtime Environment meets the expected minimum version requirement.
   * @see java.lang.System#getProperty(String)
   */
  public static boolean isJavaVersionAtLeast(final String expectedVersion) {
    String actualVersionDigits = StringUtils.getDigits(System.getProperty("java.version"));

    String expectedVersionDigits = StringUtils.pad(StringUtils.getDigits(expectedVersion), '0',
      actualVersionDigits.length());

    try {
      return (Long.parseLong(actualVersionDigits) >= Long.parseLong(expectedVersionDigits));
    }
    catch (NumberFormatException ignore) {
      return false;
    }
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
  public static boolean isIBMJvm() {
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
  protected static boolean isJvmMake(final String expectedJvmVendor) {
    String jvmVendor = System.getProperty("java.vm.vendor");
    return (jvmVendor != null && jvmVendor.contains(expectedJvmVendor));
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
  protected static boolean isJvmModel(final String expectedJvmName) {
    String jvmName = System.getProperty("java.vm.name");
    return (jvmName != null && jvmName.contains(expectedJvmName));
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
   * Determines wheehr this Operating System is the expected Operating System environment.  The current OS is determined
   * by the "os.name" System property.
   *
   * @param expectedOsName a String indicating the name of the expected OS environment.
   * @return a boolean value indicating whether this OS is the expected OS.
   * @see java.lang.System#getProperty(String)
   */
  protected static boolean isOS(final String expectedOsName) {
    String osName = System.getProperty("os.name");
    return (osName != null && osName.contains(expectedOsName));
  }

}
