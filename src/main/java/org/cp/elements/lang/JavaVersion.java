/*
 * Copyright 2016 Author or Authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */
package org.cp.elements.lang;

import java.util.concurrent.atomic.AtomicReference;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.util.ArrayUtils;

/**
 * Java class to represent the current version of the JRE or JVM.
 *
 * @author John Blum
 * @see java.lang.Comparable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class JavaVersion implements Comparable<JavaVersion> {

  public static final JavaVersion ONE_ZERO = JavaVersion.of(1, 0, 0);
  public static final JavaVersion ONE_ONE = JavaVersion.of(1, 1, 0);
  public static final JavaVersion ONE_TWO = JavaVersion.of(1, 2, 0);
  public static final JavaVersion ONE_THREE = JavaVersion.of(1, 3, 0);
  public static final JavaVersion ONE_FOUR = JavaVersion.of(1, 4, 0);
  public static final JavaVersion FIVE = JavaVersion.of(1, 5, 0);
  public static final JavaVersion SIX = JavaVersion.of(1, 6, 0);
  public static final JavaVersion SEVEN = JavaVersion.of(1, 7, 0);
  public static final JavaVersion EIGHT = JavaVersion.of(1, 8, 0);
  public static final JavaVersion NINE = JavaVersion.of(9, 0, 0);
  public static final JavaVersion TEN = JavaVersion.of(10, 0, 0);
  public static final JavaVersion ELEVEN = JavaVersion.of(11, 0, 0);
  public static final JavaVersion TWELVE = JavaVersion.of(12, 0, 0);
  public static final JavaVersion THIRTEEN = JavaVersion.of(13, 0, 0);
  public static final JavaVersion FOURTEEN = JavaVersion.of(14, 0, 0);
  public static final JavaVersion FIFTEEN = JavaVersion.of(15, 0, 0);
  public static final JavaVersion SIXTEEN = JavaVersion.of(16, 0, 0);
  public static final JavaVersion SEVENTEEN = JavaVersion.of(17, 0, 0);

  protected static final int DEFAULT_VERSION_NUMBER = 0;
  protected static final int DEFAULT_BUILD_NUMBER = DEFAULT_VERSION_NUMBER;
  protected static final int DEFAULT_PATCH_VERSION = DEFAULT_VERSION_NUMBER;

  protected static final Integer ZERO = 0;

  protected static final String JAVA_VERSION_SYSTEM_PROPERTY = "java.version";

  private static final AtomicReference<JavaVersion> CURRENT = new AtomicReference<>(null);

  /**
   * Factory method used to construct a new instance of {@link JavaVersion} with
   * the given {@literal major}, {@literal minor} and {@literal patch} version numbers.
   *
   * @param major {@link Integer#TYPE} for the {@literal major} version number.
   * @param minor {@link Integer#TYPE} for the {@literal minor} version number.
   * @param patch {@link Integer#TYPE} for the {@literal patch} version number.
   * @return a new {@link JavaVersion}
   * @see #JavaVersion(int, int, int)
   */
  protected static JavaVersion of(int major, int minor, int patch) {
    return new JavaVersion(major, minor, patch) { };
  }

  /**
   * Factory method used to get the {@literal current} {@link JavaVersion} as determined by the JRE/JVM.
   *
   * @return the {@literal current} {@link JavaVersion}.
   */
  public static JavaVersion current() {
    return CURRENT.updateAndGet(currentJavaVersion -> currentJavaVersion != null ? currentJavaVersion
      : determineCurrentJavaVersion());
  }

  /**
   * Tries to determine the {@literal current} {@link JavaVersion} as determined by the JVM
   * and the {@literal java.version} {@link System#getProperties() System property}.
   *
   * @return the {@literal current} {@link JavaVersion} as determined by the JVM.
   * @see java.lang.System#getProperties()
   */
  private static JavaVersion determineCurrentJavaVersion() {

    String javaVersion = String.valueOf(System.getProperty(JAVA_VERSION_SYSTEM_PROPERTY));

    String[] javaVersionArray = ArrayUtils.nullSafeArray(javaVersion.split("\\."), String.class);

    int major = 0;
    int minor = 0;
    int patch = 0;

    if (javaVersionArray.length > 0) {
      major = parseInt(javaVersionArray[0]);
      if (javaVersionArray.length > 1) {
        minor = parseInt(javaVersionArray[1]);
        if (javaVersionArray.length > 2) {
          patch = parseInt(javaVersionArray[2]);
        }
      }
    }

    return JavaVersion.of(major, minor, patch);
  }

  // TODO: replace with StringUtils.getDigits(:String)
  private static String parseDigits(String value) {

    StringBuilder digits = new StringBuilder();

    for (char character : String.valueOf(value).toCharArray()) {
      if (Character.isDigit(character)) {
        digits.append(character);
      }
    }

    return digits.toString();
  }

  private static int parseInt(String value) {

    try {
      return Integer.parseInt(parseDigits(value));
    }
    catch (NumberFormatException ignore) {
      return DEFAULT_VERSION_NUMBER;
    }
  }

  private final Integer buildNumber;
  private final Integer major;
  private final Integer minor;
  private final Integer patch;

  /**
   * Constructs a new instance of {@link JavaVersion} initialized with
   * the {@literal major} and {@literal minor} version numbers.
   *
   * @param major {@link Integer#TYPE} for the {@literal major} version number.
   * @param minor {@link Integer#TYPE} for the {@literal minor} version number.
   * @throws IllegalArgumentException if any version number is not valid.
   * @see #JavaVersion(int, int, int)
   */
  protected JavaVersion(int major, int minor) {
    this(major, minor, DEFAULT_PATCH_VERSION);
  }

  /**
   * Constructs a new instance of {@link JavaVersion} initialized with the {@literal major}, {@literal minor}
   * and {@literal patch} version numbers.
   *
   * @param major {@link Integer#TYPE} for the {@literal major} version number.
   * @param minor {@link Integer#TYPE} for the {@literal minor} version number.
   * @param patch {@link Integer#TYPE} for the {@literal patch} version number.
   * @throws IllegalArgumentException if any version number is not valid.
   * @see #JavaVersion(int, int, int, int)
   */
  protected JavaVersion(int major, int minor, int patch) {
    this(major, minor, patch, DEFAULT_BUILD_NUMBER);
  }

  /**
   * Constructs a new instance of {@link JavaVersion} initialized with the given {@literal major}, {@literal minor},
   * and {@literal patch} version numbers along with a {@literal build number}.
   *
   * @param major {@link Integer#TYPE} for the {@literal major} version number.
   * @param minor {@link Integer#TYPE} for the {@literal minor} version number.
   * @param patch {@link Integer#TYPE} for the {@literal patch} version number.
   * @param buildNumber {@link Integer#TYPE} for the {@literal build number}.
   * @throws IllegalArgumentException if any version number is not valid.
   * @see #validateVersionNumber(int)
   */
  protected JavaVersion(int major, int minor, int patch, int buildNumber) {

    this.major = validateVersionNumber(major);
    this.minor = validateVersionNumber(minor);
    this.patch = validateVersionNumber(patch);
    this.buildNumber = buildNumber;
  }

  private int validateVersionNumber(int version) {

    Assert.isTrue(version > -1,
      () -> String.format("Version number [%d] must be greater than equal to 0", version));

    return version;
  }

  /**
   * Determines whether {@literal this} {@link JavaVersion} is {@literal Java 8}.
   *
   * @return a boolean value indicating whether {@literal this} {@link JavaVersion} is {@literal Java 8}.
   * @see #getMajor()
   */
  public boolean isJava8() {
    return EIGHT.getMajor().equals(getMajor());
  }

  /**
   * Determines whether {@literal this} {@link JavaVersion} is {@literal Java 1}.
   *
   * @return a boolean value indicating whether {@literal this} {@link JavaVersion} is {@literal Java 11}.
   * @see #getMajor()
   */
  public boolean isJava11() {
    return ELEVEN.getMajor().equals(getMajor());
  }

  /**
   * Determines whether {@literal this} {@link JavaVersion} is {@literal Java 17}.
   *
   * @return a boolean value indicating whether {@literal this} {@link JavaVersion} is {@literal Java 17}.
   * @see #getMajor()
   */
  public boolean isJava17() {
    return SEVENTEEN.getMajor().equals(getMajor());
  }

  /**
   * Determines whether {@literal this} {@link JavaVersion} is newer than or equal to the given {@link JavaVersion}.
   *
   * {@literal This} {@link JavaVersion} is considered newer if it comes after the given {@link JavaVersion};
   * that is {@literal this} {@link JavaVersion} has a higher version number (for example: 2.0.0 vs 1.0.0).
   *
   * @param javaVersion {@link JavaVersion} to compare with {@literal this} {@link JavaVersion}.
   * @return a boolean value indicating whether {@literal this} {@link JavaVersion} is newer than or equal to
   * the given {@link JavaVersion}.
   * @see #isOlderThan(JavaVersion)
   */
  @NullSafe
  public boolean isNewerThanOrEqualTo(@Nullable JavaVersion javaVersion) {
    return javaVersion != null && this.compareTo(javaVersion) >= 0;
  }

  /**
   * Determines whether {@literal this} {@link JavaVersion} is older than the given {@link JavaVersion}.
   *
   * {@literal This} {@link JavaVersion} is considered older if it comes before the given {@link JavaVersion};
   * that is {@literal this} {@link JavaVersion} has a lower version number (for example: 1.0.0 vs 2.0.0).
   *
   * @param javaVersion {@link JavaVersion} to compare with {@literal this} {@link JavaVersion}.
   * @return a boolean value indicating whether {@literal this} {@link JavaVersion} is older than
   * the given {@link JavaVersion}.
   * @see #isNewerThanOrEqualTo(JavaVersion)
   */
  @NullSafe
  public boolean isOlderThan(@Nullable JavaVersion javaVersion) {
    return javaVersion != null && this.compareTo(javaVersion) < 0;
  }

  /**
   * Determines whether {@literal this} {@literal JavaVersion} is {@literal undetermined}.
   *
   * A {@link JavaVersion} is {@literal undetermined} if the {@literal major}, {@literal minor} and {@literal patch}
   * version numbers are all {@literal 0}.
   *
   * @return a boolean value indicating whether {@literal this} {@literal JavaVersion} is {@literal undetermined}.
   * @see #getMajor()
   * @see #getMinor()
   * @see #getPatch()
   */
  public boolean isUndetermined() {

    return ZERO.equals(getMajor())
      && ZERO.equals(getMinor())
      && ZERO.equals(getPatch());
  }

  /**
   * Gets the {@literal major} version number.
   *
   * @return the {@literal major} version number.
   */
  public @NotNull Integer getMajor() {
    return this.major;
  }

  /**
   * Gets the {@literal minor} version number.
   *
   * @return the {@literal minor} version number.
   */
  public @NotNull Integer getMinor() {
    return this.minor;
  }

  /**
   * Gets the {@literal patch} version number.
   *
   * @return the {@literal patch} version number.
   */
  public @NotNull Integer getPatch() {
    return this.patch;
  }

  /**
   * Gets the {@literal build number}.
   *
   * @return the {@literal build number}.
   */
  public @NotNull Integer getBuildNumber() {
    return this.buildNumber;
  }

  /**
   * Compares {@literal this} {@link JavaVersion} to the given, required {@link JavaVersion} to determine which version
   * of Java came first.
   *
   * @return an {@link Integer} less than {@literal 0} if {@literal this} {@link JavaVersion} came before the given,
   * required {@link JavaVersion}, greater than {@literal 0} if this {@link JavaVersion} comes after, or {@literal 0}
   * if {@literal this} {@link JavaVersion} and the given, required {@link JavaVersion} are the same.
   * @see java.lang.Comparable#compareTo(Object)
   */
  @Override
  public int compareTo(@NotNull JavaVersion version) {

    int result = getMajor().compareTo(version.getMajor());

    result = result != 0 ? result
      : getMinor().compareTo(version.getMinor());

    result = result != 0 ? result
      : getPatch().compareTo(version.getPatch());

    return result;
  }

  /**
   * Determines whether {@literal this} {@link JavaVersion} is equal to the given {@link Object}.
   *
   * @param obj {@link Object} to evaluate in the equality expression.
   * @return a boolean value indicating whether {@literal this} {@link JavaVersion} is equal to
   * the given {@link Object}.
   * @see java.lang.Object#equals(Object)
   */
  @Override
  public boolean equals(Object obj) {

    if (this == obj) {
      return true;
    }

    if (!(obj instanceof JavaVersion)) {
      return false;
    }

    JavaVersion that = (JavaVersion) obj;

    return this.getMajor().equals(that.getMajor())
      && this.getMinor().equals(that.getMinor())
      && this.getPatch().equals(that.getPatch());
  }

  /**
   * Computes a hash code for {@literal this} {@link JavaVersion}.
   *
   * @return a computed hash code for {@literal this} {@link JavaVersion}.
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode() {

    int hashValue = 17;

    hashValue = 37 * hashValue + ObjectUtils.hashCode(getMajor());
    hashValue = 37 * hashValue + ObjectUtils.hashCode(getMinor());
    hashValue = 37 * hashValue + ObjectUtils.hashCode(getPatch());

    return hashValue;
  }

  /**
   * Returns a {@link String} representing {@literal this} {@link JavaVersion}.
   *
   * @return a {@link String} describing {@literal this} {@link JavaVersion}.
   * @see java.lang.Object#toString()
   */
  @Override
  public @NotNull String toString() {
    return String.format("%1$s.%2$s.%3$s", getMajor(), getMinor(), getPatch());
  }
}
