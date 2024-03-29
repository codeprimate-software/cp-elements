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

import static org.cp.elements.lang.LangExtensions.assertThat;
import static org.cp.elements.lang.ObjectUtils.returnFirstNonNullValue;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.util.ComparatorResultBuilder;

/**
 * Abstract Data Type (ADT) modeling a {@literal software version number}.
 *
 * @author John Blum
 * @see java.time.LocalDateTime
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class Version implements Comparable<Version> {

  private static final String VERSION_TO_STRING = "%1$d.%2$d.%3$d%4$s%5$s%6$s";

  protected static final int DEFAULT_BUILD_NUMBER = 0;
  protected static final int DEFAULT_QUALIFIER_NUMBER = 0;
  protected static final int DEFAULT_VERSION_NUMBER = 0;

  protected static final String RELEASE_DATE_TIME_FORMAT = "yyyy-MMMM-dd-HH-mm-ss";
  protected static final String VERSION_NUMBER_SEPARATOR = "\\.";

  /**
   * Factory method used to construct a new {@link Version} initialized with major and minor version numbers.
   *
   * @param major {@link Integer major version number}.
   * @param minor {@link Integer minor version number}.
   * @return a new {@link Version} initialized with the {@link Integer major} and {@link Integer minor} version numbers.
   * @throws IllegalArgumentException if {@code major} or {@code minor} version numbers are less than {@literal 0}.
   * @see org.cp.elements.lang.Version
   * @see #from(int, int, int, Qualifier, int)
   */
  public static @NotNull Version from(int major, int minor) {
    return from(major, minor, DEFAULT_VERSION_NUMBER, Qualifier.UNDEFINED, DEFAULT_QUALIFIER_NUMBER);
  }

  /**
   * Factory method used to construct a new {@link Version} initialized with major, minor and maintenance
   * version numbers.
   *
   * @param major {@link Integer major version number}.
   * @param minor {@link Integer minor version number}.
   * @param maintenance {@link Integer maintenance version number}.
   * @return a new {@link Version} initialized with the {@link Integer major}, {@link Integer minor}
   * and {@link Integer maintenance} version numbers.
   * @throws IllegalArgumentException if {@code major}, {@code minor} or {@code maintenance} version numbers
   * are less than {@literal 0}.
   * @see org.cp.elements.lang.Version
   * @see #from(int, int, int, Qualifier, int)
   */
  public static @NotNull Version from(int major, int minor, int maintenance) {
    return from(major, minor, maintenance, Qualifier.UNDEFINED, DEFAULT_QUALIFIER_NUMBER);
  }

  /**
   * Factory method used to construct a new {@link Version} initialized with major, minor and maintenance
   * version numbers along with a version qualifier (for example: {@literal RELEASE}).
   *
   * @param major {@link Integer major version number}.
   * @param minor {@link Integer minor version number}.
   * @param maintenance {@link Integer maintenance version number}.
   * @param qualifier version {@link Qualifier} such as {@literal M# (Milestone)}, {@literal RC# (Release Candidate)}
   * or {@literal RELEASE}.
   * @return a new {@link Version} initialized with the {@link Integer major}, {@link Integer minor}
   * and {@link Integer maintenance} version numbers along with a version {@link Qualifier}.
   * @throws IllegalArgumentException if {@code major}, {@code minor} or {@code maintenance} version numbers
   * are less than {@literal 0}.
   * @see org.cp.elements.lang.Version.Qualifier
   * @see org.cp.elements.lang.Version
   * @see #from(int, int, int, Qualifier, int)
   */
  public static @NotNull Version from(int major, int minor, int maintenance, Qualifier qualifier) {
    return from(major, minor, maintenance, qualifier, DEFAULT_QUALIFIER_NUMBER);
  }

  /**
   * Factory method used to construct a new {@link Version} initialized with major, minor and maintenance
   * version numbers along with a version qualifier (for example: {@literal RELEASE}) and a qualifier number.
   *
   * @param major {@link Integer major version number}.
   * @param minor {@link Integer minor version number}.
   * @param maintenance {@link Integer maintenance version number}.
   * @param qualifier version {@link Qualifier} such as {@literal M# (Milestone)}, {@literal RC# (Release Candidate)}
   * or {@literal RELEASE}.
   * @param qualifierNumber {@link Integer qualifier version number} such as {@literal M1 (Milestone 1)}.
   * @return a new {@link Version} initialized with the {@link Integer major}, {@link Integer minor}
   * and {@link Integer maintenance} version numbers along with a version {@link Qualifier}
   * and {@link Integer qualifier number}.
   * @throws IllegalArgumentException if {@code major}, {@code minor} or {@code maintenance} version numbers
   * are less than {@literal 0}.
   * @see #Version(int, int, int, Qualifier, int)
   * @see org.cp.elements.lang.Version.Qualifier
   * @see org.cp.elements.lang.Version
   */
  public static @NotNull Version from(int major, int minor, int maintenance, Qualifier qualifier, int qualifierNumber) {
    return new Version(major, minor, maintenance, qualifier, qualifierNumber);
  }

  /**
   * Factory method used to parse a {@link String version} into a fully-qualified instance of {@link Version}.
   *
   * @param version {@link String} representation of the version; must not be {@literal null} or {@literal empty}.
   * @return a new {@link Version} initialized from the given, required {@link String version}.
   * @throws IllegalArgumentException if the {@link String version} is {@literal null} or {@literal empty},
   * or the {@link String version} could not be parsed into individual version elements,
   * or the {@link String version} is unrecognizable.
   * @see #parseMajorMinorMaintenanceQualifier(String[])
   * @see #parseMajorMinorMaintenance(String[])
   * @see #parseMajorMinor(String[])
   */
  public static @NotNull Version parse(@NotNull String version) {

    Assert.hasText(version, "The version [%s] to parse is required", version);

    String[] versionNumbers = version.split(VERSION_NUMBER_SEPARATOR);

    switch (versionNumbers.length) {
      case 1 -> {
        String[] majorMinorVersionNumbers = {
          versionNumbers[0], String.valueOf(DEFAULT_VERSION_NUMBER)
        };
        return parseMajorMinor(majorMinorVersionNumbers);
      }
      case 2 -> {
        return parseMajorMinor(versionNumbers);
      }
      case 3 -> {
        return parseMajorMinorMaintenance(versionNumbers);
      }
      case 4 -> {
        return parseMajorMinorMaintenanceQualifier(versionNumbers);
      }
      default -> throw new IllegalArgumentException(String.format("Unrecognized format for version [%s]", version));
    }
  }

  /**
   * Parses the given {@link String} into an {@link Integer}.
   *
   * @param value {@link String} to parse as an {@link Integer}.
   * @return an {@link Integer} from the given {@link String}.
   * @throws NumberFormatException if the {@link String} is not parse-able as an {@link Integer}.
   * @see org.cp.elements.lang.StringUtils#getDigits(String)
   * @see java.lang.Integer#parseInt(String)
   */
  private static int parseInt(@NotNull String value) {
    return Integer.parseInt(StringUtils.getDigits(value));
  }

  /**
   * Parses the major and minor version numbers from the {@code versionNumbers} array.
   *
   * @param versionNumbers array of version numbers to parse.
   * @return an instance of the {@link Version} initialized with the major and minor version numbers.
   * @see org.cp.elements.lang.Version
   * @see #from(int, int)
   */
  private static @NotNull Version parseMajorMinor(@NotNull String[] versionNumbers) {
    return from(parseInt(versionNumbers[0]), parseInt(versionNumbers[1]));
  }

  /**
   * Parses the major, minor and maintenance version numbers from the {@code versionNumbers} array.
   *
   * @param versionNumbers array of version numbers to parse.
   * @return an instance of the {@link Version} initialized with the major, minor and maintenance version numbers.
   * @see org.cp.elements.lang.Version
   * @see #from(int, int, int)
   */
  private static @NotNull Version parseMajorMinorMaintenance(@NotNull String[] versionNumbers) {
    return from(parseInt(versionNumbers[0]), parseInt(versionNumbers[1]), parseInt(versionNumbers[2]));
  }

  /**
   * Parses the major, minor and maintenance version numbers along with the version qualifier
   * from the {@code versionNumbers} array.
   *
   * @param versionNumbers array of version numbers to parse.
   * @return an instance of the {@link Version} initialized with the major, minor and maintenance version numbers
   * along with the version qualifier.
   * @see org.cp.elements.lang.Version
   * @see #from(int, int, int, Qualifier, int)
   */
  private static @NotNull Version parseMajorMinorMaintenanceQualifier(@NotNull String[] versionNumbers) {
    return from(parseInt(versionNumbers[0]), parseInt(versionNumbers[1]), parseInt(versionNumbers[2]),
      parseQualifier(versionNumbers[3]), parseQualifierNumber(versionNumbers[3]));
  }

  /**
   * Parses the {@link String version qualifier} into an instance of {@link Version.Qualifier}.
   *
   * @param qualifier {@link String} containing the version qualifier.
   * @return a {@link Version.Qualifier} instance representing the given {@link String}.
   * @see org.cp.elements.lang.Version.Qualifier
   */
  private static @NotNull Qualifier parseQualifier(@Nullable String qualifier) {
    return Qualifier.parse(qualifier);
  }

  /**
   * Parses the {@link Version.Qualifier} number.
   *
   * @param qualifier {@link String} containing the version qualifier.
   * @return an integer from the version qualifier {@link String}, or {@literal 0} if the version qualifier
   * {@link String} contained no numeric values.
   * @see #parseInt(String)
   */
  private static int parseQualifierNumber(@Nullable String qualifier) {

    try {
      return parseInt(qualifier);
    }
    catch (NumberFormatException ignore) {
      return 0;
    }
  }

  private int buildNumber = DEFAULT_BUILD_NUMBER;
  private int qualifierNumber = DEFAULT_QUALIFIER_NUMBER;

  private final int major;
  private final int minor;
  private final int maintenance;

  private LocalDateTime releaseDateTime;

  private Qualifier qualifier = Qualifier.UNDEFINED;

  /**
   * Constructs a new {@link Version} initialized with major and minor version numbers.
   *
   * @param major {@link Integer major version number}.
   * @param minor {@link Integer minor version number}.
   * @throws IllegalArgumentException if {@code major} or {@code minor} version numbers
   * are less than {@literal 0}.
   * @see #Version(int, int, int, Qualifier, int)
   */
  public Version(int major, int minor) {
    this(major, minor, 0, Qualifier.UNDEFINED, DEFAULT_QUALIFIER_NUMBER);
  }

  /**
   * Constructs a new {@link Version} initialized with major, minor and maintenance version numbers.
   *
   * @param major {@link Integer major version number}.
   * @param minor {@link Integer minor version number}.
   * @param maintenance {@link Integer maintenance version number}.
   * @throws IllegalArgumentException if {@code major}, {@code minor} or {@code maintenance} version numbers
   * are less than {@literal 0}.
   * @see #Version(int, int, int, Qualifier, int)
   */
  public Version(int major, int minor, int maintenance) {
    this(major, minor, maintenance, Qualifier.UNDEFINED, DEFAULT_QUALIFIER_NUMBER);
  }

  /**
   * Constructs a new {@link Version} initialized with major, minor and maintenance version numbers
   * along with the version {@link Qualifier}.
   *
   * @param major {@link Integer major version number}.
   * @param minor {@link Integer minor version number}.
   * @param maintenance {@link Integer maintenance version number}.
   * @param qualifier version {@link Qualifier}.
   * @throws IllegalArgumentException if {@code major}, {@code minor} or {@code maintenance} version numbers
   * are less than {@literal 0}.
   * @see #Version(int, int, int, Qualifier, int)
   */
  public Version(int major, int minor, int maintenance, Qualifier qualifier) {
    this(major, minor, maintenance, qualifier, DEFAULT_VERSION_NUMBER);
  }

  /**
   * Constructs a new {@link Version} initialized with major, minor and maintenance version numbers
   * along with a version {@link Qualifier} and qualifier number.
   *
   * @param major {@link Integer major version number}.
   * @param minor {@link Integer minor version number}.
   * @param maintenance {@link Integer maintenance version number}.
   * @param qualifier version {@link Qualifier}.
   * @param qualifierNumber {@link String qualifier number}, for example:  {@literal M1}, {@literal RC2} and so on.
   * @throws IllegalArgumentException if {@code major}, {@code minor} or {@code maintenance} version numbers
   * are less than {@literal 0}.
   */
  public Version(int major, int minor, int maintenance, Qualifier qualifier, int qualifierNumber) {

    assertThat(major).throwing(new IllegalArgumentException(String.format(
      "Major version [%d] must be greater than equal to 0", major)))
        .isGreaterThanEqualTo(0);

    assertThat(minor).throwing(new IllegalArgumentException(String.format(
      "Minor version [%d] must be greater than equal to 0", minor)))
        .isGreaterThanEqualTo(0);

    assertThat(maintenance).throwing(new IllegalArgumentException(String.format(
      "Maintenance version [%d] must be greater than equal to 0", maintenance)))
        .isGreaterThanEqualTo(0);

    this.major = major;
    this.minor = minor;
    this.maintenance = maintenance;
    this.qualifier = returnFirstNonNullValue(qualifier, Qualifier.UNDEFINED);
    this.qualifierNumber = Math.max(qualifierNumber, 0);
  }

  /**
   * Determines whether this {@link Version} represents a Milestone version.
   *
   * @return a boolean value indicating whether this is a Milestone {@link Version}.
   * @see org.cp.elements.lang.Version.Qualifier#isMilestone()
   * @see #getQualifier()
   */
  public boolean isMilestone() {
    return getQualifier().isMilestone();
  }

  /**
   * Determines whether this {@link Version} represents a Release Candidate version.
   *
   * @return a boolean value indicating whether this is a Release Candidate {@link Version}.
   * @see org.cp.elements.lang.Version.Qualifier#isReleaseCandidate()
   * @see #getQualifier()
   */
  public boolean isReleaseCandidate() {
    return getQualifier().isReleaseCandidate();
  }

  /**
   * Determines whether this {@link Version} represents a Release version.
   *
   * @return a boolean value indicating whether this is a Release {@link Version}.
   * @see org.cp.elements.lang.Version.Qualifier#isRelease()
   * @see #getQualifier()
   */
  public boolean isRelease() {
    return getQualifier().isRelease();
  }

  /**
   * Determines whether this {@link Version} represents a Snapshot version.
   *
   * @return a boolean value indicating whether this is a Snapshot {@link Version}.
   * @see org.cp.elements.lang.Version.Qualifier#isSnapshot()
   * @see #getQualifier()
   */
  public boolean isSnapshot() {
    return getQualifier().isSnapshot();
  }

  /**
   * Determines whether this {@link Version} is unqualified.
   *
   * @return a boolean value indicating whether this is an unqualified {@link Version}.
   * @see org.cp.elements.lang.Version.Qualifier#isUndefined()
   * @see #getQualifier()
   */
  public boolean isUndefined() {
    return getQualifier().isUndefined();
  }

  /**
   * Returns the build number for this {@link Version}.
   *
   * @return the software build number for this {@link Version}.
   */
  public int getBuildNumber() {
    return Math.max(this.buildNumber, 0);
  }

  /**
   * Returns the {@link Integer major version number} for this {@link Version}.
   *
   * @return the major version number for this {@link Version}.
   */
  public int getMajor() {
    return this.major;
  }

  /**
   * Returns the minor version number for this {@link Version}.
   *
   * @return the minor version number for this {@link Version}.
   */
  public int getMinor() {
    return this.minor;
  }

  /**
   * Returns the maintenance version number for this {@link Version}.
   *
   * @return the maintenance version number for this {@link Version}.
   */
  public int getMaintenance() {
    return this.maintenance;
  }

  /**
   * Returns this {@link Version Version's} {@link Version.Qualifier}.
   *
   * @return the {@link Qualifier} for this {@link Version}.
   * @see org.cp.elements.lang.Version.Qualifier
   * @see #getQualifierNumber()
   */
  public @NotNull Qualifier getQualifier() {
    return returnFirstNonNullValue(this.qualifier, Qualifier.UNDEFINED);
  }

  /**
   * Returns the {@link Version} qualifier number.
   *
   * @return the {@link Qualifier} number for this {@link Version}
   * @see org.cp.elements.lang.Version.Qualifier
   * @see #getQualifier()
   */
  public int getQualifierNumber() {
    return Math.max(this.qualifierNumber, 0);
  }

  /**
   * Returns the release date and time for this {@link Version}.
   *
   * @return the date and time on which this {@link Version} was released.
   * @see java.time.LocalDateTime
   */
  public @Nullable LocalDateTime getReleaseDateTime() {
    return this.releaseDateTime;
  }

  /**
   * Compares this {@link Version} to the given {@link Version} to determine relative ordering.
   * <p>
   * In this case, the {@link Comparable#compareTo(Object)} method determines earlier and later
   * {@link Version versions}.
   *
   * @param version given {@link Version} used in the comparison.
   * @return an integer indicating the sort order, or order of precedence.
   * @see org.cp.elements.util.ComparatorResultBuilder
   * @see #getMajor()
   * @see #getMinor()
   * @see #getMaintenance()
   * @see #getQualifier()
   * @see #getQualifierNumber()
   */
  @Override
  public int compareTo(@NotNull Version version) {

    return ComparatorResultBuilder.<Integer>create()
      .doCompare(this.getMajor(), version.getMajor())
      .doCompare(this.getMinor(), version.getMinor())
      .doCompare(this.getMaintenance(), version.getMaintenance())
      .doCompare(this.getQualifier().ordinal(), version.getQualifier().ordinal())
      .doCompare(this.getQualifierNumber(), version.getQualifierNumber())
      .getResult();
  }

  /**
   * Determines whether this {@link Version} is equal to the given {@link Object}.
   *
   * @param obj {@link Object} to compare for equality with this {@link Version}.
   * @return a boolean value indicating whether this {@link Version} is equal to the given {@link Object}.
   * @see java.lang.Object#equals(Object)
   */
  @Override
  public boolean equals(Object obj) {

    if (this == obj) {
      return true;
    }

    if (!(obj instanceof Version that)) {
      return false;
    }

    return ObjectUtils.equals(this.getBuildNumber(), that.getBuildNumber())
      && ObjectUtils.equals(this.getMajor(), that.getMajor())
      && ObjectUtils.equals(this.getMinor(), that.getMinor())
      && ObjectUtils.equals(this.getMaintenance(), that.getMaintenance())
      && ObjectUtils.equals(this.getQualifier(), that.getQualifier())
      && ObjectUtils.equals(this.getQualifierNumber(), that.getQualifierNumber());
  }

  /**
   * Computes the hash code of this {@link Version}.
   *
   * @return the computed hash code for this {@link Version}.
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode() {
    return ObjectUtils.hashCodeOf(getBuildNumber(), getMajor(), getMinor(), getMaintenance(),
      getQualifier(), getQualifierNumber());
  }

  /**
   * Returns a {@link String} representation (view) of this {@link Version}.
   *
   * @return a {@link String} describing this {@link Version}.
   * @see java.lang.Object#toString()
   */
  @Override
  public @NotNull String toString() {
    return String.format(VERSION_TO_STRING, getMajor(), getMinor(), getMaintenance(),
      toQualifierString(), toBuildNumberString(), toReleaseDateTimeString());
  }

  /**
   * Returns a {@link String} containing the software build number for this {@link Version}.
   *
   * @return a {@link String} containing the software build number for this {@link Version}, or an empty {@link String}
   * if the software build number was unspecified.
   * @see #getBuildNumber()
   */
  private @NotNull String toBuildNumberString() {

    int buildNumber = getBuildNumber();

    return buildNumber > 0 ? String.format(" build %d", buildNumber) : StringUtils.EMPTY_STRING;
  }

  /**
   * Returns a {@link String} containing the version {@link Qualifier} and qualifier number of this {@link Version}.
   *
   * @return a {@link String} containing the {@link Version.Qualifier} and qualifier number, or an empty {@link String}
   * if the {@link Version.Qualifier} is undefined.
   * @see #getQualifierNumber()
   * @see #getQualifier()
   */
  private @NotNull String toQualifierString() {

    Qualifier qualifier = getQualifier();

    int qualifierNumber = getQualifierNumber();

    return qualifier.isUndefined() ? StringUtils.EMPTY_STRING
      : (qualifierNumber > 0 ? String.format(".%s%d", qualifier.getAbbreviation(), qualifierNumber)
        : String.format(".%s", qualifier.name()));
  }

  /**
   * Returns a {@link String} containing the release date and time for this {@link Version}.
   *
   * @return a {@link String} containing the software release date and time for this {@link Version},
   * or an empty {@link String} if the software release date and time is unknown.
   */
  private @NotNull String toReleaseDateTimeString() {

    LocalDateTime releaseDateTime = getReleaseDateTime();

    return releaseDateTime == null ? StringUtils.EMPTY_STRING
      : String.format(" on %s", releaseDateTime.format(DateTimeFormatter.ofPattern(RELEASE_DATE_TIME_FORMAT)));
  }

  /**
   * Sets the software release date and time for this {@link Version}.
   *
   * @param releaseDateTime {@link LocalDateTime} indicating the software release date and time to set
   * for this {@link Version}.
   * @return this {@link Version} reference.
   */
  public @NotNull Version on(@Nullable LocalDateTime releaseDateTime) {
    this.releaseDateTime = releaseDateTime;
    return this;
  }

  /**
   * Sets the software build number for this {@link Version}.
   *
   * @param buildNumber software build number to set for this {@link Version}.
   * @return this {@link Version} reference.
   */
  public @NotNull Version with(int buildNumber) {
    this.buildNumber = Math.max(buildNumber, 0);
    return this;
  }

  /**
   * Sets the {@link Version.Qualifier} for this {@link Version}.
   *
   * @param qualifier {@link Version.Qualifier}.
   * @return this {@link Version} reference.
   * @see #with(Qualifier, int)
   */
  public @NotNull Version with(@Nullable Qualifier qualifier) {
    return with(qualifier, 0);
  }

  /**
   * Sets the {@link Version.Qualifier} for this {@link Version}.
   *
   * @param qualifier {@link Version.Qualifier}.
   * @param qualifierNumber {@link Version.Qualifier} number.
   * @return this {@link Version} reference.
   */
  public @NotNull Version with(@Nullable Qualifier qualifier, int qualifierNumber) {
    this.qualifier = returnFirstNonNullValue(qualifier, Qualifier.UNDEFINED);
    this.qualifierNumber = Math.max(qualifierNumber, 0);
    return this;
  }

  /**
   * The {@link Qualifier} enumerated type is a {@link Version} qualifier,
   * such as M# (Milestone), RC# (Release Candidate) or RELEASE, etc.
   */
  public enum Qualifier {

    ALPHA("ALPHA", "Alpha"),
    BETA("BETA", "Beta"),
    BUILD_SNAPSHOT("BUILD-SNAPSHOT", "Build Snapshot"),
    ITERATION("IT", "Iteration"),
    MILESTONE("M", "Milestone"),
    RELEASE_CANDIDATE("RC", "Release Candidate"),
    RELEASE("RELEASE", "Release"),
    SNAPSHOT("SNAPSHOT", "Snapshot"),
    UNDEFINED("UNDEFINED", "Undefined");

    /**
     * Factory method to search for a {@link Qualifier} enumerated version matching the {@code version} that contains
     * the matching {@link Qualifier Qualifier's} abbreviation.
     *
     * @param version {@link String} version for which to resolve the {@link Qualifier}.
     * @return a {@link Qualifier} matching the {@link String} version or {@literal null} if no match was found.
     * @see org.cp.elements.lang.Version.Qualifier
     * @see #getAbbreviation()
     */
    public static @NotNull Qualifier parse(@Nullable String version) {

      if (StringUtils.hasText(version)) {

        String trimmedLowerCaseVersion = version.trim().toLowerCase();
        String versionQualifier = version.substring(version.lastIndexOf(".") + 1);

        for (Qualifier qualifier : values()) {

          String qualifierName = qualifier.name().toLowerCase();
          String alternateQualifierName = qualifierName.replace("_", "-");

          if (trimmedLowerCaseVersion.endsWith(qualifierName)
              || trimmedLowerCaseVersion.endsWith(alternateQualifierName)
              || versionQualifier.startsWith(qualifier.getAbbreviation())) {

            return qualifier;
          }
        }
      }

      return Qualifier.UNDEFINED;
    }

    private final String abbreviation;
    private final String description;

    /**
     * Constructs an instance of a {@link Qualifier} enumerated value initialized with the given abbreviation
     * and description.
     *
     * @param abbreviation {@link String} abbreviation for this {@link Qualifier}.
     * @param description describing this {@link Qualifier}.
     */
    Qualifier(@NotNull String abbreviation, @NotNull String description) {
      this.abbreviation = abbreviation;
      this.description = description;
    }

    /**
     * Determines whether this {@link Qualifier} is an ALPHA version.
     *
     * @return a boolean value if this {@link Qualifier} is an ALPHA version.
     */
    public boolean isAlpha() {
      return this == ALPHA;
    }

    /**
     * Determines whether this {@link Qualifier} is a BETA version.
     *
     * @return a boolean value if this {@link Qualifier} is a BETA version.
     */
    public boolean isBeta() {
      return this == BETA;
    }

    /**
     * Determines whether this {@link Qualifier} is a BUILD-SNAPSHOT version.
     *
     * @return a boolean value if this {@link Qualifier} is a BUILD-SNAPSHOT version.
     */
    public boolean isBuildSnapshot() {
      return this == BUILD_SNAPSHOT;
    }

    /**
     * Determines whether this {@link Qualifier} is an ITERATION version.
     *
     * @return a boolean value if this {@link Qualifier} is an ITERATION version.
     */
    public boolean isIteration() {
      return this == ITERATION;
    }

    /**
     * Determines whether this {@link Qualifier} is a MILESTONE version.
     *
     * @return a boolean value if this {@link Qualifier} is an MILESTONE version.
     */
    public boolean isMilestone() {
      return this == MILESTONE;
    }

    /**
     * Determines whether this {@link Qualifier} is a RELEASE-CANDIDATE version.
     *
     * @return a boolean value if this {@link Qualifier} is a RELEASE-CANDIDATE version.
     */
    public boolean isReleaseCandidate() {
      return this == RELEASE_CANDIDATE;
    }

    /**
     * Determines whether this {@link Qualifier} is a RELEASE version.
     *
     * @return a boolean value if this {@link Qualifier} is a RELEASE version.
     */
    public boolean isRelease() {
      return this == RELEASE;
    }

    /**
     * Determines whether this {@link Qualifier} is a SNAPSHOT version.
     *
     * @return a boolean value if this {@link Qualifier} is a SNAPSHOT version.
     */
    public boolean isSnapshot() {
      return this == SNAPSHOT;
    }

    /**
     * Determines whether this {@link Qualifier} is UNDEFINED.
     *
     * @return a boolean value if this {@link Qualifier} is UNDEFINED.
     */
    public boolean isUndefined() {
      return this == UNDEFINED;
    }

    /**
     * Determines whether this {@link Qualifier} is not UNDEFINED.
     *
     * @return a boolean value if this {@link Qualifier} is not UNDEFINED.
     */
    public boolean isNotUndefined() {
      return !isUndefined();
    }

    /**
     * Returns the abbreviation for this {@link Qualifier}.
     *
     * @return the abbreviation for this {@link Qualifier}.
     */
    public @NotNull String getAbbreviation() {
      return this.abbreviation;
    }

    /**
     * Returns a description of this {@link Qualifier}.
     *
     * @return a description of this {@link Qualifier}.
     */
    public @NotNull String getDescription() {
      return this.description;
    }

    /**
     * Return a {@link String} representation of this {@link Qualifier}.
     *
     * @return a {@link String} describing this {@link Qualifier}.
     * @see java.lang.Object#toString()
     */
    @Override
    public @NotNull String toString() {
      return getDescription();
    }
  }
}
