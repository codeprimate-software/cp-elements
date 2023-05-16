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

import java.time.LocalDateTime;
import java.time.Month;

import org.cp.elements.lang.Version.Qualifier;
import org.cp.elements.test.TestUtils;
import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link Version} and {@link Version.Qualifier}.
 *
 * @author John Blum
 * @see org.junit.Rule
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.lang.Version
 * @see org.cp.elements.lang.Version.Qualifier
 * @since 1.0.0
 */
public class VersionTests {

  @Test
  public void fromMajorMinorVersionNumbers() {

    Version version = Version.from(1, 2);

    assertThat(version).isNotNull();
    assertThat(version.getBuildNumber()).isEqualTo(0);
    assertThat(version.getMajor()).isEqualTo(1);
    assertThat(version.getMinor()).isEqualTo(2);
    assertThat(version.getMaintenance()).isEqualTo(0);
    assertThat(version.getQualifier()).isEqualTo(Qualifier.UNDEFINED);
    assertThat(version.getQualifierNumber()).isEqualTo(0);
    assertThat(version.getReleaseDateTime()).isNull();
  }

  @Test
  public void fromMajorMinorMaintenanceVersionNumbers() {

    Version version = Version.from(1, 2, 4);

    assertThat(version).isNotNull();
    assertThat(version.getBuildNumber()).isEqualTo(0);
    assertThat(version.getMajor()).isEqualTo(1);
    assertThat(version.getMinor()).isEqualTo(2);
    assertThat(version.getMaintenance()).isEqualTo(4);
    assertThat(version.getQualifier()).isEqualTo(Qualifier.UNDEFINED);
    assertThat(version.getQualifierNumber()).isEqualTo(0);
    assertThat(version.getReleaseDateTime()).isNull();
  }

  @Test
  public void fromMajorMinorMaintenanceVersionNumbersWithQualifier() {

    Version version = Version.from(1, 2, 4, Qualifier.RELEASE);

    assertThat(version).isNotNull();
    assertThat(version.getBuildNumber()).isEqualTo(0);
    assertThat(version.getMajor()).isEqualTo(1);
    assertThat(version.getMinor()).isEqualTo(2);
    assertThat(version.getMaintenance()).isEqualTo(4);
    assertThat(version.getQualifier()).isEqualTo(Qualifier.RELEASE);
    assertThat(version.getQualifierNumber()).isEqualTo(0);
    assertThat(version.getReleaseDateTime()).isNull();
  }

  @Test
  public void fromMajorMinorMaintenanceVersionNumbersWithQualifierAndQualifierNumber() {

    Version version = Version.from(1, 2, 4, Qualifier.SNAPSHOT, 8);

    assertThat(version).isNotNull();
    assertThat(version.getBuildNumber()).isEqualTo(0);
    assertThat(version.getMajor()).isEqualTo(1);
    assertThat(version.getMinor()).isEqualTo(2);
    assertThat(version.getMaintenance()).isEqualTo(4);
    assertThat(version.getQualifier()).isEqualTo(Qualifier.SNAPSHOT);
    assertThat(version.getQualifierNumber()).isEqualTo(8);
    assertThat(version.getReleaseDateTime()).isNull();
  }

  @Test
  public void parseVersion() {

    Version version = Version.parse("1.2.4.M3");

    assertThat(version).isNotNull();
    assertThat(version.getBuildNumber()).isEqualTo(0);
    assertThat(version.getMajor()).isEqualTo(1);
    assertThat(version.getMinor()).isEqualTo(2);
    assertThat(version.getMaintenance()).isEqualTo(4);
    assertThat(version.getQualifier()).isEqualTo(Qualifier.MILESTONE);
    assertThat(version.getQualifierNumber()).isEqualTo(3);
    assertThat(version.getReleaseDateTime()).isNull();
  }

  @Test
  public void parseMajorMinorVersion() {

    Version version = Version.parse("1.2");

    assertThat(version).isNotNull();
    assertThat(version.getBuildNumber()).isEqualTo(0);
    assertThat(version.getMajor()).isEqualTo(1);
    assertThat(version.getMinor()).isEqualTo(2);
    assertThat(version.getMaintenance()).isEqualTo(0);
    assertThat(version.getQualifier()).isEqualTo(Qualifier.UNDEFINED);
    assertThat(version.getQualifierNumber()).isEqualTo(0);
    assertThat(version.getReleaseDateTime()).isNull();
  }

  @Test
  public void parseMajorMinorMaintenanceVersion() {

    Version version = Version.parse("1.2.4");

    assertThat(version).isNotNull();
    assertThat(version.getBuildNumber()).isEqualTo(0);
    assertThat(version.getMajor()).isEqualTo(1);
    assertThat(version.getMinor()).isEqualTo(2);
    assertThat(version.getMaintenance()).isEqualTo(4);
    assertThat(version.getQualifier()).isEqualTo(Qualifier.UNDEFINED);
    assertThat(version.getQualifierNumber()).isEqualTo(0);
    assertThat(version.getReleaseDateTime()).isNull();
  }

  @Test
  public void parseMajorMinorMaintenanceWithQualifierVersion() {

    Version version = Version.parse("1.2.4.BUILD-SNAPSHOT");

    assertThat(version).isNotNull();
    assertThat(version.getBuildNumber()).isEqualTo(0);
    assertThat(version.getMajor()).isEqualTo(1);
    assertThat(version.getMinor()).isEqualTo(2);
    assertThat(version.getMaintenance()).isEqualTo(4);
    assertThat(version.getQualifier()).isEqualTo(Qualifier.BUILD_SNAPSHOT);
    assertThat(version.getQualifierNumber()).isEqualTo(0);
    assertThat(version.getReleaseDateTime()).isNull();
  }

  @Test(expected = IllegalArgumentException.class)
  public void parseUnrecognizedVersion() {
    TestUtils.doIllegalArgumentExceptionThrowingOperation(() -> Version.parse("1.2.3.4.5"),
      () -> "Unrecognized format for version [1.2.3.4.5]");
  }

  @Test(expected = IllegalArgumentException.class)
  public void parseNullVersion() {
    TestUtils.doIllegalArgumentExceptionThrowingOperation(() -> Version.parse(null),
      () -> "A version [null] is required");
  }

  @Test(expected = IllegalArgumentException.class)
  public void parseEmptyVersion() {
    TestUtils.doIllegalArgumentExceptionThrowingOperation(() -> Version.parse(""),
      () -> "A version [] is required");
  }

  @Test(expected = IllegalArgumentException.class)
  public void parseBlankVersion() {
    TestUtils.doIllegalArgumentExceptionThrowingOperation(() -> Version.parse("  "),
      () -> "A version [  ] is required");
  }

  @Test
  public void constructVersion() {

    Version version = new Version(1, 2, 4, null, -1);

    assertThat(version).isNotNull();
    assertThat(version.getBuildNumber()).isEqualTo(0);
    assertThat(version.getMajor()).isEqualTo(1);
    assertThat(version.getMinor()).isEqualTo(2);
    assertThat(version.getMaintenance()).isEqualTo(4);
    assertThat(version.getQualifier()).isEqualTo(Qualifier.UNDEFINED);
    assertThat(version.getQualifierNumber()).isEqualTo(0);
    assertThat(version.getReleaseDateTime()).isNull();
  }

  @Test(expected = IllegalArgumentException.class)
  public void constructVersionWithIllegalMajorVersionNumber() {
    TestUtils.doIllegalArgumentExceptionThrowingOperation(() -> new Version(-1, 0),
      () -> "Major version [-1] must be greater than equal to 0");
  }

  @Test(expected = IllegalArgumentException.class)
  public void constructVersionWithIllegalMinorVersionNumber() {
    TestUtils.doIllegalArgumentExceptionThrowingOperation(() -> new Version(0, -2),
      () -> "Minor version [-2] must be greater than equal to 0");
  }

  @Test(expected = IllegalArgumentException.class)
  public void constructVersionWithIllegalMaintenanceVersionNumber() {
    TestUtils.doIllegalArgumentExceptionThrowingOperation(() -> new Version(0, 0, -4),
      () -> "Maintenance version [-4] must be greater than equal to 0");
  }

  @Test
  public void constructVersionWithBuilderNumberAndReleaseDate() {

    LocalDateTime releaseDateTime = LocalDateTime.of(2016, Month.DECEMBER, 22, 14, 0, 30);

    Version version = Version.from(1, 2, 4, Qualifier.RELEASE).with(1024).on(releaseDateTime);

    assertThat(version).isNotNull();
    assertThat(version.getBuildNumber()).isEqualTo(1024);
    assertThat(version.getMajor()).isEqualTo(1);
    assertThat(version.getMinor()).isEqualTo(2);
    assertThat(version.getMaintenance()).isEqualTo(4);
    assertThat(version.getQualifier()).isEqualTo(Qualifier.RELEASE);
    assertThat(version.getQualifierNumber()).isEqualTo(0);
    assertThat(version.getReleaseDateTime()).isEqualTo(releaseDateTime);
  }

  @Test
  @SuppressWarnings("all")
  public void compareToIsCorrect() {

    Version milestoneOne = Version.from(1, 2, 4, Qualifier.MILESTONE, 1);
    Version milestoneTwo = Version.from(1, 2, 4, Qualifier.MILESTONE, 2);
    Version releaseCandidateOne = Version.from(1, 2, 4, Qualifier.RELEASE_CANDIDATE);
    Version releaseCandidateTwo = Version.from(4, 2, 1, Qualifier.RELEASE_CANDIDATE);
    Version releaseOne = Version.from(1, 2, 4, Qualifier.RELEASE);
    Version releaseTwo = Version.from(2, 1, 4, Qualifier.RELEASE);

    assertThat(releaseOne.compareTo(releaseOne)).isEqualTo(0);
    assertThat(releaseOne.compareTo(releaseTwo)).isLessThan(0);
    assertThat(releaseTwo.compareTo(releaseOne)).isGreaterThan(0);
    assertThat(milestoneOne.compareTo(milestoneOne)).isEqualTo(0);
    assertThat(milestoneOne.compareTo(milestoneTwo)).isLessThan(0);
    assertThat(milestoneTwo.compareTo(milestoneOne)).isGreaterThan(0);
    assertThat(releaseCandidateOne.compareTo(releaseOne)).isLessThan(0);
    assertThat(releaseCandidateTwo.compareTo(releaseOne)).isGreaterThan(0);
    assertThat(releaseTwo.compareTo(releaseCandidateTwo)).isLessThan(0);
  }

  @Test
  @SuppressWarnings("all")
  public void equalsIsCorrect() {

    Version majorMinorVersion = Version.from(1, 2);
    Version majorMinorMaintenanceVersion = Version.from(1, 2, 4);
    Version majorMinorMaintenanceQualifierVersion = Version.from(1, 2, 4, Qualifier.MILESTONE);
    Version majorMinorMaintenanceQualifierNumberVersion = Version.from(1, 2, 4, Qualifier.MILESTONE, 2);
    Version majorMinorMaintenanceQualifierBuildNumberVersion = Version.from(1, 2, 4, Qualifier.MILESTONE, 2).with(1234);

    assertThat(majorMinorVersion.equals(majorMinorVersion)).isTrue();
    assertThat(majorMinorVersion.equals(majorMinorMaintenanceVersion)).isFalse();
    assertThat(majorMinorMaintenanceVersion.equals(majorMinorMaintenanceQualifierVersion)).isFalse();
    assertThat(majorMinorMaintenanceQualifierVersion.equals(majorMinorMaintenanceQualifierNumberVersion)).isFalse();
    assertThat(majorMinorMaintenanceQualifierNumberVersion.equals(majorMinorMaintenanceQualifierBuildNumberVersion)).isFalse();
    assertThat(majorMinorMaintenanceQualifierBuildNumberVersion.equals(majorMinorVersion)).isFalse();
  }

  @Test
  public void notEqualsIsCorrect() {

    Version versionOne = Version.from(1, 2, 4, Qualifier.RELEASE).with(1234);
    Version versionTwo = Version.from(1, 2, 4, Qualifier.RELEASE).with(6789);

    assertThat(versionOne.equals(versionTwo)).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void notEqualToNullIsCorrect() {
    assertThat(Version.from(1, 2, 4, Qualifier.RELEASE).equals(null)).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void notEqualToStringVersionIsCorrect() {
    assertThat(Version.from(1, 2, 4, Qualifier.RELEASE).equals("1.2.4.RELEASE")).isFalse();
  }

  @Test
  public void hashCodeIsCorrect() {

    Version majorMinorVersion = Version.from(1, 2);
    Version majorMinorMaintenanceVersion = Version.from(1, 2, 4);
    Version majorMinorMaintenanceQualifierVersion = Version.from(1, 2, 4, Qualifier.MILESTONE);
    Version majorMinorMaintenanceQualifierNumberVersion = Version.from(1, 2, 4, Qualifier.MILESTONE, 2);
    Version majorMinorMaintenanceQualifierBuildNumberVersion = Version.from(1, 2, 4, Qualifier.MILESTONE, 2).with(1234);

    assertThat(majorMinorVersion.hashCode()).isEqualTo(majorMinorVersion.hashCode());
    assertThat(majorMinorVersion.hashCode()).isNotEqualTo(majorMinorMaintenanceVersion.hashCode());
    assertThat(majorMinorMaintenanceVersion.hashCode()).isNotEqualTo(majorMinorMaintenanceQualifierVersion.hashCode());
    assertThat(majorMinorMaintenanceQualifierVersion.hashCode()).isNotEqualTo(majorMinorMaintenanceQualifierNumberVersion.hashCode());
    assertThat(majorMinorMaintenanceQualifierNumberVersion.hashCode()).isNotEqualTo(majorMinorMaintenanceQualifierBuildNumberVersion.hashCode());
    assertThat(majorMinorMaintenanceQualifierBuildNumberVersion.hashCode()).isNotEqualTo(majorMinorVersion.hashCode());
  }

  @Test
  public void toStringIsCorrect() {

    Version version = Version.from(1, 2, 4);

    assertThat(version.toString()).isEqualTo("1.2.4");
  }

  @Test
  public void toStringWithBuildNumberIsCorrect() {

    Version version = Version.from(1, 0, 1).with(1234);

    assertThat(version.toString()).isEqualTo("1.0.1 build 1234");
  }

  @Test
  public void toStringWithQualifierIsCorrect() {

    Version version = Version.from(1, 0, 1, Qualifier.SNAPSHOT);

    assertThat(version.toString()).isEqualTo("1.0.1.SNAPSHOT");
  }

  @Test
  public void toStringWithReleaseDateTimeIsCorrect() {

    LocalDateTime releaseDateTime = LocalDateTime.of(2016, Month.DECEMBER, 22, 14, 30, 15);

    Version version = Version.from(1, 0, 1).on(releaseDateTime);

    assertThat(version.toString()).isEqualTo("1.0.1 on 2016-December-22-14-30-15");
  }

  @Test
  public void toStringWithBuildNumberAndQualifierIsCorrect() {

    Version version = Version.from(1, 0, 1, Qualifier.MILESTONE, 4).with(1234);

    assertThat(version.toString()).isEqualTo("1.0.1.M4 build 1234");
  }

  @Test
  public void toStringWithQualifierAndReleaseDateTimeIsCorrect() {

    LocalDateTime releaseDateTime = LocalDateTime.of(2016, Month.DECEMBER, 22, 14, 35, 0);

    Version version = Version.from(1, 0, 1, Qualifier.RELEASE_CANDIDATE, 2).on(releaseDateTime);

    assertThat(version.toString()).isEqualTo("1.0.1.RC2 on 2016-December-22-14-35-00");
  }

  @Test
  public void toStringWithBuildNumberQualifierAndReleaseDateTimeIsCorrect() {

    LocalDateTime releaseDateTime = LocalDateTime.of(2016, Month.DECEMBER, 22, 14, 30, 45);

    Version version = Version.from(1, 0, 1, Qualifier.MILESTONE, 2).on(releaseDateTime).with(5678);

    assertThat(version.toString()).isEqualTo("1.0.1.M2 build 5678 on 2016-December-22-14-30-45");
  }

  @Test
  public void qualifierIdentityIsCorrect() {

    assertThat(Qualifier.ALPHA.isAlpha()).isTrue();
    assertThat(Qualifier.BETA.isBeta()).isTrue();
    assertThat(Qualifier.BUILD_SNAPSHOT.isBuildSnapshot()).isTrue();
    assertThat(Qualifier.ITERATION.isIteration()).isTrue();
    assertThat(Qualifier.MILESTONE.isMilestone()).isTrue();
    assertThat(Qualifier.RELEASE_CANDIDATE.isReleaseCandidate()).isTrue();
    assertThat(Qualifier.RELEASE.isRelease()).isTrue();
    assertThat(Qualifier.SNAPSHOT.isSnapshot()).isTrue();
    assertThat(Qualifier.UNDEFINED.isUndefined()).isTrue();
    assertThat(Qualifier.ALPHA.isBeta()).isFalse();
    assertThat(Qualifier.BETA.isAlpha()).isFalse();
    assertThat(Qualifier.BUILD_SNAPSHOT.isSnapshot()).isFalse();
    assertThat(Qualifier.ITERATION.isMilestone()).isFalse();
    assertThat(Qualifier.MILESTONE.isIteration()).isFalse();
    assertThat(Qualifier.RELEASE_CANDIDATE.isRelease()).isFalse();
    assertThat(Qualifier.RELEASE.isReleaseCandidate()).isFalse();
    assertThat(Qualifier.SNAPSHOT.isBuildSnapshot()).isFalse();
    assertThat(Qualifier.UNDEFINED.isRelease()).isFalse();
  }

  @Test
  public void qualifierResolveIsCorrect() {

    assertThat(Qualifier.resolve("1.0.0.ALPHA")).isEqualTo(Qualifier.ALPHA);
    assertThat(Qualifier.resolve("1.0.0.BETA")).isEqualTo(Qualifier.BETA);
    assertThat(Qualifier.resolve("1.0.0.BUILD-SNAPSHOT")).isEqualTo(Qualifier.BUILD_SNAPSHOT);
    assertThat(Qualifier.resolve("1.0.0.BUILD_SNAPSHOT")).isEqualTo(Qualifier.BUILD_SNAPSHOT);
    assertThat(Qualifier.resolve("1.0.0.ITERATION")).isEqualTo(Qualifier.ITERATION);
    assertThat(Qualifier.resolve("1.0.0.ITER")).isEqualTo(Qualifier.ITERATION);
    assertThat(Qualifier.resolve("1.0.0.IT")).isEqualTo(Qualifier.ITERATION);
    assertThat(Qualifier.resolve("1.0.0.MILESTONE")).isEqualTo(Qualifier.MILESTONE);
    assertThat(Qualifier.resolve("1.0.0.MILE")).isEqualTo(Qualifier.MILESTONE);
    assertThat(Qualifier.resolve("1.0.0.MI")).isEqualTo(Qualifier.MILESTONE);
    assertThat(Qualifier.resolve("1.0.0.M0")).isEqualTo(Qualifier.MILESTONE);
    assertThat(Qualifier.resolve("1.0.0.M1")).isEqualTo(Qualifier.MILESTONE);
    assertThat(Qualifier.resolve("1.0.0.M2")).isEqualTo(Qualifier.MILESTONE);
    assertThat(Qualifier.resolve("1.0.0.M3")).isEqualTo(Qualifier.MILESTONE);
    assertThat(Qualifier.resolve("1.0.0.M")).isEqualTo(Qualifier.MILESTONE);
    assertThat(Qualifier.resolve("1.0.0.MileStone")).isEqualTo(Qualifier.MILESTONE);
    assertThat(Qualifier.resolve("1.0.0.MS")).isEqualTo(Qualifier.MILESTONE);
    assertThat(Qualifier.resolve("1.0.0.M#")).isEqualTo(Qualifier.MILESTONE);
    assertThat(Qualifier.resolve("1.0.0.M!")).isEqualTo(Qualifier.MILESTONE);
    assertThat(Qualifier.resolve("1.0.0.RELEASE_CANDIDATE")).isEqualTo(Qualifier.RELEASE_CANDIDATE);
    assertThat(Qualifier.resolve("1.0.0.RC")).isEqualTo(Qualifier.RELEASE_CANDIDATE);
    assertThat(Qualifier.resolve("1.0.0.RC0")).isEqualTo(Qualifier.RELEASE_CANDIDATE);
    assertThat(Qualifier.resolve("1.0.0.RC1")).isEqualTo(Qualifier.RELEASE_CANDIDATE);
    assertThat(Qualifier.resolve("1.0.0.RC2")).isEqualTo(Qualifier.RELEASE_CANDIDATE);
    assertThat(Qualifier.resolve("1.0.0.RC3")).isEqualTo(Qualifier.RELEASE_CANDIDATE);
    assertThat(Qualifier.resolve("1.0.0.RELEASE")).isEqualTo(Qualifier.RELEASE);
    assertThat(Qualifier.resolve("1.0.0.Release")).isEqualTo(Qualifier.RELEASE);
    assertThat(Qualifier.resolve("1.0.0.SNAPSHOT")).isEqualTo(Qualifier.SNAPSHOT);
    assertThat(Qualifier.resolve("1.0.0.snapshot")).isEqualTo(Qualifier.SNAPSHOT);
    assertThat(Qualifier.resolve("RELEASE")).isEqualTo(Qualifier.RELEASE);
    assertThat(Qualifier.resolve("Snapshot")).isEqualTo(Qualifier.SNAPSHOT);
    assertThat(Qualifier.resolve("undefined")).isEqualTo(Qualifier.UNDEFINED);
  }

  @Test
  public void qualifierResolvesToUndefined() {

    assertThat(Qualifier.resolve("1.0.0.ALPH")).isEqualTo(Qualifier.UNDEFINED);
    assertThat(Qualifier.resolve("1.0.0.BET")).isEqualTo(Qualifier.UNDEFINED);
    assertThat(Qualifier.resolve("1.0.0.BLD-SNPSHT")).isEqualTo(Qualifier.UNDEFINED);
    assertThat(Qualifier.resolve("1.0.0.BUILD-SNAP")).isEqualTo(Qualifier.UNDEFINED);
    assertThat(Qualifier.resolve("1.0.0.BUILD")).isEqualTo(Qualifier.UNDEFINED);
    assertThat(Qualifier.resolve("1.0.0.I")).isEqualTo(Qualifier.UNDEFINED);
    assertThat(Qualifier.resolve("1.0.0.R")).isEqualTo(Qualifier.UNDEFINED);
    assertThat(Qualifier.resolve("1.0.0.RD")).isEqualTo(Qualifier.UNDEFINED);
    assertThat(Qualifier.resolve("1.0.0.REL")).isEqualTo(Qualifier.UNDEFINED);
    assertThat(Qualifier.resolve("1.0.0.GA")).isEqualTo(Qualifier.UNDEFINED);
    assertThat(Qualifier.resolve("1.0.0.SNAP")).isEqualTo(Qualifier.UNDEFINED);
    assertThat(Qualifier.resolve("1.0.0.SR")).isEqualTo(Qualifier.UNDEFINED);
    assertThat(Qualifier.resolve("1.0.0.S")).isEqualTo(Qualifier.UNDEFINED);
    assertThat(Qualifier.resolve("1.0.0.UNDEFINED")).isEqualTo(Qualifier.UNDEFINED);
    assertThat(Qualifier.resolve("1.0.0.DEFINED")).isEqualTo(Qualifier.UNDEFINED);
    assertThat(Qualifier.resolve("1.0.0.UNDEF")).isEqualTo(Qualifier.UNDEFINED);
    assertThat(Qualifier.resolve("1.0.0.DEF")).isEqualTo(Qualifier.UNDEFINED);
    assertThat(Qualifier.resolve("1.0.0.U")).isEqualTo(Qualifier.UNDEFINED);
    assertThat(Qualifier.resolve("1.0.0")).isEqualTo(Qualifier.UNDEFINED);
    assertThat(Qualifier.resolve("  ")).isEqualTo(Qualifier.UNDEFINED);
    assertThat(Qualifier.resolve("")).isEqualTo(Qualifier.UNDEFINED);
    assertThat(Qualifier.resolve(null)).isEqualTo(Qualifier.UNDEFINED);
  }
}
