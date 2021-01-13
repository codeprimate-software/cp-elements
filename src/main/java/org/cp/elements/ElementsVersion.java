/*
 * Copyright 2016 Author or Authors.
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
package org.cp.elements;

import java.time.LocalDate;
import java.util.Optional;
import java.util.Properties;

import org.cp.elements.io.IOUtils;
import org.cp.elements.lang.StringUtils;

/**
 * The {@link ElementsVersion} class declares the version of the Codeprimate Elements project.
 *
 * @author John Blum
 * @see java.lang.Runnable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ElementsVersion implements Runnable {

  public static final int DEFAULT_YEAR = 2011;

  protected static final String MAVEN_PROPERTIES_FILE_LOCATION =
    "META-INF/maven/org.codeprimate/cp-elements/pom.properties";

  protected static final String MAVEN_ARTIFACT_ID_PROPERTY = "artifactId";
  protected static final String MAVEN_GROUP_ID_PROPERTY = "groupId";
  protected static final String MAVEN_VERSION_PROPERTY = "version";

  public static final String PROJECT_ARTIFACT_ID = "cp-elements";
  public static final String PROJECT_LICENSE = "Apache License, version 2.0";
  public static final String PROJECT_NAME = "Codeprimate Elements";
  public static final String PROJECT_VERSION = "version 1.0.0.M4";

  private Properties maven;

  public static void main(String[] args) {
    new ElementsVersion().run();
  }

  @Override
  public void run() {
    log("%1$s (%2$s) %3$s - Copyright \u00A9 %4$d%n%5$s%n", PROJECT_NAME, resolveProjectArtifactId(),
      resolveProjectVersion(), resolveYear(), PROJECT_LICENSE);
  }

  private Properties loadMavenProperties() {

    Properties maven = new Properties();

    Optional.ofNullable(ElementsVersion.class.getResourceAsStream(MAVEN_PROPERTIES_FILE_LOCATION))
      .ifPresent(inputStream -> IOUtils.doSafeIo(() -> maven.load(inputStream)));

    return maven;
  }

  private void log(String message, Object... args) {
    System.err.printf(message, args);
    System.err.flush();
  }

  private Properties resolveMavenProperties() {
    return Optional.ofNullable(maven).orElseGet(() -> maven = loadMavenProperties());
  }

  private String resolveProjectArtifactId() {
    return resolveProjectArtifactId(PROJECT_ARTIFACT_ID);
  }

  private String resolveProjectArtifactId(String defaultArtifactId) {

    return Optional.ofNullable(resolveMavenProperties().getProperty(MAVEN_ARTIFACT_ID_PROPERTY))
      .filter(StringUtils::hasText)
      .orElse(defaultArtifactId);
  }

  private String resolveProjectVersion() {
    return resolveProjectVersion(PROJECT_VERSION);
  }

  private String resolveProjectVersion(String defaultVersion) {

    return Optional.ofNullable(resolveMavenProperties().getProperty(MAVEN_VERSION_PROPERTY))
      .filter(StringUtils::hasText)
      .orElse(defaultVersion);
  }

  private int resolveYear() {
    return resolveYear(DEFAULT_YEAR);
  }

  private int resolveYear(int defaultYear) {
    return LocalDate.now().getYear();
  }
}
