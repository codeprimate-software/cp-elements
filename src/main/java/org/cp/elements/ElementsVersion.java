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
package org.cp.elements;

import java.io.IOException;
import java.io.InputStream;
import java.time.LocalDate;
import java.util.Optional;
import java.util.Properties;
import java.util.function.Supplier;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.cp.elements.io.IOUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NotNull;

/**
 * Declares the version of the {@literal Codeprimate Elements} software project and Java class library.
 *
 * @author John Blum
 * @see java.time.LocalDate
 * @see java.lang.Runnable
 * @see java.util.Properties
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
  public static final String PROJECT_VERSION = "version 1.0.0-M7";

  private final Logger logger = Logger.getLogger(getClass().getName());

  private Properties maven;

  public static void main(String[] args) {
    new ElementsVersion().run();
  }

  @Override
  public void run() {
    log("%1$s (%2$s) %3$s - Copyright \u00A9 %4$d%n%5$s%n", PROJECT_NAME, resolveProjectArtifactId(),
      resolveProjectVersion(), resolveYear(), PROJECT_LICENSE);
  }

  private @NotNull Properties loadMavenProperties() {

    Properties mavenProperties = new Properties();

    try (InputStream inputStream = ElementsVersion.class.getResourceAsStream(MAVEN_PROPERTIES_FILE_LOCATION)) {
      if (inputStream != null) {
        IOUtils.doSafeIo(() -> mavenProperties.load(inputStream));
      }
    }
    catch (IOException ignore) { }

    return mavenProperties;
  }

  private void log(@NotNull String message, @NotNull Object... args) {
    System.err.printf(message, args);
    System.err.flush();
  }

  private @NotNull Properties resolveMavenProperties() {

    if (this.maven == null) {
      this.maven = loadMavenProperties();
    }

    return this.maven;
  }

  private @NotNull String resolveProjectArtifactId() {
    return resolveProjectArtifactId(PROJECT_ARTIFACT_ID);
  }

  private @NotNull String resolveProjectArtifactId(@NotNull String defaultArtifactId) {

    String artifactId = resolveMavenProperties().getProperty(MAVEN_ARTIFACT_ID_PROPERTY);

    return Optional.ofNullable(artifactId)
      .filter(StringUtils::hasText)
      .orElse(defaultArtifactId);
  }

  private @NotNull String resolveProjectVersion() {
    return resolveProjectVersion(PROJECT_VERSION);
  }

  private @NotNull String resolveProjectVersion(@NotNull String defaultVersion) {

    String version = resolveMavenProperties().getProperty(MAVEN_VERSION_PROPERTY);

    return Optional.ofNullable(version)
      .filter(StringUtils::hasText)
      .orElse(defaultVersion);
  }

  private int resolveYear() {
    return resolveYear(DEFAULT_YEAR);
  }

  private int resolveYear(int defaultYear) {
    log(() -> "Inception Year [%d]".formatted(defaultYear));
    return LocalDate.now().getYear();
  }

  private void log(Supplier<String> message) {
    if (this.logger.isLoggable(Level.FINE)) {
      this.logger.fine(message.get());
    }
  }
}
