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

package org.cp.elements.test;

import static org.cp.elements.util.stream.StreamUtils.stream;

import java.io.File;
import java.util.logging.ConsoleHandler;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * The {@link AbstractBaseTestSuite} class is an abstract base class containing functionality common to
 * all test classes and test suites in the cp-elements project.
 *
 * @author John J. Blum
 * @see java.util.logging.Logger
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractBaseTestSuite {

  protected static final File TEMPORARY_DIRECTORY = new File(System.getProperty("java.io.tmpdir"));
  protected static final File USER_HOME = new File(System.getProperty("user.home"));
  protected static final File WORKING_DIRECTORY = new File(System.getProperty("user.dir"));

  private final Logger logger;

  protected AbstractBaseTestSuite() {

    this.logger = Logger.getLogger(getClass().getName());
    this.logger.setLevel(Level.WARNING);
    this.logger.setUseParentHandlers(false);

    stream(this.logger.getHandlers()).forEach(this.logger::removeHandler);

    this.logger.addHandler(new ConsoleHandler());
  }

  protected File getBuildDirectory() {
    return new File(getProjectHomeDirectory(), getBuildDirectoryName());
  }

  protected String getBuildDirectoryName() {
    return Boolean.getBoolean("gradle.build") ? "build" : "target";
  }

  protected File getClassesDirectory() {
    return new File(getBuildDirectory(), getClassesDirectoryName());
  }

  protected String getClassesDirectoryName() {

    return Boolean.getBoolean("gradle.build") ?
      String.format("%1$s%2$s%3$s", "classes", File.separator, "main") : "classes";
  }

  protected File getLocation(Class type) {

    String pathname = type.getName().replaceAll("\\.", "/").concat(".class");

    return new File(getClassesDirectory(), pathname);
  }

  protected File getProjectHomeDirectory() {

    File projectHomeDirectory = WORKING_DIRECTORY;

    while (projectHomeDirectory != null && !getProjectHomeDirectoryName().equals(projectHomeDirectory.getName())) {
      projectHomeDirectory = projectHomeDirectory.getParentFile();
    }

    return projectHomeDirectory;
  }

  protected String getProjectHomeDirectoryName() {
    return "cp-elements";
  }

  protected File getSourceDirectory() {
    return new File(getProjectHomeDirectory(), getSourceDirectoryName());
  }

  protected String getSourceDirectoryName() {
    return "src";
  }

  protected void logDebug(final String message) {

    if (this.logger.isLoggable(Level.FINE) || this.logger.isLoggable(Level.FINER)
        || this.logger.isLoggable(Level.FINEST)) {

      this.logger.fine(message);
    }
  }

  protected void logConfig(final String message) {

    if (this.logger.isLoggable(Level.CONFIG)) {
      this.logger.config(message);
    }
  }

  protected void logInfo(final String message) {

    if (this.logger.isLoggable(Level.INFO)) {
      this.logger.info(message);
    }
  }

  protected void logWarning(final String message) {

    if (this.logger.isLoggable(Level.WARNING)) {
      this.logger.warning(message);
    }
  }

  protected void logError(final String message) {

    if (this.logger.isLoggable(Level.SEVERE)) {
      this.logger.severe(message);
    }
  }

  protected void setLogLevel(final Level logLevel) {

    this.logger.setLevel(logLevel);

    stream(this.logger.getHandlers()).forEach(logHandler -> {
      if (logHandler instanceof ConsoleHandler) {
        logHandler.setLevel(logLevel);
      }
    });
  }
}
