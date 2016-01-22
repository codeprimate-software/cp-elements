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

import java.io.File;
import java.util.logging.ConsoleHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * The AbstractBaseTestSuite class is an abstract base class containing functionality common to all test classes
 * and test suites in the cp-elements project.
 *
 * @author John J. Blum
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractBaseTestSuite {

  protected static final File TEMPORARY_DIRECTORY = new File(System.getProperty("java.io.tmpdir"));
  protected static final File USER_HOME = new File(System.getProperty("user.home"));
  protected static final File WORKING_DIRECTORY = new File(System.getProperty("user.dir"));

  private final Logger logger;

  protected AbstractBaseTestSuite() {
    logger = Logger.getLogger(getClass().getName());
    logger.setLevel(Level.WARNING);
    logger.setUseParentHandlers(false);

    for (Handler handler : logger.getHandlers()) {
      logger.removeHandler(handler);
    }

    logger.addHandler(new ConsoleHandler());
  }

  protected File getBuildOutputDirectory() {
    File buildOutputDirectory = new File(WORKING_DIRECTORY, getBuildOutputDirectoryName());
    return (buildOutputDirectory.isDirectory() ? buildOutputDirectory : WORKING_DIRECTORY);
  }

  protected String getBuildOutputDirectoryName() {
    return "target";
  }

  protected File getClassesOutputDirectory() {
    return new File(getBuildOutputDirectory(), getClassesOutputDirectoryName());
  }

  protected String getClassesOutputDirectoryName() {
    return "classes";
  }

  protected File getLocation(Class type) {
    String pathname = type.getName().replaceAll("\\.", "/").concat(".class");
    return new File(getClassesOutputDirectory(), pathname);
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
    if (logger.isLoggable(Level.FINE) || logger.isLoggable(Level.FINER) || logger.isLoggable(Level.FINEST)) {
      logger.fine(message);
    }
  }

  protected void logConfig(final String message) {
    if (logger.isLoggable(Level.CONFIG)) {
      logger.config(message);
    }
  }

  protected void logInfo(final String message) {
    if (logger.isLoggable(Level.INFO)) {
      logger.info(message);
    }
  }

  protected void logWarning(final String message) {
    if (logger.isLoggable(Level.WARNING)) {
      logger.warning(message);
    }
  }

  protected void logError(final String message) {
    if (logger.isLoggable(Level.SEVERE)) {
      logger.severe(message);
    }
  }

  protected void setLogLevel(final Level logLevel) {
    logger.setLevel(logLevel);

    for (Handler logHandler : logger.getHandlers()) {
      if (logHandler instanceof ConsoleHandler) {
        logHandler.setLevel(logLevel);
      }
    }
  }

}
