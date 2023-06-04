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
package org.cp.elements.test;

import java.io.File;
import java.io.FileFilter;
import java.util.logging.ConsoleHandler;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ClassUtils;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.util.ArrayUtils;
import org.cp.elements.util.stream.StreamUtils;

/**
 * Abstract base class containing functionality common to all test classes and test suites
 * in the {@literal cp-elements} project.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.util.logging.Logger
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractTestSuite {

  protected static final File TEMPORARY_DIRECTORY = FileSystemUtils.TEMPORARY_DIRECTORY;
  protected static final File USER_HOME = FileSystemUtils.USER_HOME_DIRECTORY;
  protected static final File WORKING_DIRECTORY = FileSystemUtils.WORKING_DIRECTORY;

  protected static final String BUILD_DIRECTORY_NAME = "build";
  protected static final String CLASSES_DIRECTORY_NAME = "classes";
  protected static final String SOURCE_DIRECTORY_NAME = "src";
  protected static final String TARGET_DIRECTORY_NAME = "target";

  private final Logger logger;

  protected AbstractTestSuite() {

    this.logger = Logger.getLogger(getClass().getName());
    this.logger.setLevel(Level.WARNING);
    this.logger.setUseParentHandlers(false);

    StreamUtils.stream(this.logger.getHandlers()).forEach(this.logger::removeHandler);

    this.logger.addHandler(new ConsoleHandler());
  }

  protected File getBuildDirectory() {
    return new File(getProjectHomeDirectory(), getBuildDirectoryName());
  }

  protected String getBuildDirectoryName() {
    return Boolean.getBoolean("gradle.build") ? BUILD_DIRECTORY_NAME : TARGET_DIRECTORY_NAME;
  }

  protected File getClassesDirectory() {
    return new File(getBuildDirectory(), getClassesDirectoryName());
  }

  protected String getClassesDirectoryName() {

    return Boolean.getBoolean("gradle.build")
      ? CLASSES_DIRECTORY_NAME.concat(File.separator).concat("java").concat(File.separator).concat("main")
      : CLASSES_DIRECTORY_NAME;
  }

  protected File getLocation(Class<?> type) {
    return new File(getClassesDirectory(), ClassUtils.getResourceName(type));
  }

  protected File getProjectHomeDirectory() {

    if (!WORKING_DIRECTORY.getAbsolutePath().contains(getProjectHomeDirectoryName())) {
      return ObjectUtils.requireState(searchForProjectHomeDirectory(WORKING_DIRECTORY),
        "Unable to find project directory [%s] in working directory [%s]",
        getProjectHomeDirectoryName(), WORKING_DIRECTORY);
    }
    else {
      int index = WORKING_DIRECTORY.getAbsolutePath().indexOf(getProjectHomeDirectoryName());
      Assert.isTrue(index > -1,
        "Expected project home directory [%s] to be in the path of the working directory [%s]",
        getProjectHomeDirectoryName(), WORKING_DIRECTORY);
      return new File(WORKING_DIRECTORY.getAbsolutePath().substring(0, index), getProjectHomeDirectoryName());
    }
  }

  protected String getProjectHomeDirectoryName() {
    return "cp-elements";
  }

  private @Nullable File searchForProjectHomeDirectory(@NotNull File baseDirectory) {

    Assert.notNull(baseDirectory, "Directory to search is required");
    Assert.isTrue(baseDirectory.isDirectory(), "Pathname [%s] must be a directory", baseDirectory);

    FileFilter matchingDirectoryFilter = file ->
      file != null && file.getName().equals(getProjectHomeDirectoryName());

    File[] subdirectories = ArrayUtils.nullSafeArray(baseDirectory.listFiles(file ->
      file != null && file.isDirectory()), File.class);

    for (File subdirectory : subdirectories) {
      if (matchingDirectoryFilter.accept(subdirectory)) {
        return subdirectory;
      }
      else {
        File resolvedProjectHomeDirectory = searchForProjectHomeDirectory(subdirectory);
        if (matchingDirectoryFilter.accept(resolvedProjectHomeDirectory)) {
          return resolvedProjectHomeDirectory;
        }
      }
    }

    return null;
  }

  protected File getSourceDirectory() {
    return new File(getProjectHomeDirectory(), getSourceDirectoryName());
  }

  protected String getSourceDirectoryName() {
    return SOURCE_DIRECTORY_NAME;
  }

  protected void logDebug(String message) {

    if (this.logger.isLoggable(Level.FINE) || this.logger.isLoggable(Level.FINER)
        || this.logger.isLoggable(Level.FINEST)) {

      this.logger.fine(message);
    }
  }

  protected void logConfig(String message) {

    if (this.logger.isLoggable(Level.CONFIG)) {
      this.logger.config(message);
    }
  }

  protected void logInfo(String message) {

    if (this.logger.isLoggable(Level.INFO)) {
      this.logger.info(message);
    }
  }

  protected void logWarning(String message) {

    if (this.logger.isLoggable(Level.WARNING)) {
      this.logger.warning(message);
    }
  }

  protected void logError(String message) {

    if (this.logger.isLoggable(Level.SEVERE)) {
      this.logger.severe(message);
    }
  }

  protected void setLogLevel(Level logLevel) {

    this.logger.setLevel(logLevel);

    StreamUtils.stream(this.logger.getHandlers()).forEach(logHandler -> {
      if (logHandler instanceof ConsoleHandler) {
        logHandler.setLevel(logLevel);
      }
    });
  }
}
