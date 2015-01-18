/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
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
    String pathname = type.getName().replaceAll("\\.", File.separator).concat(".class");
    return new File(getClassesOutputDirectory(), pathname);
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
