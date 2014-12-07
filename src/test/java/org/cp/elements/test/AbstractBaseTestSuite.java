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

  public String getClassesOutputDirectoryName() {
    return "classes";
  }

  public File getLocation(Class type) {
    String pathname = type.getName().replaceAll("\\.", File.separator).concat(".class");
    return new File(getClassesOutputDirectory(), pathname);
  }

}
