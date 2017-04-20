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

package org.cp.elements.process.java;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.process.ProcessAdapter;
import org.cp.elements.process.support.RuntimeProcessExecutor;
import org.cp.elements.util.ArrayUtils;

/**
 * The {@link JavaProcessExecutor} class is an implementation of {@link org.cp.elements.process.ProcessExecutor}
 * and extension of {@link RuntimeProcessExecutor} used to execute and launch Java application processes
 * (i.e. Java classes with a {@literal main} method).
 *
 * @author John J. Blum
 * @see java.io.File
 * @see org.cp.elements.process.ProcessAdapter
 * @see org.cp.elements.process.support.RuntimeProcessExecutor
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class JavaProcessExecutor extends RuntimeProcessExecutor {

  /**
   * Factory method used to construct an instance of the {@link JavaProcessExecutor} class.
   *
   * @return a new instance of the {@link JavaProcessExecutor} class.
   * @see org.cp.elements.process.java.JavaProcessExecutor
   */
  public static JavaProcessExecutor newJavaProcessExecutor() {
    return new JavaProcessExecutor();
  }

  /**
   * Forks and executes the given Java {@link Class} in the {@link File current working directory}
   * of the currently executing Java {@link Process}, passing the provided array of {@link String arguments}
   * on the command-line to the Java program.
   *
   * @param type Java {@link Class} to execute.
   * @param args array of {@link String arguments} passed to the Java program.
   * @return a {@link ProcessAdapter} representing the forked {@link Process} executing the given Java {@link Class}.
   * @see org.cp.elements.process.ProcessAdapter
   * @see #execute(File, Class, String...)
   */
  public ProcessAdapter execute(Class<?> type, String... args) {
    return execute(FileSystemUtils.WORKING_DIRECTORY, type, args);
  }

  /**
   * Forks and executes the given Java {@link Class} in the specified {@link File working directory},
   * passing the provided array of {@link String arguments} on the command-line to the Java program.
   *
   * @param directory {@link File directory} in which to execute the Java program.
   * @param type Java {@link Class} to execute.
   * @param args array of {@link String arguments} passed to the Java program.
   * @return a {@link ProcessAdapter} representing the forked {@link Process} executing the given Java {@link Class}.
   * @see RuntimeProcessExecutor#execute(File, String...)
   * @see org.cp.elements.process.ProcessAdapter
   */
  public ProcessAdapter execute(File directory, Class<?> type, String... args) {
    return super.execute(directory, toJavaCommandLine(type, args));
  }

  /**
   * Forks and executes the given Java {@link File JAR File} in the {@link File current working directory}
   * of the currently executing Java {@link Process}, passing the provided array of {@link String arguments}
   * on the command-line to the Java program.
   *
   * @param jarFile Java {@link File JAR File} containing the Java program to execute.
   * @param args array of {@link String arguments} passed to the Java program.
   * @return a {@link ProcessAdapter} representing the forked {@link Process} executing
   * the given Java {@link File JAR File}.
   * @see org.cp.elements.process.ProcessAdapter
   * @see #execute(File, File, String...)
   */
  public ProcessAdapter execute(File jarFile, String... args) {
    return execute(FileSystemUtils.WORKING_DIRECTORY, jarFile, args);
  }

  /**
   * Forks and executes the given Java {@link File JAR File} in the specified {@link File working directory}
   * of the currently executing Java {@link Process}, passing the provided array of {@link String arguments}
   * on the command-line to the Java program.
   *
   * @param directory {@link File directory} in which to execute the Java program.
   * @param jarFile Java {@link File JAR File} containing the Java program to execute.
   * @param args array of {@link String arguments} passed to the Java program.
   * @return a {@link ProcessAdapter} representing the forked {@link Process} executing
   * the given Java {@link File JAR File}.
   * @see RuntimeProcessExecutor#execute(File, String...)
   * @see org.cp.elements.process.ProcessAdapter
   */
  public ProcessAdapter execute(File directory, File jarFile, String... args) {
    return super.execute(directory, toJavaCommandLine(jarFile, args));
  }

  /**
   * Builds a Java executable command-line with the given {@link Class} type and program {@link String arguments}.
   *
   * @param type Java {@link Class} to execute.
   * @param args array of {@link String arguments} passed to the Java program.
   * @return a {@link String} array containing the elements of the Java command-line.
   * @see java.lang.Class
   */
  protected String[] toJavaCommandLine(Class<?> type, String... args) {
    List<String> javaCommandLine = new ArrayList<>();

    javaCommandLine.add(FileSystemUtils.JAVA_EXE.getAbsolutePath());
    javaCommandLine.add("-server");
    javaCommandLine.add("-classpath");
    javaCommandLine.add(System.getProperty("java.class.path"));
    javaCommandLine.add(type.getName());
    javaCommandLine.addAll(Arrays.asList(ArrayUtils.nullSafeArray(args, String.class)));

    return javaCommandLine.toArray(new String[javaCommandLine.size()]);
  }

  /**
   * Builds a Java executable command-line with the given {@link File JAR File} and program {@link String arguments}.
   *
   * @param jarFile Java {@link File JAR File} to execute.
   * @param args array of {@link String arguments} passed to the Java program.
   * @return a {@link String} array containing the elements of the Java command-line.
   * @see java.io.File
   */
  protected String[] toJavaCommandLine(File jarFile, String... args) {
    List<String> javaCommandLine = new ArrayList<>();

    javaCommandLine.add(FileSystemUtils.JAVA_EXE.getAbsolutePath());
    javaCommandLine.add("-jar");
    javaCommandLine.add(FileSystemUtils.tryGetCanonicalPathElseGetAbsolutePath(jarFile));
    javaCommandLine.addAll(Arrays.asList(ArrayUtils.nullSafeArray(args, String.class)));

    return javaCommandLine.toArray(new String[javaCommandLine.size()]);
  }
}
