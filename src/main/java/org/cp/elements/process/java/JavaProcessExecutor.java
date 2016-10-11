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
import org.cp.elements.process.support.ProcessBuilderProcessExecutor;
import org.cp.elements.util.ArrayUtils;

/**
 * The {@link JavaProcessExecutor} class is an implementation of {@link org.cp.elements.process.ProcessExecutor}
 * and extension of {@link ProcessBuilderProcessExecutor} used to execute and launch Java application processes
 * (i.e. Java classes with a {@literal main} method).
 *
 * @author John J. Blum
 * @see org.cp.elements.process.support.ProcessBuilderProcessExecutor
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class JavaProcessExecutor extends ProcessBuilderProcessExecutor {

  public Process execute(Class<?> type, String... args) {
    return execute(FileSystemUtils.WORKING_DIRECTORY, type, args);
  }

  public Process execute(File directory, Class<?> type, String... args) {
    return execute(directory, toJavaCommandLine(type, args));
  }

  public Process execute(File jarFile, String... args) {
    return execute(FileSystemUtils.WORKING_DIRECTORY, jarFile, args);
  }

  public Process execute(File directory, File jarFile, String... args) {
    return execute(directory, toJavaCommandLine(jarFile, args));
  }

  protected String[] toJavaCommandLine(Class<?> type, String... args) {
    List<String> javaCommandLine = new ArrayList<>();

    javaCommandLine.add(FileSystemUtils.JAVA_EXE.getAbsolutePath());
    javaCommandLine.add(type.getName());
    javaCommandLine.addAll(Arrays.asList(ArrayUtils.nullSafeArray(args, String.class)));

    return javaCommandLine.toArray(new String[javaCommandLine.size()]);
  }

  protected String[] toJavaCommandLine(File jarFile, String... args) {
    List<String> javaCommandLine = new ArrayList<>();

    javaCommandLine.add(FileSystemUtils.JAVA_EXE.getAbsolutePath());
    javaCommandLine.add("-jar");
    javaCommandLine.add(FileSystemUtils.tryGetCanonicalPathElseGetAbsolutePath(jarFile));

    return javaCommandLine.toArray(new String[javaCommandLine.size()]);
  }
}
