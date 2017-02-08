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

import static org.cp.elements.lang.ObjectUtils.isNullOrEqualTo;
import static org.cp.elements.lang.ObjectUtils.returnValueOrThrowIfNull;
import static org.cp.elements.util.ArrayUtils.indexOf;
import static org.cp.elements.util.ArrayUtils.nullSafeArray;
import static org.cp.elements.util.stream.StreamUtils.stream;

import java.io.File;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.concurrent.Callable;

import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.NullSafe;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.process.EmbeddedProcessExecutionException;
import org.cp.elements.process.ProcessExecutor;

/**
 * The EmbeddedJavaProcessExecutor class...
 *
 * @author John Blum
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class EmbeddedJavaProcessExecutor implements ProcessExecutor {

  @Override
  public Process execute(File directory, String... commandLine) {
    Assert.isTrue(isNullOrEqualTo(directory, FileSystemUtils.WORKING_DIRECTORY),
      "The Java class can only be ran in the same working directory as the containing process");

    Class type = resolveJavaClassFrom(commandLine);

    Assert.notNull(type, "The Java class to run could not be resolved from the given command-line [%s]",
      Arrays.toString(commandLine));

    String[] args = resolveArgumentsFrom(type, commandLine);

    execute(type, args);

    return null;
  }

  protected String[] resolveArgumentsFrom(Class type, String... args) {
    int index = indexOf(args, type.getName());
    int position = (index + 1);

    String[] arguments = StringUtils.EMPTY_STRING_ARRAY;

    if (index != -1 && position < args.length) {
      int length = (args.length - position);
      arguments = new String[length];
      System.arraycopy(args, position, arguments, 0, length);
    }

    return arguments;
  }

  protected <T> Class<T> resolveJavaClassFrom(String... args) {
    for (String arg : nullSafeArray(args, String.class)) {
      if (ObjectUtils.isPresent(arg)) {
        return ObjectUtils.loadClass(arg);
      }
    }

    return null;
  }

  @SuppressWarnings("unchecked")
  public <T> T execute(Class type, String... args) {
    Assert.notNull(type, "Class type must not be null");

    if (isCallable(type)) {
      return invokeCallable(type, args);
    }
    else if (isRunnable(type)) {
      return invokeRunnable(type, args);
    }
    else {
      return invokeMainMethod(type, args);
    }
  }

  protected boolean isCallable(Class type) {
    return Callable.class.isAssignableFrom(type);
  }

  @SuppressWarnings("unchecked")
  protected <T> T invokeCallable(Class type, String[] args) {
    try {
      Callable<T> callable = this.<Callable<T>>constructInstance(type, args);
      return callable.call();
    }
    catch (Exception e) {
      throw new EmbeddedProcessExecutionException(String.format("Failed to execute Java class [%s]", type), e);
    }
  }

  protected boolean isRunnable(Class type) {
    return Runnable.class.isAssignableFrom(type);
  }

  @SuppressWarnings("unchecked")
  protected <T> T invokeRunnable(Class type, String[] args) {
    Runnable runnable = this.<Runnable>constructInstance(type, args);
    runnable.run();
    return null;
  }

  private <T> T invokeMainMethod(Class<?> type, Object[] args) {
    try {
      Method mainMethod = type.getDeclaredMethod(ObjectUtils.MAIN_METHOD_NAME, String[].class);
      mainMethod.invoke(null, args);
      return null;
    }
    catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException e) {
      throw new EmbeddedProcessExecutionException(String.format("Failed to execute Java class [%s]", type), e);
    }
  }

  protected <T> T constructInstance(Class<T> type, String[] args) {
    try {
      Constructor<T> constructor = findConstructor(type);
      constructor.setAccessible(true);
      return constructor.newInstance((Object[]) args);
    }
    catch (IllegalAccessException | InstantiationException | InvocationTargetException e) {
      throw new EmbeddedProcessExecutionException(
        String.format("Failed to construct an instance of Java class [%s]", type), e);
    }
  }

  @SuppressWarnings("unchecked")
  protected <T> Constructor<T> findConstructor(Class<T> type) {
    Constructor<T> foundConstructor = (Constructor<T>) stream(nullSafeArray(
      type.getDeclaredConstructors(), Constructor.class)).filter((this::isTargetConstructor)).sorted(
        (constructorOne, constructorTwo) -> constructorTwo.getParameterCount() - constructorOne.getParameterCount())
          .findFirst().orElse(null);

    return returnValueOrThrowIfNull(foundConstructor, new EmbeddedProcessExecutionException(String.format(
      "Unable to find default constructor or a constructor with arguments (%1$s(:String[]) for type [%2$s]",
        type.getSimpleName(), type.getName())));
  }

  @NullSafe
  protected boolean isTargetConstructor(Constructor<?> constructor) {
    return (constructor != null && (ObjectUtils.isDefaultConstructor(constructor)
      || ObjectUtils.isConstructorWithArgumentArrayParameter(constructor)));
  }
}
