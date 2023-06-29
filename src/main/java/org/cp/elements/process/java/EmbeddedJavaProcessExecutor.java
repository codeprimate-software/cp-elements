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
package org.cp.elements.process.java;

import static org.cp.elements.lang.ClassUtils.getName;
import static org.cp.elements.lang.ClassUtils.getSimpleName;
import static org.cp.elements.lang.ClassUtils.isConstructorWithArrayParameter;
import static org.cp.elements.lang.ClassUtils.isDefaultConstructor;
import static org.cp.elements.lang.ElementsExceptionsFactory.newEmbeddedProcessExecutionException;
import static org.cp.elements.lang.ObjectUtils.isNullOrEqualTo;
import static org.cp.elements.util.ArrayUtils.indexOf;
import static org.cp.elements.util.ArrayUtils.nullSafeArray;
import static org.cp.elements.util.ArrayUtils.toStringArray;
import static org.cp.elements.util.stream.StreamUtils.stream;

import java.io.File;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.Callable;

import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ClassUtils;
import org.cp.elements.lang.Executable;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.process.EmbeddedProcessExecutionException;
import org.cp.elements.process.ProcessExecutor;

/**
 * Launches an embedded Java process.
 *
 * @author John Blum
 * @see java.io.File
 * @see org.cp.elements.lang.Executable
 * @see org.cp.elements.process.ProcessExecutor
 * @since 1.0.0
 */
@SuppressWarnings({ "rawtypes", "unused" })
public class EmbeddedJavaProcessExecutor implements ProcessExecutor<Void> {

  protected static final Collection<JavaClassExecutor> JAVA_CLASS_EXECUTORS =
    List.of(new ExecutableExecutor<>(), new RunnableExecutor<>(), new CallableExecutor<>(), new MainMethodExecutor<>());

  /**
   * Factory method used to construct a new {@link EmbeddedJavaProcessExecutor}, which is used to
   * execute Java {@link Class Classes} embedded in the currently running Java program.
   *
   * @return a new instance of the {@link EmbeddedJavaProcessExecutor} class.
   * @see org.cp.elements.process.java.EmbeddedJavaProcessExecutor
   */
  public static EmbeddedJavaProcessExecutor newEmbeddedJavaProcessExecutor() {
    return new EmbeddedJavaProcessExecutor();
  }

  /**
   * Executes the given {@link String command-line} executable in the given {@link File directory}.
   *
   * @return {@link Void}.  The Java {@link Process} is run in embedded mode and therefore will not
   * have a forked {@link Process}.
   * @throws IllegalArgumentException if the {@link File directory} is the current working directory or {@literal null},
   * or the Java {@link Class} could not be resolved from the given command-line.
   * @see org.cp.elements.process.ProcessExecutor#execute(File, String...)
   * @see #execute(Class, String...)
   * @see java.io.File
   */
  @Override
  public Void execute(File directory, String... commandLine) {

    Assert.isTrue(isNullOrEqualTo(directory, FileSystemUtils.WORKING_DIRECTORY),
      "The Java class can only be ran in the same working directory [%1$s] as the containing process"
        + "; directory was [%2$s]", FileSystemUtils.WORKING_DIRECTORY, directory);

    Class type = resolveJavaClassFrom(commandLine);

    Assert.notNull(type, "The Java class to execute could not be resolved from the given command-line [%s]",
      Arrays.toString(commandLine));

    String[] args = resolveArgumentsFrom(type, commandLine);

    execute(type, args);

    return null;
  }

  /**
   * Resolves the arguments to the Java program identified by the given {@link Class} type
   * from the given array of {@link String arguments}.
   *
   * @param type Java {@link Class} containing the {@literal main} method of the Java program;
   * must not be {@literal null}.
   * @param args array of {@link String arguments} from which to extract the Java program arguments.
   * @return the array of {@link String arguments} to be passed to the Java program.
   */
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

  /**
   * Resolves the Java {@link Class} containing the {@literal main} method to the Java program to execute
   * from the given array of {@link String arguments}.
   *
   * @param <T> {@link Class} type of the main Java program.
   * @param args array of {@link String arguments} from which to extract the main Java {@link Class}
   * of the Java program.
   * @return the Java {@link Class} containing the {@literal main} method to the Java program.
   * @see java.lang.Class
   */
  protected <T> Class<T> resolveJavaClassFrom(String... args) {
    return Arrays.stream(nullSafeArray(args, String.class)).filter(ObjectUtils::isPresent).findFirst()
      .<Class<T>>map(ObjectUtils::loadClass).orElse(null);
  }

  /**
   * Executes the given Java {@link Class} passing the given array of {@link String arguments}.
   * <p>
   * This {@code execute} method employs a {@link JavaClassExecutor} strategy to execute the given Java {@link Class}.
   * The Java {@link Class} is expected to either implement {@link Runnable}, {@link Callable}, {@link Executable}
   * or implement a {@literal main} {@link Method}.
   *
   * @param <T> {@link Class} type of the Java program return value.
   * @param type Java {@link Class} to execute in embedded mode.
   * @param args array of {@link String arguments} to pass to the Java {@link Class}.
   * @return an {@link Optional} return value from the execution of the Java {@link Class}.
   * @throws IllegalArgumentException if the Java {@link Class} is {@literal null}.
   * @throws EmbeddedProcessExecutionException if the Java {@link Class} is not executable.
   * @see org.cp.elements.process.java.EmbeddedJavaProcessExecutor.JavaClassExecutor
   * @see java.lang.Class
   * @see java.util.Optional
   */
  @SuppressWarnings("unchecked")
  public <T> Optional<T> execute(Class type, String... args) {

    Assert.notNull(type, "Class type must not be null");

    return JAVA_CLASS_EXECUTORS.stream().filter(javaClassExecutor -> javaClassExecutor.isExecutable(type))
      .findFirst().map(javaClassExecutor -> javaClassExecutor.execute(type, (Object[]) args)).orElseThrow(() ->
        new EmbeddedProcessExecutionException(String.format("Unable to execute Java class [%s];"
          + " Please verify that your class either implements Runnable, Callable, Executable or has a main method",
            getName(type))));
  }

  /**
   * Strategy interface for executing Java {@link Class Classes}.
   *
   * @param <T> {@link Class} type of the executions return value (result).
   */
  interface JavaClassExecutor<T> {

    /**
     * Determines whether the given Java {@link Class} is executable by this executor.
     *
     * @param type Java {@link Class} to evaluate.
     * @return a boolean value indicating whether this executor can execute the given Java {@link Class}.
     * @see java.lang.Class
     */
    boolean isExecutable(Class type);

    /**
     * Executes the given Java {@link Class} passing the array of {@link String arguments} used during execution.
     *
     * @param type Java {@link Class} to execute.
     * @param args array of {@link String arguments} passed during execution.
     * @return the {@link Optional} return value (result) from the execution.
     * @see java.lang.Class
     * @see java.util.Optional
     */
    Optional<T> execute(Class type, Object... args);

    @NullSafe
    default boolean isTargetConstructor(Constructor<?> constructor) {

      return Optional.ofNullable(constructor).map(localConstructor ->
        isDefaultConstructor(constructor) || isConstructorWithArrayParameter(constructor))
          .orElse(false);
    }

    @SuppressWarnings("unchecked")
    default <T> Constructor<T> findConstructor(Class<T> type) {

      return (Constructor<T>) stream(nullSafeArray(type.getDeclaredConstructors(), Constructor.class))
        .filter(this::isTargetConstructor)
        .min((constructorOne, constructorTwo) ->
          constructorTwo.getParameterCount() - constructorOne.getParameterCount())
        .orElseThrow(() -> new EmbeddedProcessExecutionException(String.format(
          "No default constructor or constructor with arguments (%1$s(:Object[]) for type [%2$s] was found",
            getSimpleName(type), getName(type))));
    }

    default <T> T constructInstance(Class<T> type, Object[] args) {

      try {
        Constructor<T> constructor = findConstructor(type);

        constructor.setAccessible(true);

        return (isConstructorWithArrayParameter(constructor) ? constructor.newInstance((Object) args)
          : constructor.newInstance());
      }
      catch (IllegalAccessException | InstantiationException | InvocationTargetException cause) {
        throw newEmbeddedProcessExecutionException(cause, "Failed to construct an instance of Java class [%s]",
          getName(type));
      }
    }
  }

  /**
   * Execution strategy for executing {@link Callable} Java {@link Class Classes}.
   *
   * @param <T> {@link Class} type of the executions return value (result).
   * @see java.util.concurrent.Callable
   */
  static class CallableExecutor<T> implements JavaClassExecutor<T> {

    @Override
    public boolean isExecutable(Class type) {
      return Callable.class.isAssignableFrom(type);
    }

    @Override
    @SuppressWarnings("unchecked")
    public Optional<T> execute(Class type, Object... args) {

      try {
        Callable<T> callable = this.<Callable<T>>constructInstance(type, args);
        return Optional.ofNullable(callable.call());
      }
      catch (Exception cause) {
        throw newEmbeddedProcessExecutionException(cause, "Failed to call Java class [%s]", getName(type));
      }
    }
  }

  /**
   * Execution strategy for executing {@link Executable} Java {@link Class Classes}.
   *
   * @param <T> {@link Class} type of the executions return value (result).
   * @see org.cp.elements.lang.Executable
   */
  static class ExecutableExecutor<T> implements JavaClassExecutor<T> {

    @Override
    public boolean isExecutable(Class type) {
      return Executable.class.isAssignableFrom(type);
    }

    @SuppressWarnings("unchecked")
    public Optional<T> execute(Class type, Object... args) {
      Executable<T> executable = this.<Executable<T>>constructInstance(type, args);
      return Optional.ofNullable(executable.execute(args));
    }
  }

  /**
   * Execution strategy for executing Java {@link Class Classes} having a {@literal main} {@link Method}.
   *
   * @param <T> {@link Class} type of the executions return value (result).
   */
  static class MainMethodExecutor<T> implements JavaClassExecutor<T> {

    @Override
    public boolean isExecutable(Class type) {
      return Arrays.stream(type.getDeclaredMethods()).anyMatch(ClassUtils::isMainMethod);
    }

    @Override
    @SuppressWarnings("unchecked")
    public Optional<T> execute(Class type, Object... args) {

      try {
        Method mainMethod = type.getDeclaredMethod(ClassUtils.MAIN_METHOD_NAME, String[].class);

        mainMethod.invoke(null, (Object) toStringArray(args));

        return Optional.empty();
      }
      catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException cause) {
        throw newEmbeddedProcessExecutionException(cause, "Failed to execute Java class [%s] using main method",
          getName(type));
      }
    }
  }

  /**
   * Execution strategy for executing {@link Runnable} Java {@link Class Classes}.
   *
   * @param <T> {@link Class} type of the executions return value (result).
   * @see java.lang.Runnable
   */
  static class RunnableExecutor<T> implements JavaClassExecutor<T> {

    @Override
    public boolean isExecutable(Class type) {
      return Runnable.class.isAssignableFrom(type);
    }

    @Override
    @SuppressWarnings("unchecked")
    public Optional<T> execute(Class type, Object... args) {

      Runnable runnable = this.<Runnable>constructInstance(type, args);

      runnable.run();

      return Optional.empty();
    }
  }
}
