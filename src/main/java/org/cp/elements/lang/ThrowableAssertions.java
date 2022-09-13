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
package org.cp.elements.lang;

import static org.cp.elements.lang.ElementsExceptionsFactory.newAssertionException;

import java.util.function.BiFunction;
import java.util.function.Supplier;
import java.util.regex.Pattern;

import org.cp.elements.function.FunctionUtils;
import org.cp.elements.lang.annotation.Dsl;
import org.cp.elements.lang.annotation.FluentApi;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.text.FormatUtils;
import org.cp.elements.util.ArrayUtils;

/**
 * Abstract base class defining assertions for {@link Throwable Throwables}, such as {@link Exception Exceptions},
 * {@link RuntimeException RuntimeExceptions} and {@link Error Errors}.
 *
 * @author John Blum
 * @see java.lang.Error
 * @see java.lang.Exception
 * @see java.lang.RuntimeException
 * @see java.lang.Throwable
 * @see java.util.regex.Pattern
 * @see org.cp.elements.lang.annotation.Dsl
 * @see org.cp.elements.lang.annotation.FluentApi
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class ThrowableAssertions {

  /**
   * Asserts that an {@link IllegalArgumentException} will be thrown by the invocation of a given code block.
   *
   * @return a {@link ThrowableSource} used to reference the code to invoke
   * throwing the {@link IllegalArgumentException}.
   * @see org.cp.elements.lang.ThrowableAssertions.ThrowableSource
   * @see java.lang.IllegalArgumentException
   * @see #assertThatThrowableOfType(Class)
   */
  @Dsl
  public static @NotNull ThrowableSource assertThatIllegalArgumentException() {
    return assertThatThrowableOfType(IllegalArgumentException.class);
  }

  /**
   * Asserts that an {@link IllegalStateException} will be thrown by the invocation of a given code block.
   *
   * @return a {@link ThrowableSource} used to reference the code to invoke
   * throwing the {@link IllegalStateException}.
   * @see org.cp.elements.lang.ThrowableAssertions.ThrowableSource
   * @see #assertThatThrowableOfType(Class)
   * @see java.lang.IllegalStateException
   */
  @Dsl
  public static @NotNull ThrowableSource assertThatIllegalStateException() {
    return assertThatThrowableOfType(IllegalStateException.class);
  }

  /**
   * Asserts that an {@link IndexOutOfBoundsException} will be thrown by the invocation of a given code block.
   *
   * @return a {@link ThrowableSource} used to reference the code to invoke
   * throwing the {@link IndexOutOfBoundsException}.
   * @see org.cp.elements.lang.ThrowableAssertions.ThrowableSource
   * @see java.lang.IndexOutOfBoundsException
   * @see #assertThatThrowableOfType(Class)
   */
  @Dsl
  public static @NotNull ThrowableSource assertThatIndexOutOfBoundsException() {
    return assertThatThrowableOfType(IndexOutOfBoundsException.class);
  }

  /**
   * Asserts that an {@link InterruptedException} will be thrown by the invocation of a given code block.
   *
   * @return a {@link ThrowableSource} used to reference the code to invoke
   * throwing the {@link InterruptedException}.
   * @see org.cp.elements.lang.ThrowableAssertions.ThrowableSource
   * @see #assertThatThrowableOfType(Class)
   * @see java.lang.InterruptedException
   */
  @Dsl
  public static @NotNull ThrowableSource assertThatInterruptedException() {
    return assertThatThrowableOfType(InterruptedException.class);
  }

  /**
   * Asserts that a {@link NullPointerException} will be thrown by the invocation of a given code block.
   *
   * @return a {@link ThrowableSource} used to reference the code to invoke
   * throwing the {@link NullPointerException}.
   * @see org.cp.elements.lang.ThrowableAssertions.ThrowableSource
   * @see #assertThatThrowableOfType(Class)
   * @see java.lang.NullPointerException
   */
  @Dsl
  public static @NotNull ThrowableSource assertThatNullPointerException() {
    return assertThatThrowableOfType(NullPointerException.class);
  }

  /**
   * Asserts that a {@link RuntimeException} will be thrown by the invocation of a given code block.
   *
   * @return a {@link ThrowableSource} used to reference the code to invoke
   * throwing the {@link RuntimeException}.
   * @see org.cp.elements.lang.ThrowableAssertions.ThrowableSource
   * @see #assertThatThrowableOfType(Class)
   * @see java.lang.RuntimeException
   */
  @Dsl
  public static @NotNull ThrowableSource assertThatRuntimeException() {
    return assertThatThrowableOfType(RuntimeException.class);
  }

  /**
   * Asserts that a {@link SecurityException} will be thrown by the invocation of a given code block.
   *
   * @return a {@link ThrowableSource} used to reference the code to invoke
   * throwing the {@link SecurityException}.
   * @see org.cp.elements.lang.ThrowableAssertions.ThrowableSource
   * @see #assertThatThrowableOfType(Class)
   * @see java.lang.SecurityException
   */
  @Dsl
  public static @NotNull ThrowableSource assertThatSecurityException() {
    return assertThatThrowableOfType(SecurityException.class);
  }

  /**
   * Asserts that an {@link UnsupportedOperationException} will be thrown by the invocation of a given code block.
   *
   * @return a {@link ThrowableSource} used to reference the code to invoke
   * throwing the {@link UnsupportedOperationException}.
   * @see org.cp.elements.lang.ThrowableAssertions.ThrowableSource
   * @see java.lang.UnsupportedOperationException
   * @see #assertThatThrowableOfType(Class)
   */
  @Dsl
  public static @NotNull ThrowableSource assertThatUnsupportedOperationException() {
    return assertThatThrowableOfType(UnsupportedOperationException.class);
  }

  /**
   * Asserts that some {@link Throwable} of the given, required {@link Class type} will be thrown by
   * the invocation of a given code block.
   *
   * @param type {@link Class} of the {@link Throwable} expected to be thrown by the given code block.
   * @return a {@link ThrowableSource} used to reference the code to invoke throwing a {@link Throwable}.
   * @see org.cp.elements.lang.ThrowableAssertions.ThrowableSource
   * @see java.lang.Throwable
   */
  @Dsl
  public static @NotNull ThrowableSource assertThatThrowableOfType(@NotNull Class<? extends Throwable> type) {
    return ThrowableSource.from(type);
  }

  /**
   * Interface defining a contract for asserting the details of a {@link Throwable}.
   *
   * @see org.cp.elements.lang.annotation.FluentApi
   */
  @FluentApi
  @SuppressWarnings("unused")
  public interface AssertThatThrowable {

    /**
     * Factory method used to construct a new instance of {@link AssertThatThrowable} initialized with the given,
     * expected {@link Class type} of the {@link Throwable} thrown along with the given, actual {@link Throwable cause}.
     *
     * @param type expected {@link Class type} of the {@link Throwable cause} to be thrown.
     * @param cause actual {@link Throwable cause} thrown.
     * @return a new {@link AssertThatThrowable}.
     */
    static @NotNull AssertThatThrowable from(@NotNull Class<? extends Throwable> type, @NotNull Throwable cause) {
      return AssertThatThrowableExpression.from(type, cause);
    }

    /**
     * Asserts that the current, subject {@link Throwable} was caused by a previous {@link Throwable} thrown earlier
     * in the call stack.
     *
     * @param throwableType expected {@link Class type} of the {@link Throwable cause}.
     * @return this {@link AssertThatThrowable} instance.
     * @throws AssertionException if the subject {@link Throwable} was not caused by
     * a {@link Throwable} of the given {@link Class type}.
     */
    AssertThatThrowable causedBy(Class<? extends Throwable> throwableType);

    /**
     * Asserts that the current, subject {@link Throwable} has a message equal to the given,
     * expected {@link String message}.
     *
     * @param message {@link String} containing the expected message.
     * @param args an optional, variable array of {@link Object arguments} used to replace placeholders
     * in the expected message.
     * @return this {@link AssertThatThrowable} instance.
     * @throws AssertionException if the {@link Throwable#getMessage() Throwable message} is not equal to
     * (match, case-sensitive) the given, expected {@link String message} after formatted with
     * the array of {@link Object arguments}.
     */
    AssertThatThrowable havingMessage(String message, Object... args);

    /**
     * Asserts that the current, subject {@link Throwable} has a message containing the given,
     * expected {@link String message}.
     *
     * @param message {@link String} containing the expected message.
     * @param args an optional, variable array of {@link Object arguments} used to replace the placeholders
     * in the expected message.
     * @return this {@link AssertThatThrowable} instance.
     * @throws AssertionException if the {@link Throwable#getMessage() Throwable message} does not contain
     * the given {@link String message} after formatted with the array of {@link Object arguments}.
     * @see #havingMessageMatching(String)
     */
    AssertThatThrowable havingMessageContaining(String message, Object... args);

    /**
     * Asserts that the current, subject {@link Throwable} has a message ending with the given,
     * expected {@link String message}.
     *
     * @param message {@link String} containing the expected message.
     * @param args an optional, variable array of {@link Object arguments} used to replace the placeholders
     * in the expected message.
     * @return this {@link AssertThatThrowable} instance.
     * @throws AssertionException if the {@link Throwable#getMessage() Throwable message} does not end with
     * the given {@link String message} after formatted with the array of {@link Object arguments}.
     * @see #havingMessageStartingWith(String, Object...)
     */
    AssertThatThrowable havingMessageEndingWith(String message, Object... args);

    /**
     * Asserts that the current, subject {@link Throwable} has a message matching the given
     * {@literal Regular Expression (REGEX)} {@link String pattern}.
     *
     * @param pattern {@link String} containing the {@literal Regular Expression (REGEX)}
     * used to match {@link Throwable#getMessage() Throwable message}.
     * @return this {@link AssertThatThrowable} instance.
     * @throws AssertionException if the {@link Throwable#getMessage() Throwable message}
     * does match the {@literal Regular Expression (REGEX)} {@link String pattern}.
     * @throws java.util.regex.PatternSyntaxException if the given {@link String pattern}
     * is not valid {@literal REGEX} syntax.
     * @see #havingMessageContaining(String, Object...)
     */
    AssertThatThrowable havingMessageMatching(String pattern);

    /**
     * Asserts that the current, subject {@link Throwable} has a message starting with the given,
     * expected {@link String message}.
     *
     * @param message {@link String} containing the expected message.
     * @param args an optional, variable array of {@link Object arguments} used to replace the placeholders
     * in the expected message.
     * @return this {@link AssertThatThrowable} instance.
     * @throws AssertionException if the {@link Throwable#getMessage() Throwable message} does not start with
     * the given {@link String message} after formatted with the array of {@link Object arguments}.
     * @see #havingMessageEndingWith(String, Object...)
     */
    AssertThatThrowable havingMessageStartingWith(String message, Object... args);

    /**
     * Asserts that the current, subject {@link Throwable} has no cause.
     *
     * @return this {@link AssertThatThrowable} instance.
     */
    AssertThatThrowable withNoCause();

  }

  protected static class AssertThatThrowableExpression
      implements AssertThatThrowable, DslExtension, FluentApiExtension {

    protected static AssertThatThrowableExpression from(@NotNull Class<? extends Throwable> type,
        @NotNull Throwable throwable) {

      return new AssertThatThrowableExpression(type, throwable);
    }

    private final Class<? extends Throwable> type;

    private final Throwable throwable;

    protected AssertThatThrowableExpression(@NotNull Class<? extends Throwable> type,
        @NotNull Throwable throwable) {

      Assert.notNull(type, "The type of Throwable is required");

      Assert.isInstanceOf(throwable, type, "Expected Throwable [%s] to be an instance of [%s]",
        ObjectUtils.getClassName(throwable), ObjectUtils.getName(type));

      this.type = type;
      this.throwable = throwable;
    }

    protected @NotNull Throwable getThrowable() {
      return this.throwable;
    }

    protected @NotNull Class<? extends Throwable> getType() {
      return this.type;
    }

    @Override
    @SuppressWarnings("all")
    public @NotNull AssertThatThrowable causedBy(@NotNull Class<? extends Throwable> throwableType) {

      Assert.notNull(throwableType, "A type for the cause is required");

      Throwable throwable = getThrowable();
      Throwable cause = throwable.getCause();

      if (!throwableType.isInstance(cause)) {
        throw newAssertionException("Expected Throwable [%s] to have a cause of type [%s]; but was [%s]",
          ObjectUtils.getClassName(throwable), ObjectUtils.getName(throwableType), ObjectUtils.getClassName(cause));
      }

      return AssertThatThrowableExpression.from(throwableType, cause);
    }

    @Override
    public @NotNull AssertThatThrowable havingMessage(String message, Object... args) {
      return withMessage(String::equals, "Expected message [%s]; but was [%s]", message, args);
    }

    @Override
    public @NotNull AssertThatThrowable havingMessageContaining(String message, Object... args) {
      return withMessage(String::contains, "Expected message containing [%s] in [%s]",
        message, args);
    }

    @Override
    public @NotNull AssertThatThrowable havingMessageEndingWith(String message, Object... args) {
      return withMessage(String::endsWith, "Expected message ending with [%s] in [%s]",
        message, args);
    }

    @Override
    @SuppressWarnings("all")
    public @NotNull AssertThatThrowable havingMessageMatching(@NotNull String pattern) {

      Assert.hasText(pattern, "Regular Expression (REGEX) Pattern [%s] is required", pattern);

      Pattern regularExpressionPattern = Pattern.compile(pattern);

      String message = getThrowable().getMessage();

      if (!regularExpressionPattern.matcher(message).matches()) {
        throw newAssertionException("The Throwable [%1$s] message [%2$s] does not match the pattern [%3$s]",
          getThrowable().getClass().getName(), message, pattern);
      }

      return this;
    }

    @Override
    public @NotNull AssertThatThrowable havingMessageStartingWith(String message, Object... args) {
      return withMessage(String::startsWith, "Expected message starting with [%s] in [%s]",
        message, args);
    }

    private @NotNull AssertThatThrowable withMessage(@NotNull BiFunction<String, String, Boolean> assertMessageFunction,
        @NotNull String assertionExceptionMessage,
        @NotNull String expectedMessage,
        @NotNull Object... expectedMessageArguments) {

      String resolvedExpectedMessage = FormatUtils.format(expectedMessage, expectedMessageArguments);
      String actualMessage = getThrowable().getMessage();

      boolean throwAssertException = actualMessage == null
        || !assertMessageFunction.apply(actualMessage, resolvedExpectedMessage);

      if (throwAssertException) {
        throw newAssertionException(assertionExceptionMessage, resolvedExpectedMessage, actualMessage);
      }

      return this;
    }

    @Override
    public @NotNull AssertThatThrowable withNoCause() {

      Throwable cause = getThrowable().getCause();

      if (cause != null) {
        throw newAssertionException("Expected Throwable [%s] to have no cause; but was caused by [%s]",
          ObjectUtils.getName(getType()), ObjectUtils.getClassName(cause));
      }

      return this;
    }
  }

  /**
   * Interface defining a contract for some snippet of source code that can throw a {@link Throwable},
   * such as an {@link Exception} or an {@link Error}.
   *
   * @see org.cp.elements.lang.annotation.FluentApi
   */
  @FluentApi
  public interface ThrowableSource {

    /**
     * Factory method used to construct a new instance of {@link ThrowableSource} initialized with the given, expected
     * {@link Class type} of the {@link Throwable} thrown by the code to assert.
     *
     * @param type expected {@link Class type} of the {@link Throwable} thrown by the code to assert;
     * must not be {@literal null}.
     * @return a new {@link ThrowableSource}.
     * @see ThrowableSourceExpression#from(Class)
     */
    static ThrowableSource from(@NotNull Class<? extends Throwable> type) {
      return ThrowableSourceExpression.from(type);
    }

    /**
     * Sets the description used in the {@link AssertionException} thrown when the assertion fails.
     *
     * @param message {@link String} containing the message describing the assertion failure.
     * @param args an optional, variable array of {@link Object arguments} used to replace the placeholders
     * in the {@link AssertionException} message.
     * @return this {@link ThrowableSource} instance.
     */
    ThrowableSource describedAs(String message, Object... args);

    /**
     * Configures an array of {@link Object arguments} passed to the {@link ThrowableOperation} when run
     * by the {@link ThrowableAssertions}.
     *
     * @param args array of {@link Object arguments} to pass to the {@link ThrowableOperation}.
     * @return this {@link ThrowableSource}.
     * @see #usingArguments(Supplier)
     */
    default ThrowableSource usingArguments(Object... args) {
      return usingArguments(() -> ArrayUtils.nullSafeArray(args, Object.class));
    }

    /**
     * Configures the {@link Supplier} of {@link Object arguments} passed to the {@link ThrowableOperation}
     * when run by the {@link ThrowableAssertions}.
     *
     * @param arguments {@link Supplier} of {@link Object arguments} to pass to the {@link ThrowableOperation}.
     * @return this {@link ThrowableSource}.
     * @see #usingArguments(Object...)
     */
    ThrowableSource usingArguments(Supplier<Object[]> arguments);

    /**
     * {@link ThrowableOperation Callback} containing snippet of code throwing the {@link Throwable} to invoke
     * during the assertion.
     *
     * @param operation {@link ThrowableOperation} containing the code throwing a {@link Throwable}
     * that will be invoked during the assertion.
     * @return this {@link ThrowableSource} instance.
     */
    AssertThatThrowable isThrownBy(ThrowableOperation<?> operation);

  }

  protected static class ThrowableSourceExpression implements DslExtension, FluentApiExtension, ThrowableSource {

    protected static ThrowableSourceExpression from(@NotNull Class<? extends Throwable> type) {
      return new ThrowableSourceExpression(type);
    }

    private final Class<? extends Throwable> type;

    private String description;

    private Supplier<Object[]> arguments;

    protected ThrowableSourceExpression(@NotNull Class<? extends Throwable> type) {
      this.type = ObjectUtils.requireObject(type, "The type of Throwable is required");
    }

    protected @NotNull Supplier<Object[]> getArguments() {
      return FunctionUtils.nullSafeSupplier(this.arguments);
    }

    public @NotNull String getDescription() {

      String description = this.description;

      return StringUtils.hasText(description) ? description
        : String.format("Expected Throwable of type [%s] to be thrown by operation", ObjectUtils.getName(getType()));
    }

    protected @NotNull Class<? extends Throwable> getType() {
      return this.type;
    }

    /**
     * Run the {@link ThrowableOperation} passing any configured array of {@link Object arguments}.
     *
     * @param operation {@link ThrowableOperation} to run.
     * @throws Throwable {@link Throwable} object thrown by the {@link ThrowableOperation} during the run.
     * @see java.lang.Throwable
     */
    private void run(@NotNull ThrowableOperation<?> operation) throws Throwable {

      Object[] arguments = getArguments().get();

      if (ArrayUtils.isNotEmpty(arguments)) {
        operation.run(arguments);
      }
      else {
        operation.run();
      }
    }

    @Override
    public @NotNull ThrowableSource describedAs(@Nullable String message, Object... args) {

      this.description = StringUtils.hasText(message)
        ? FormatUtils.format(message, args)
        : null;

      return this;
    }

    @Override
    public @NotNull AssertThatThrowable isThrownBy(@NotNull ThrowableOperation<?> operation) {

      try {
        run(operation);
        throw newAssertionException(getDescription());
      }
      catch (AssertionException assertionException) {
        throw assertionException;
      }
      catch (ThrowableOperationException cause) {
        return AssertThatThrowable.from(getType(), cause.getCause());
      }
      catch (Throwable cause) {
        //return AssertThatThrowable.from(getType(), cause.getCause());
        return AssertThatThrowable.from(getType(), cause);
      }
    }

    @Override
    public @NotNull ThrowableSource usingArguments(@Nullable Supplier<Object[]> args) {
      this.arguments = args;
      return this;
    }
  }
}
