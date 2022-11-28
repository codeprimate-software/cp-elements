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

import java.util.function.Function;
import java.util.function.Predicate;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * {@link CodeBlocks} is an abstract base class used to simplify code for common code blocks, such as
 * {@literal if-then-else} statements, {@literal try-catch} and {@literal try-catch-finally} blocks,
 * {@literal while-loops}, and so on.
 *
 * @author John Blum
 * @see java.lang.Runnable
 * @see java.util.function.Function
 * @see java.util.function.Predicate
 * @since 1.0.0
 */
@SuppressWarnings({ "AbstractClassName", "unused" })
public abstract class CodeBlocks {

  public static final Runnable NO_OP_RUNNABLE = () -> { };

  /**
   * Implementation of an {@literal if-else} code block.
   *
   * @param <T> {@link Class type} of the {@link Object target} processed in the {@literal if} statement.
   * @param <R> {@link Class type} of the {@link Object result} returned from the {@literal if} statement.
   * @param target {@link Object} to evaluate in the {@literal if} statement.
   * @param ifCondition {@link Predicate} used to evaluate the {@link Object target} in the {@literal if} condition.
   * @param ifBlock {@link Function} encapsulating the logic in the {@literal if} block.
   * @param elseBlock {@link Function} encapsulating the logic in the {@literal else} block.
   * @return the {@link Object result} from the evaluation of the {@literal if} statement.
   * @throws IllegalArgumentException if the {@link Predicate if-condition}, {@link Function if-block}
   * or the {@link Function else-block} are {@literal null}.
   * @see java.util.function.Function
   * @see java.util.function.Predicate
   */
  public static <T, R> R ifElse(@Nullable T target, @NotNull Predicate<T> ifCondition,
      @NotNull Function<T, R> ifBlock, @NotNull Function<T, R> elseBlock) {

    Assert.notNull(ifCondition, "The Predicate used in the condition of the if statement is required");
    Assert.notNull(ifBlock, "The Function for the if block is required");
    Assert.notNull(elseBlock, "The Function for the else block is required");

    //return ifCondition.test(target) ? ifBlock.apply(target) : elseBlock.apply(target);

    if (ifCondition.test(target)) {
      return ifBlock.apply(target);
    }
    else {
      return elseBlock.apply(target);
    }
  }

  /**
   * Implementation of the {@literal try-catch} code block, which handles the {@link Throwable} object
   * by rethrowing a {@link RuntimeException} determined by the given, required {@link Function}.
   *
   * @param <T> {@link Class type} of the {@link Object result} from processing the code in the {@literal try} block.
   * @param tryBlock {@link Function} encapsulating the logic invoked in the {@literal try} block. The processing logic
   * in this case is capable of throwing an {@link Error} or {@link Exception}.
   * @param catchBlock {@link Function} used to convert the {@link Throwable} into a {@link RuntimeException}.
   * @return the {@link Object result} of processing the code in the {@literal try} block.
   * @throws IllegalArgumentException if the {@literal try-block} {@link ThrowableOperation} or {@link Throwable}
   * converting {@link Function} are {@literal null}.
   * @see #tryCatchFinally(ThrowableOperation, Function, Runnable)
   * @see org.cp.elements.lang.ThrowableOperation
   * @see java.util.function.Function
   */
  public static <T> T tryCatch(@NotNull ThrowableOperation<T> tryBlock,
      @NotNull Function<Throwable, ? extends RuntimeException> catchBlock) {

    return tryCatchFinally(tryBlock, catchBlock, NO_OP_RUNNABLE);
  }

  /**
   * Implementation of the {@literal try-catch} code block, which handles the {@link Throwable} object
   * by rethrowing a {@link RuntimeException} determined by the given, required {@link Function}.
   *
   * @param <T> {@link Class type} of the {@link Object result} from processing the code in the {@literal try} block.
   * @param tryBlock {@link Function} encapsulating the logic invoked in the {@literal try} block. The processing logic
   * in this case is capable of throwing an {@link Error} or {@link Exception}.
   * @param catchBlock {@link Function} used to convert the {@link Throwable} into a {@link RuntimeException}.
   * @param finallyBlock {@link Runnable} encapsulating the logic invoked in the {@literal finally} block.
   * @return the {@link Object result} of processing the code in the {@literal try} block.
   * @throws IllegalArgumentException if the {@literal try-block} {@link ThrowableOperation}, {@link Throwable}
   * converting {@link Function} or {@literal finally-block} {@link Runnable} are {@literal null}.
   * @see org.cp.elements.lang.ThrowableOperation
   * @see java.util.function.Function
   * @see java.lang.Runnable
   */
  public static <T> T tryCatchFinally(@NotNull ThrowableOperation<T> tryBlock,
      @NotNull Function<Throwable, ? extends RuntimeException> catchBlock, @NotNull Runnable finallyBlock) {

    Assert.notNull(tryBlock, "The ThrowableOperation invoked in the try block is required");
    Assert.notNull(catchBlock, "The Function converter invoked in the catch block is required");
    Assert.notNull(finallyBlock, "The Runnable invoked in the finally block is required");

    try {
      return tryBlock.run(ObjectUtils.EMPTY_OBJECT_ARRAY);
    }
    catch (Throwable cause) {
      throw catchBlock.apply(cause);
    }
    finally {
      finallyBlock.run();
    }
  }

  /**
   * Implementation of the {@literal try-catch} code block, which handles the {@link Throwable} object
   * by returning a {@link Object result}, such as an alternative {@link Object value}.
   *
   * @param <T> {@link Class type} of the {@link Object result} from processing the code in the {@literal try} block.
   * @param tryBlock {@link Function} encapsulating the logic invoked in the {@literal try} block. The processing logic
   * in this case is capable of throwing an {@link Error} or {@link Exception}.
   * @param catchBlock {@link Function} used to handle the {@link Throwable} and return a {@link Object result}.
   * @return the {@link Object result} of processing the code in the {@literal try} block if no {@link Error}
   * or {@link Exception} is thrown, or returns the {@link Object value} computed from the {@link Throwable}
   * {@link Function} handler.
   * @throws IllegalArgumentException if the {@literal try-block} {@link ThrowableOperation} or {@link Throwable}
   * handling {@link Function} are {@literal null}.
   * @see #tryHandleFinally(ThrowableOperation, Function, Runnable)
   * @see org.cp.elements.lang.ThrowableOperation
   * @see java.util.function.Function
   */
  public static <T> T tryHandle(@NotNull ThrowableOperation<T> tryBlock, @NotNull Function<Throwable, T> catchBlock) {
    return tryHandleFinally(tryBlock, catchBlock, NO_OP_RUNNABLE);
  }

  /**
   * Implementation of the {@literal try-catch} code block, which handles the {@link Throwable} object
   * by returning a {@link Object result}, such as an alternative {@link Object value}.
   *
   * @param <T> {@link Class type} of the {@link Object result} from processing the code in the {@literal try} block.
   * @param tryBlock {@link Function} encapsulating the logic invoked in the {@literal try} block. The processing logic
   * in this case is capable of throwing an {@link Error} or {@link Exception}.
   * @param catchBlock {@link Function} used to handle the {@link Throwable} and return a {@link Object result}.
   * @param finallyBlock {@link Runnable} encapsulating the logic invoked in the {@literal finally} block.
   * @return the {@link Object result} of processing the code in the {@literal try} block if no {@link Error}
   * or {@link Exception} is thrown, or returns the {@link Object value} computed from the {@link Throwable}
   * {@link Function} handler.
   * @throws IllegalArgumentException if the {@literal try-block} {@link ThrowableOperation} or {@link Throwable}
   * handling {@link Function} are {@literal null}.
   * @see org.cp.elements.lang.ThrowableOperation
   * @see java.util.function.Function
   * @see java.lang.Runnable
   */
  public static <T> T tryHandleFinally(@NotNull ThrowableOperation<T> tryBlock,
      @NotNull Function<Throwable, T> catchBlock, @NotNull Runnable finallyBlock) {

    Assert.notNull(tryBlock, "The ThrowableOperation invoked in the try block is required");
    Assert.notNull(catchBlock, "The Function handler invoked in the catch block is required");
    Assert.notNull(finallyBlock, "The Runnable invoked in the finally block is required");

    try {
      return tryBlock.run(ObjectUtils.EMPTY_OBJECT_ARRAY);
    }
    catch (Throwable cause) {
      return catchBlock.apply(cause);
    }
    finally {
      finallyBlock.run();
    }
  }

  /**
   * Implementation of the {@literal while-loop} code block.
   *
   * @param <T> {@link Class type} of the {@link Object target} being processed in the {@literal while-loop}.
   * @param target {@link Object} being processed in the {@literal while-loop}
   * @param whileCondition {@link Predicate} encapsulating the condition used in the {@literal while-loop}.
   * @param whileBlock {@link Function} encapsulating the logic used to process the {@link Object target}
   * for each iteration of the {@literal while-loop}.
   * @return the {@link Object result} of processing the {@link Object target} in the {@literal while-loop}.
   * @see java.util.function.Function
   * @see java.util.function.Predicate
   */
  public static <T> T whileLoop(@Nullable T target,
      @NotNull Predicate<T> whileCondition, @NotNull Function<T, T> whileBlock) {

    Assert.notNull(whileCondition, "The condition for the while-loop is required");
    Assert.notNull(whileBlock, "The Function of the while-loop is required");

    T result = target;

    while (whileCondition.test(result)) {
      result = whileBlock.apply(result);
    }

    return result;
  }
}
