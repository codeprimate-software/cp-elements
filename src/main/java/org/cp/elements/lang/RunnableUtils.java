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

package org.cp.elements.lang;

import java.util.Optional;

import org.cp.elements.lang.concurrent.ThreadUtils;

/**
 * The {@link RunnableUtils} class is a utility class for working with {@link Runnable} objects.
 *
 * @author John Blum
 * @see java.lang.Runnable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class RunnableUtils {

  public static boolean runWithPause(long milliseconds, Runnable runnable) {
    Assert.isTrue(milliseconds > 0, "Milliseconds [%d] must be greater than 0", milliseconds);
    runnable.run();
    return ThreadUtils.sleep(milliseconds, 0);
  }

  public static boolean runWithPauseUninterrupted(long milliseconds, Runnable runnable) {
    Assert.isTrue(milliseconds > 0, "Milliseconds [%d] must be greater than 0", milliseconds);
    runnable.run();
    return safeSleep(milliseconds);
  }

  private static boolean safeSleep(long milliseconds) {

    boolean interrupted = false;

    long timeout = System.currentTimeMillis() + milliseconds;

    while (System.currentTimeMillis() < timeout) {
      try {
        Thread.sleep(milliseconds);
      }
      catch (InterruptedException cause) {
        interrupted = true;
      }
    }

    if (interrupted) {
      Thread.currentThread().interrupt();
    }

    return true;
  }

  /**
   * Times the {@link Runnable#run()} method of the given {@link Runnable}.
   *
   * @param runnable the {@link Runnable} object to run and time.
   * @return the amount of time in milliseconds that it took to run the given {@link Runnable}.
   * @see java.lang.Runnable
   */
  public static Optional<Long> timedRun(Runnable runnable) {
    long t0 = System.currentTimeMillis();
    runnable.run();
    return Optional.of(System.currentTimeMillis() - t0);
  }
}
