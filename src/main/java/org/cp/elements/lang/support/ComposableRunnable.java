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

package org.cp.elements.lang.support;

import org.cp.elements.lang.Assert;

/**
 * The ComposableRunnable class is an implementation of the Runnable interface and the Composite Design Pattern to
 * compose multiple Runnable objects and treat it as a single instance of Runnable.
 *
 * @author John J. Blum
 * @see java.lang.Runnable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public final class ComposableRunnable implements Runnable {

  private final Runnable leftRunnable;
  private final Runnable rightRunnable;

  /**
   * Constructs an instance of the ComposableRunnable class composed an initialized with 2 Runnable objects.
   *
   * @param leftRunnable the left Runnable node in the composed graph.
   * @param rightRunnable the right Runnable node in the composed graph.
   * @throws NullPointerException if either the left or right Runnable object is null.
   */
  private ComposableRunnable(final Runnable leftRunnable, final Runnable rightRunnable) {
    Assert.notNull(leftRunnable, "The left node Runnable object cannot be null!");
    Assert.notNull(rightRunnable, "The right node Runnable object cannot be null!");

    this.leftRunnable = leftRunnable;
    this.rightRunnable = rightRunnable;
  }

  /**
   * Composes 2 Runnable objects into a single, compound Runnable object.  Returns the left Runnable if the right is
   * null and the right if the left is null.
   *
   * @param leftRunnable the left Runnable node in the composed graph.
   * @param rightRunnable the right Runnable node in the composed graph.
   * @return a Runnable object composed with the left and right Runnable objects.
   * @see #compose(Runnable...)
   */
  public static Runnable compose(final Runnable leftRunnable, final Runnable rightRunnable) {
    return (rightRunnable == null ? leftRunnable : (leftRunnable == null ? rightRunnable
      : new ComposableRunnable(leftRunnable, rightRunnable)));
  }

  /**
   * Composes the array of Runnable objects into a single, compound Runnable object that are run in the order the
   * Runnable objects are contained in the array.
   *
   * @param runnables an array of Runnable objects to compose into a single, compound Runnable object.
   * @return a Runnable object composed with the Runnable objects contained in the array.
   * @see #compose(Runnable, Runnable)
   */
  public static Runnable compose(final Runnable... runnables) {
    Runnable currentRunnable = null;

    if (runnables != null) {
      for (Runnable runnable : runnables) {
        currentRunnable = compose(currentRunnable, runnable);
      }
    }

    return currentRunnable;
  }

  /**
   * Runs the left and right Runnable objects/nodes in the graph.
   *
   * @see java.lang.Runnable#run()
   */
  @Override
  public void run() {
    leftRunnable.run();
    rightRunnable.run();
  }

}
