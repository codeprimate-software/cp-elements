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
import org.cp.elements.lang.Composite;

/**
 * The ComposableRunnable class is an implementation of the Runnable interface and the Composite Design Pattern to
 * compose multiple Runnable objects and treat it as a single instance of Runnable.
 *
 * @author John J. Blum
 * @see java.lang.Runnable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ComposableRunnable implements Composite<Runnable>, Runnable {

  protected static final ComposableRunnable INSTANCE = new ComposableRunnable();

  private final Runnable leftRunnable;
  private final Runnable rightRunnable;

  /**
   * Returns the single instance of {@link ComposableRunnable} used to compose 2 or more indiviudal {@link Runnable}
   * objects into a {@link Composite} {@link Runnable} object.
   *
   * @return the single instance of {@link ComposableRunnable}.
   * @see org.cp.elements.lang.support.ComposableRunnable
   */
  public static ComposableRunnable getInstance() {
    return INSTANCE;
  }

  /**
   * Default, private constructor used to construct a Singleton instance of the {@link ComposableRunnable} used to
   * compose 2 or more individual {@link Runnable} objects into a Composite {@link Runnable} object.
   */
  private ComposableRunnable() {
    this.leftRunnable = () -> {};
    this.rightRunnable = () -> {};
  }

  /**
   * Constructs an instance of the ComposableRunnable class composed an initialized with 2 Runnable objects.
   *
   * @param leftRunnable the left Runnable node in the composed graph.
   * @param rightRunnable the right Runnable node in the composed graph.
   * @throws NullPointerException if either the left or right Runnable object is null.
   */
  private ComposableRunnable(Runnable leftRunnable, Runnable rightRunnable) {
    Assert.notNull(leftRunnable, "The left Runnable object cannot be null");
    Assert.notNull(rightRunnable, "The right Runnable object cannot be null");

    this.leftRunnable = leftRunnable;
    this.rightRunnable = rightRunnable;
  }

  /**
   * Composes two {@link Runnable} objects into a single, compound {@link Runnable} object.
   *
   * Returns the {@code leftRunnable} if {@code rightRunnable} is {@literal null}
   * and {@code rightRunnable} if the {@code leftRunnable} is {@literal null}.
   *
   * @param leftRunnable {@link Runnable} object in the composition.
   * @param rightRunnable {@link Runnable} object in the composition.
   * @return a {@link Runnable} object composed with the given left and right {@link Runnable} objects.
   * @see java.lang.Runnable
   */
  public Runnable compose(Runnable leftRunnable, Runnable rightRunnable) {
    return (leftRunnable == null ? rightRunnable : (rightRunnable == null ? leftRunnable
      : new ComposableRunnable(leftRunnable, rightRunnable)));
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
