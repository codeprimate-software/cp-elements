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
package org.cp.elements.lang.support;

import org.cp.elements.lang.Composite;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;

/**
 * Implementation of the {@link Runnable} interface and the {@literal Composite Software Design Pattern} used to
 * compose multiple {@link Runnable} objects and treat it as a single instance of {@link Runnable}.
 *
 * @author John J. Blum
 * @see java.lang.Runnable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ComposableRunnable implements Composite<Runnable>, Runnable {

  protected static final ComposableRunnable INSTANCE = new ComposableRunnable();

  private final Runnable runnableOne;
  private final Runnable runnableTwo;

  /**
   * Returns the single instance of {@link ComposableRunnable} used to compose 2 or more individual {@link Runnable}
   * objects into a {@link Composite} {@link Runnable} object.
   *
   * @return the single instance of {@link ComposableRunnable}.
   * @see org.cp.elements.lang.support.ComposableRunnable
   */
  public static ComposableRunnable builder() {
    return INSTANCE;
  }

  /**
   * Default, private constructor used to construct a {@literal Singleton} instance of the {@link ComposableRunnable}
   * used to compose 2 or more individual {@link Runnable} objects into a {@literal Composite} {@link Runnable} object.
   */
  private ComposableRunnable() {

    this.runnableOne = () -> { };
    this.runnableTwo = () -> { };
  }

  /**
   * Constructs a new {@link ComposableRunnable} composed an initialized with 2 {@link Runnable} objects.
   *
   * @param runnableOne the left Runnable node in the composed graph.
   * @param runnableTwo the right Runnable node in the composed graph.
   * @throws NullPointerException if either the left or right Runnable object is null.
   */
  protected ComposableRunnable(@NotNull Runnable runnableOne, @NotNull Runnable runnableTwo) {

    this.runnableOne = ObjectUtils.requireObject(runnableOne, "Runnable one is required");
    this.runnableTwo = ObjectUtils.requireObject(runnableTwo, "Runnable two is required");
  }

  /**
   * Composes two {@link Runnable} objects into a single, compound {@link Runnable} object.
   * <p>
   * Returns the {@code runnableOne} if {@code runnableTwo} is {@literal null}
   * and {@code runnableTwo} if the {@code runnableOne} is {@literal null}.
   *
   * @param runnableOne {@link Runnable} object in the composition.
   * @param runnableTwo {@link Runnable} object in the composition.
   * @return a {@link Runnable} object composed with the given left and right {@link Runnable} objects.
   * @see java.lang.Runnable
   */
  public Runnable compose(Runnable runnableOne, Runnable runnableTwo) {

    return runnableOne == null ? runnableTwo : (runnableTwo == null ? runnableOne
      : new ComposableRunnable(runnableOne, runnableTwo));
  }

  /**
   * Returns a reference to the first {@link Runnable} in this composition.
   *
   * @return a reference to the first {@link Runnable} in this composition.
   * @see java.lang.Runnable
   */
  protected Runnable getRunnableOne() {
    return this.runnableOne;
  }

  /**
   * Returns a reference to the second {@link Runnable} in this composition.
   *
   * @return a reference to the second {@link Runnable} in this composition.
   * @see java.lang.Runnable
   */
  protected Runnable getRunnableTwo() {
    return this.runnableTwo;
  }

  /**
   * Runs the left and right {@link Runnable} objects/nodes in the graph.
   *
   * @see java.lang.Runnable#run()
   */
  @Override
  public void run() {
    getRunnableOne().run();
    getRunnableTwo().run();
  }
}
