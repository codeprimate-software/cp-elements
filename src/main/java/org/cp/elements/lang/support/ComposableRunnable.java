/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.lang.support;

import org.cp.elements.lang.Assert;

/**
 * The ComposableRunnable class is an implementation of the Runnable interface and the Composite Design Pattern to
 * compose multiple Runnable objects and treat it as a single instance of Runnable.
 * <p/>
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
   * <p/>
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
   * <p/>
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
   * <p/>
   * @param runnables an array of Runnable objects to compose into a single, compound Runnable object.
   * @return a Runnable object composed with the Runnable objects contained in the array.
   * @see #compose(Runnable, Runnable)
   */
  public static Runnable compose(final Runnable... runnables) {
    Runnable currentRunnable = null;

    for (Runnable runnable : runnables) {
      currentRunnable = compose(currentRunnable, runnable);
    }

    return currentRunnable;
  }

  /**
   * Runs the left and right Runnable objects/nodes in the graph.
   * <p/>
   * @see java.lang.Runnable#run()
   */
  @Override
  public void run() {
    leftRunnable.run();
    rightRunnable.run();
  }

}
