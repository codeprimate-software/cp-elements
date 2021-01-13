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

package org.cp.elements.test;

/**
 * The {@link Tester} interface defines a contract for an implementing {@link Object} to perform a single test.
 *
 * @author John Blum
 * @since 1.0.0
 */
@FunctionalInterface
public interface Tester {

  /**
   * Performs the test defined by the implementing {@link Object}.
   *
   * @return a boolean value indicating whether the test passed or failed.
   */
  boolean test();

}
