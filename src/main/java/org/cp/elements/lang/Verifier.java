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

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Interface defining a contract for implementing {@link Object objects}
 * capable of verifying {@link Verifiable} objects.
 *
 * @author John Blum
 * @see org.cp.elements.lang.Verifiable
 * @since 1.0.0
 */
@FunctionalInterface
public interface Verifier {

  /**
   * Verifies the {@link Verifiable} object for validity.
   *
   * @param verifiable {@link Verifiable} object to verify.
   * @see org.cp.elements.lang.Verifiable
   */
  void verify(Verifiable<?> verifiable);

  /**
   * Builder method used to compose {@link Verifier Verifiers} into a single, composite {@link Verifier}.
   *
   * @param verifier {@link Verifier} to compose with this {@link Verifier}.
   * @return a new composite {@link Verifier} composed of this {@link Verifier} followed by the given {@link Verifier},
   * or simply return this {@link Verifier} if the given {@link Verifier} is {@literal null}.
   */
  default @NotNull Verifier andThen(@Nullable Verifier verifier) {

    return verifier == null ? this : (Verifiable<?> verifiable) -> {
      this.verify(verifiable);
      verifier.verify(verifiable);
    };
  }
}
