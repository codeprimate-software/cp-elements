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

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.IdentifierSequence;

/**
 * Unit Tests for {@link BalancedIdentifierSequence}.
 *
 * @author John Blum
 * @see BalancedIdentifierSequence
 * @see org.junit.jupiter.api.Test
 * @since 3.0.0
 */
class BalancedIdentifierSequenceUnitTests {

  private static final int COUNT = 1_000;

  @Test
  void identifiersAreBalanced() {

    int greaterThanEqualToIdentifierCount = 0;
    int lessThanIdentifierCount = 0;

    IdentifierSequence<Long> identifierSequence = new BalancedIdentifierSequence();

    for (int count = 0; count < COUNT; count++) {
      long id = identifierSequence.nextId();
      if (id < BalancedIdentifierSequence.STARTING_VALUE) {
        lessThanIdentifierCount++;
      }
      else {
        greaterThanEqualToIdentifierCount++;
      }
    }

    assertThat(greaterThanEqualToIdentifierCount).isEqualTo(lessThanIdentifierCount);
    assertThat(greaterThanEqualToIdentifierCount + lessThanIdentifierCount).isEqualTo(COUNT);
  }
}
