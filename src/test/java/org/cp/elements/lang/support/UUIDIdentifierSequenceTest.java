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

import java.util.HashSet;
import java.util.Set;

import org.junit.Test;

/**
 * Unit Tests for {@link UUIDIdentifierSequence}.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.cp.elements.lang.IdentifierSequence
 * @see org.cp.elements.lang.support.UUIDIdentifierSequence
 * @since 1.0.0
 */
public class UUIDIdentifierSequenceTest {

  private static final int COUNT = 100000;

  @Test
  public void nextIdGeneratedUniqueIds() {

    UUIDIdentifierSequence identifierSequence = new UUIDIdentifierSequence();

    Set<String> identifiers = new HashSet<>(COUNT);

    for (int index = COUNT; index > 0; --index) {
      identifiers.add(identifierSequence.nextId());
    }

    assertThat(identifiers.size()).isEqualTo(COUNT);
  }
}
