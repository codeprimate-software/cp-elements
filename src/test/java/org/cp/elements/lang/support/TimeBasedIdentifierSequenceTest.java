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

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Set;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.TimeUnit;

import org.junit.Test;

import edu.umd.cs.mtc.MultithreadedTestCase;
import edu.umd.cs.mtc.TestFramework;

/**
 * The TimeBasedIdentifierSequenceTest class is a test suite of test cases testing the contract and functionality
 * of the TimeBasedIdentifierSequence class.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.cp.elements.lang.IdentifierSequence
 * @see org.cp.elements.lang.support.TimeBasedIdentifierSequence
 * @see edu.umd.cs.mtc.MultithreadedTestCase
 * @see edu.umd.cs.mtc.TestFramework
 * @since 1.0.0
 */
public class TimeBasedIdentifierSequenceTest {

  private static final int COUNT = 100000;

  @Test
  public void nextIdIsUnique() {
    long previousId = TimeBasedIdentifierSequence.nextGlobalId();

    assertThat(previousId).isGreaterThan(0);

    TimeBasedIdentifierSequence.setGlobalId(previousId + TimeUnit.MILLISECONDS.toNanos(500));

    long nextId = TimeBasedIdentifierSequence.nextGlobalId();

    assertThat(nextId).isGreaterThan(previousId);
  }

  @Test
  public void nextIdGeneratesUniqueIdentifiers() {
    TimeBasedIdentifierSequence identifierSequence = new TimeBasedIdentifierSequence();

    long previousId = -1;

    for (int index = COUNT; index > 0; --index) {
      long newId = identifierSequence.nextId();
      assertThat(newId).isGreaterThan(previousId);
      previousId = newId;
    }
  }

  @Test
  public void multipleInstancesOfTimeBasedIdentifierSequenceGenerateUniqueIdentifiers() {
    TimeBasedIdentifierSequence identifierSequenceOne = new TimeBasedIdentifierSequence();
    TimeBasedIdentifierSequence identifierSequenceTwo = new TimeBasedIdentifierSequence();

    long previousId = -1;

    for (int index = COUNT; index > 0; --index) {
      long newId = (index % 2 == 0 ? identifierSequenceOne.nextId() : identifierSequenceTwo.nextId());
      assertThat(newId).isGreaterThan(previousId);
      previousId = newId;
    }
  }

  @Test
  public void timeBasedIdentifierSequenceIsThreadSafe() throws Throwable {
    TestFramework.runOnce(new SimpleIdentifierSequenceThreadSafetyTest());
  }

  @SuppressWarnings("unused")
  protected static final class SimpleIdentifierSequenceThreadSafetyTest extends MultithreadedTestCase {

    private final Set<Long> identifiersOne = new ConcurrentSkipListSet<>();
    private final Set<Long> identifiersTwo = new ConcurrentSkipListSet<>();

    private final TimeBasedIdentifierSequence identifierSequence = new TimeBasedIdentifierSequence();

    public void thread1() {
      assertTick(0);

      Thread.currentThread().setName("Time-based Identifier Sequence Thread 1");

      for (int index = COUNT; index > 0; --index) {
        identifiersOne.add(identifierSequence.nextId());
      }
    }

    public void thread2() {
      assertTick(0);

      Thread.currentThread().setName("Time-based Identifier Sequence Thread 2");

      for (int index = COUNT; index > 0; --index) {
        identifiersTwo.add(identifierSequence.nextId());
      }
    }

    @Override
    public void finish() {
      assertThat(identifiersOne.size()).isEqualTo(COUNT);
      assertThat(identifiersTwo.size()).isEqualTo(COUNT);
      assertThat(identifiersOne.removeAll(identifiersTwo)).isFalse();
      assertThat(identifiersOne.size()).isEqualTo(COUNT);
    }
  }
}
