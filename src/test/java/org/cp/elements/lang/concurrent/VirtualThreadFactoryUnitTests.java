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
package org.cp.elements.lang.concurrent;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.Mockito.verifyNoInteractions;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.IntStream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

/**
 * Unit Tests for {@link VirtualThreadFactory}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @since 3.0.0
 */
@ExtendWith(MockitoExtension.class)
public class VirtualThreadFactoryUnitTests {

  @Mock
  private Runnable mockRunnable;

  @Test
  void newThreadFactory() {

    VirtualThreadFactory threadFactory = VirtualThreadFactory.newThreadFactory();

    assertThat(threadFactory).isNotNull();
    assertThat(threadFactory).isNotSameAs(VirtualThreadFactory.newThreadFactory());
  }

  @Test
  void generateThreadId() {

    VirtualThreadFactory threadFactory = VirtualThreadFactory.newThreadFactory();

    Set<String> threadIds = new HashSet<>();

    IntStream.range(0, 100).forEach(index -> threadIds.add(threadFactory.generateThreadId()));

    assertThat(threadIds).hasSize(100);
  }

  @Test
  void generateThreadName() {
    assertThat(VirtualThreadFactory.newThreadFactory().generateThreadName())
      .containsPattern(VirtualThreadFactory.class.getName()+".THREAD-.+");
  }

  @Test
  void newThreadWithRunnable() {

    Thread thread = VirtualThreadFactory.newThreadFactory().newThread(this.mockRunnable);

    assertThat(thread).isNotNull();
    assertThat(thread.getName()).containsPattern(VirtualThreadFactory.class.getName()+".THREAD-.+");
    assertThat(thread.getState()).isEqualTo(Thread.State.NEW);
    assertThat(thread.isVirtual()).isTrue();
  }

  @Test
  void newThreadWithNullRunnable() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> VirtualThreadFactory.newThreadFactory().newThread(null))
      .withMessage("Runnable is required")
      .withNoCause();
  }

  @Test
  void newNamedThreadWithRunnable() {

    Thread thread = VirtualThreadFactory.newThreadFactory().newThread("TEST", this.mockRunnable);

    assertThat(thread).isNotNull();
    assertThat(thread.getName()).isEqualTo("TEST");
    assertThat(thread.getState()).isEqualTo(Thread.State.NEW);
    assertThat(thread.isVirtual()).isTrue();

    verifyNoInteractions(this.mockRunnable);
  }

  @Test
  void newNamedThreadWithInvalidName() {

    Arrays.asList("  ", "", null).forEach(name ->
      assertThatIllegalArgumentException()
        .isThrownBy(() -> VirtualThreadFactory.newThreadFactory().newThread(name, this.mockRunnable))
        .withMessage("Name [%s] is required", name)
        .withNoCause());

    verifyNoInteractions(this.mockRunnable);
  }

  @Test
  void newNamedThreadWithNullRunnable() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> VirtualThreadFactory.newThreadFactory().newThread("MOCK", null))
      .withMessage("Runnable is required")
      .withNoCause();
  }
}
