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
package org.cp.elements.lang.concurrent.lock;

import static org.assertj.core.api.Assertions.assertThat;
import static org.cp.elements.lang.ThrowableAssertions.assertThatThrowableOfType;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verifyNoInteractions;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.function.Supplier;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.annotation.NotNull;

import edu.umd.cs.mtc.MultithreadedTestCase;
import edu.umd.cs.mtc.TestFramework;

/**
 * Integration Tests for {@link LockUtils}.
 *
 * @author John Blum
 * @see java.util.concurrent.locks.Lock
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.concurrent.lock.LockUtils
 * @see edu.umd.cs.mtc.MultithreadedTestCase
 * @see edu.umd.cs.mtc.TestFramework
 * @since 2.0.0
 */
public class LockUtilsIntegrationTests {

  @Test
  void doRunnableWithThreadLockInterrupted() throws Throwable {
    TestFramework.runOnce(new LockingRunnableMultithreadedTestCase());
  }

  @Test
  void getFromSupplierWithThreadLockInterrupted() throws Throwable {
    TestFramework.runOnce(new LockingSupplierMultithreadedTestCase());
  }

  private static abstract class AbstractLockingMultithreadedTestCase extends MultithreadedTestCase {

    private final Lock lock = new ReentrantLock(false);

    private volatile Thread lockingThread;

    protected @NotNull Lock getLock() {
      return this.lock;
    }

    public void thread1() {

      assertTick(0);
      Thread.currentThread().setName("Locking Thread");

      try {
        this.lock.lock();
        waitForTick(2);
        this.lockingThread.interrupt();
      }
      finally {
        this.lock.unlock();
      }
    }

    public void thread2() {

      assertTick(0);
      this.lockingThread = Thread.currentThread();
      this.lockingThread.setName("Interrupted Thread");

      waitForTick(1);

      assertThat(this.lockingThread.isInterrupted()).isFalse();

      testWithLock();

      assertThat(this.lockingThread.isInterrupted()).isTrue();
    }

    protected abstract void testWithLock();

  }

  @SuppressWarnings("unused")
  private static class LockingRunnableMultithreadedTestCase extends AbstractLockingMultithreadedTestCase {

    @Override
    protected void testWithLock() {

      Runnable mockRunnable = mock(Runnable.class);

      LockUtils.usingInterruptableLock().doWithLock(getLock(), mockRunnable);

      verifyNoInteractions(mockRunnable);
    }
  }

  @SuppressWarnings("unused")
  private static class LockingSupplierMultithreadedTestCase extends AbstractLockingMultithreadedTestCase {

    @Override
    protected void testWithLock() {

      Supplier<?> mockSupplier = mock(Supplier.class);

      assertThatThrowableOfType(IllegalMonitorStateException.class)
        .isThrownBy(args -> LockUtils.usingInterruptableLock().doWithLock(getLock(), mockSupplier))
        .havingMessage("Thread was interrupted while trying to obtain lock [%s]", getLock())
        .causedBy(InterruptedException.class)
        .withNoCause();

      verifyNoInteractions(mockSupplier);
    }
  }
}
