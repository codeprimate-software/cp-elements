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
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.concurrent.locks.Lock;
import java.util.function.Supplier;

import org.junit.jupiter.api.Test;

import org.mockito.InOrder;

/**
 * Unit Tests for {@link Locking}.
 *
 * @author John Blum
 * @see java.util.concurrent.locks.Lock
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see Locking
 * @since 2.0.0
 */
public class LockingUnitTests {

  @Test
  public void usingBlockingLockingStategyIsCorrect() throws InterruptedException {

    Lock mockLock = mock(Lock.class);

    Locking locking = Locking.usingBlockingLock();

    assertThat(locking).isNotNull();
    assertThat(locking).isSameAs(Locking.usingBlockingLock());

    locking.lockAcquiringStrategy().acquire(mockLock);

    verify(mockLock, times(1)).lock();
    verifyNoMoreInteractions(mockLock);
  }

  @Test
  public void usingInterruptableLockingStrategyIsCorrect() throws InterruptedException {

    Lock mockLock = mock(Lock.class);

    Locking locking = Locking.usingInterruptableLock();

    assertThat(locking).isNotNull();
    assertThat(locking).isSameAs(Locking.usingInterruptableLock());

    locking.lockAcquiringStrategy().acquire(mockLock);

    verify(mockLock, times(1)).lockInterruptibly();
    verifyNoMoreInteractions(mockLock);
  }

  @Test
  public void doRunnableWithLock() throws InterruptedException {

    Lock mockLock = mock(Lock.class);

    Runnable mockRunnable = mock(Runnable.class);

    Locking.usingInterruptableLock().doWithLock(mockLock, mockRunnable);

    InOrder order = inOrder(mockLock, mockRunnable);

    order.verify(mockLock, times(1)).lockInterruptibly();
    order.verify(mockRunnable, times(1)).run();
    order.verify(mockLock, times(1)).unlock();

    verifyNoMoreInteractions(mockLock, mockRunnable);
  }

  @Test
  public void doNullRunnableWithLock() {

    Lock mockLock = mock(Lock.class);

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Locking.usingBlockingLock().doWithLock(mockLock, (Runnable) null))
      .withMessage("Code to run with Lock is required")
      .withNoCause();

    verifyNoInteractions(mockLock);
  }

  @Test
  public void doRunnableWithNullLock() {

    Runnable mockRunnable = mock(Runnable.class);

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Locking.usingInterruptableLock().doWithLock(null, mockRunnable))
      .withMessage("Lock is required")
      .withNoCause();

    verifyNoInteractions(mockRunnable);
  }

  @Test
  public void doSupplierWithLock() throws InterruptedException {

    Lock mockLock = mock(Lock.class);

    Supplier<?> mockSupplier = mock(Supplier.class);

    doReturn("TEST").when(mockSupplier).get();

    assertThat(Locking.usingInterruptableLock().doWithLock(mockLock, mockSupplier)).isEqualTo("TEST");

    InOrder order = inOrder(mockLock, mockSupplier);

    order.verify(mockLock, times(1)).lockInterruptibly();
    order.verify(mockSupplier, times(1)).get();
    order.verify(mockLock, times(1)).unlock();

    verifyNoMoreInteractions(mockLock, mockSupplier);
  }

  @Test
  public void doNullSupplierWithLock() {

    Lock mockLock = mock(Lock.class);

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Locking.usingBlockingLock().doWithLock(mockLock, (Supplier<?>) null))
      .withMessage("Code to run with Lock is required")
      .withNoCause();

    verifyNoInteractions(mockLock);
  }

  @Test
  public void doSupplierWithNullLock() {

    Supplier<?> mockSupplier = mock(Supplier.class);

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Locking.usingInterruptableLock().doWithLock(null, mockSupplier))
      .withMessage("Lock is required")
      .withNoCause();

    verifyNoInteractions(mockSupplier);
  }
}
