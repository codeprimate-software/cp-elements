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

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.Test;

/**
 * Unit tests for {@link Destroyable}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.lang.Destroyable
 * @since 1.0.0
 */
public class DestroyableTests {

  @Test
  public void noArgumentDestroyCallsDestroyWithNoopRunnable() {

    Destroyable mockDestroyable = mock(Destroyable.class);

    doCallRealMethod().when(mockDestroyable).destroy();

    mockDestroyable.destroy();

    verify(mockDestroyable, times(1)).destroy(eq(RunnableUtils.NOOP_RUNNABLE));
  }
}
