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

package org.cp.elements.lang;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit tests for {@link Executable}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.Executable
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class ExecutableTests {

  @Mock
  private Executable mockExecutable;

  @Test
  public void callInvokesExecuteWithNoArgumentsAndReturnsValue() throws Exception {

    when(this.mockExecutable.call()).thenCallRealMethod();
    when(this.mockExecutable.execute(any())).thenReturn("test");

    assertThat(this.mockExecutable.call()).isEqualTo("test");

    verify(this.mockExecutable, times(1)).call();
    verify(this.mockExecutable, times(1)).execute();
  }

  @Test
  public void runInvokesExecuteWithNoArgumentsReturningNoValue() {

    doCallRealMethod().when(this.mockExecutable).run();

    this.mockExecutable.run();

    verify(this.mockExecutable, times(1)).run();
    verify(this.mockExecutable, times(1)).execute();
  }

  @Test(expected = IllegalStateException.class)
  public void isRunningThrowsIllegalStateException() {

    try {
      when(this.mockExecutable.isRunning()).thenCallRealMethod();

      this.mockExecutable.isRunning();
    }
    catch (IllegalStateException expected) {

      assertThat(expected).hasMessage("The runnable state of this object cannot be determined");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }
}
