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

package org.cp.elements.process;

import static org.assertj.core.api.Assertions.assertThat;
import static org.cp.elements.process.ProcessAdapter.newProcessAdapter;
import static org.cp.elements.process.ProcessContext.newProcessContext;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;

import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.lang.SystemUtils;
import org.cp.elements.util.Environment;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

/**
 * Unit tests for {@link ProcessAdapter}.
 *
 * @author John Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.cp.elements.process.ProcessAdapter
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class ProcessAdapterTests {

  @Rule
  public ExpectedException exception = ExpectedException.none();

  @Mock
  private Process mockProcess;

  private ProcessContext processContext;

  @Before
  public void setup() {
    processContext = newProcessContext(mockProcess);
  }

  @Test
  public void newProcessAdapterWithProcess() {
    ProcessAdapter processAdapter = newProcessAdapter(mockProcess);

    assertThat(processAdapter).isNotNull();
    assertThat(processAdapter.getProcess()).isSameAs(mockProcess);

    ProcessContext processContext = processAdapter.getProcessContext();

    assertThat(processContext).isInstanceOf(ProcessContext.class);
    assertThat(processContext).isNotSameAs(this.processContext);
    assertThat(processContext.getCommandLine()).isEmpty();
    assertThat(processContext.getDirectory()).isEqualTo(FileSystemUtils.WORKING_DIRECTORY);
    assertThat(processContext.getEnvironment()).isEqualTo(Environment.fromEnvironmentVariables());
    assertThat(processContext.getProcess()).isSameAs(mockProcess);
    assertThat(processContext.getUsername()).isEqualTo(SystemUtils.USERNAME);
    assertThat(processContext.isRedirectingErrorStream()).isFalse();
  }

  @Test
  public void newProcessAdapterWithProcessAndProcessContext() {
    ProcessAdapter processAdapter = newProcessAdapter(mockProcess, processContext);

    assertThat(processAdapter).isNotNull();
    assertThat(processAdapter.getProcess()).isSameAs(mockProcess);
    assertThat(processAdapter.getProcessContext()).isSameAs(processContext);
  }

  @Test
  public void newProcessAdapterWithNullProcess() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Process cannot be null");

    newProcessAdapter(null, processContext);
  }

  @Test
  public void newProcessAdapterWithNullProcessContext() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("ProcessContext cannot be null");

    newProcessAdapter(mockProcess, null);
  }
}
