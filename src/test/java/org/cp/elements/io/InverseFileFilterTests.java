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
package org.cp.elements.io;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.FileFilter;

import org.cp.elements.test.TestUtils;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit Tests for {@link InverseFileFilter}.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.cp.elements.io.InverseFileFilter
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class InverseFileFilterTests {

  @Mock
  private File mockFile;

  @Mock
  private FileFilter mockFileFilter;

  @Test
  public void constructInverseFileFilter() {

    InverseFileFilter inverseFileFilter = new InverseFileFilter(this.mockFileFilter);

    assertThat(inverseFileFilter.getDelegate()).isSameAs(this.mockFileFilter);
  }

  @Test(expected = IllegalArgumentException.class)
  public void constructInverseFileFilterWithNullDelegate() {
    TestUtils.doIllegalArgumentExceptionThrowingOperation(() -> new InverseFileFilter(null),
      () -> "FileFilter must not be null");
  }

  @Test
  public void accept() {

    InverseFileFilter inverseFileFilter = new InverseFileFilter(this.mockFileFilter);

    when(this.mockFileFilter.accept(any(File.class))).thenReturn(false);

    assertThat(inverseFileFilter.accept(this.mockFile)).isTrue();

    verify(this.mockFileFilter, times(1)).accept(eq(this.mockFile));
    verifyNoMoreInteractions(this.mockFileFilter);
    verifyNoInteractions(this.mockFile);
  }

  @Test
  public void reject() {

    InverseFileFilter inverseFileFilter = new InverseFileFilter(this.mockFileFilter);

    when(this.mockFileFilter.accept(any(File.class))).thenReturn(true);

    assertThat(inverseFileFilter.accept(this.mockFile)).isFalse();

    verify(this.mockFileFilter, times(1)).accept(eq(this.mockFile));
    verifyNoMoreInteractions(this.mockFileFilter);
    verifyNoInteractions(this.mockFile);
  }
}
