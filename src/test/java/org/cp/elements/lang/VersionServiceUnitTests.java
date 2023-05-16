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

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.support.AbstractVersionedObject;

/**
 * Unit Tests for {@link VersionService}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.Versioned
 * @see org.cp.elements.lang.VersionService
 * @see org.cp.elements.lang.support.AbstractVersionedObject
 * @since 1.0.0
 */
@SuppressWarnings("unchecked")
public class VersionServiceUnitTests {

  @Test
  public void setVersionOnAbstractVersionedObject() {

    AbstractVersionedObject<?, Integer> versionedObject = mock(AbstractVersionedObject.class);

    VersionService<Integer> versionService = mock(VersionService.class);

    doReturn(1).when(versionService).newVersion();
    doCallRealMethod().when(versionService).setVersion(any());

    assertThat(versionService.setVersion(versionedObject)).isSameAs(versionedObject);

    verify(versionService, times(1)).setVersion(eq(versionedObject));
    verify(versionService, times(1)).newVersion();
    verify(versionedObject, times(1)).atVersion(eq(1));
    verifyNoMoreInteractions(versionedObject, versionService);
  }

  @Test
  public void setVersionOnVersionedObject() {

    Versioned<Integer> versionedObject = mock(Versioned.class);

    VersionService<Integer> versionService = mock(VersionService.class);

    doReturn(1).when(versionService).newVersion();
    doCallRealMethod().when(versionService).setVersion(any());

    assertThat(versionService.setVersion(versionedObject)).isSameAs(versionedObject);

    verify(versionService, times(1)).setVersion(eq(versionedObject));
    verifyNoMoreInteractions(versionService);
    verifyNoInteractions(versionedObject);
  }

  @Test
  public void setVersionWithNullIsNullSafe() {

    VersionService<Integer> versionService = mock(VersionService.class);

    doReturn(1).when(versionService).newVersion();
    doCallRealMethod().when(versionService).setVersion(any());

    assertThat((Versioned<?>) versionService.setVersion(null)).isNull();

    verify(versionService, times(1)).setVersion(isNull());
    verifyNoMoreInteractions(versionService);
  }
}
