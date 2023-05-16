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
package org.cp.elements.dao.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Optional;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.stream.IntStream;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.Versioned;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

import org.mockito.ArgumentMatchers;

/**
 * Unit Tests for {@link VersionedObjectRepository}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.dao.support.VersionedObjectRepository
 * @see org.cp.elements.lang.Versioned
 * @since 1.0.0
 */
public class VersionedObjectRepositoryUnitTests {

  @SuppressWarnings("unchecked")
  private void testExistsByVersion(@Nullable Integer result,
      @NotNull Consumer<Boolean> repositoryExistsByVersionResultAssertion) {

    String version = UUID.randomUUID().toString();

    Versioned<String> mockVersionedObject = mock(Versioned.class);

    doReturn(version).when(mockVersionedObject).getVersion();

    VersionedObjectRepository<String> versionedObjectRepository = mock(VersionedObjectRepository.class);

    doCallRealMethod().when(versionedObjectRepository).existsByVersion(any(Versioned.class));
    doReturn(Optional.ofNullable(result)).when(versionedObjectRepository).existsByVersion(anyString());

    repositoryExistsByVersionResultAssertion.accept(versionedObjectRepository.existsByVersion(mockVersionedObject));

    verify(mockVersionedObject, times(1)).getVersion();
    verify(versionedObjectRepository, times(1)).existsByVersion(eq(mockVersionedObject));
    verify(versionedObjectRepository, times(1)).existsByVersion(eq(version));
    verifyNoMoreInteractions(mockVersionedObject, versionedObjectRepository);
  }

  @Test
  public void existsByVersionWhenIntegerIsGreaterThanOneReturnsTrue() {
    IntStream.range(2, 100).forEach(value ->
      testExistsByVersion(value, result -> assertThat(result).isTrue()));
  }

  @Test
  public void existsByVersionWhenIntegerIsMinusOneReturnsFalse() {
    testExistsByVersion(-1, result -> assertThat(result).isFalse());
  }

  @Test
  public void existsByVersionWhenIntegerIsOneReturnsTrue() {
    testExistsByVersion(1, result -> assertThat(result).isTrue());
  }

  @Test
  public void existsByVersionWhenIntegerIsZeroReturnsFalse() {
    testExistsByVersion(0, result -> assertThat(result).isFalse());
  }

  @Test
  public void existsByVersionWhenNullReturnsFalse() {
    testExistsByVersion(null, result -> assertThat(result).isFalse());
  }

  @Test
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public void existByVersionWithNullVersionedObject() {

    VersionedObjectRepository<?> versionedObjectRepository = mock(VersionedObjectRepository.class);

    doCallRealMethod().when(versionedObjectRepository).existsByVersion(ArgumentMatchers.<Versioned>any());

    assertThatIllegalArgumentException()
      .isThrownBy(() -> versionedObjectRepository.existsByVersion((Versioned) null))
      .withMessage("Versioned object is required")
      .withNoCause();

    verify(versionedObjectRepository, times(1))
      .existsByVersion(ArgumentMatchers.<Versioned>isNull());

    verifyNoMoreInteractions(versionedObjectRepository);
  }
}
