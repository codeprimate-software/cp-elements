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
package org.cp.elements.lang.support;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link DefaultFilter}.
 *
 * @author John J. Blum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.lang.support.DefaultFilter
 * @since 1.0.0
 */
public class DefaultFilterTests {

  @Test
  public void getInstanceWithTrueReturnsAccept() {
    assertThat(DefaultFilter.getInstance(true)).isSameAs(DefaultFilter.ACCEPT);
  }

  @Test
  public void getInstanceWithFalseReturnsReject() {
    assertThat(DefaultFilter.getInstance(false)).isSameAs(DefaultFilter.REJECT);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void accept() {

    DefaultFilter<Object> defaultFilter = DefaultFilter.ACCEPT;

    assertThat(defaultFilter.isAccepting()).isTrue();
    assertThat(defaultFilter.accept(null)).isTrue();
    assertThat(defaultFilter.accept(Boolean.FALSE)).isTrue();
    assertThat(defaultFilter.accept('\0')).isTrue();
    assertThat(defaultFilter.accept(0)).isTrue();
    assertThat(defaultFilter.accept(-0.0d)).isTrue();
    assertThat(defaultFilter.accept("")).isTrue();
    assertThat(defaultFilter.accept("  ")).isTrue();
    assertThat(defaultFilter.accept("test")).isTrue();
    assertThat(defaultFilter.accept(new Object())).isTrue();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void reject() {

    DefaultFilter<Object> defaultFilter = DefaultFilter.REJECT;

    assertThat(defaultFilter.isAccepting()).isFalse();
    assertThat(defaultFilter.accept(null)).isFalse();
    assertThat(defaultFilter.accept(Boolean.TRUE)).isFalse();
    assertThat(defaultFilter.accept('a')).isFalse();
    assertThat(defaultFilter.accept(1)).isFalse();
    assertThat(defaultFilter.accept(Math.PI)).isFalse();
    assertThat(defaultFilter.accept("")).isFalse();
    assertThat(defaultFilter.accept("  ")).isFalse();
    assertThat(defaultFilter.accept("test")).isFalse();
    assertThat(defaultFilter.accept(new Object())).isFalse();
  }
}
