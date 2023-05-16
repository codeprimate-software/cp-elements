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
package org.cp.elements.security.model;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link User}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.security.model.User
 * @since 1.0.0
 */
public class UserUnitTests {

  @SuppressWarnings("unchecked")
  private User<Integer> mockUser(Integer id, String name) {

    User<Integer> mockUser = mock(User.class);

    doReturn(id).when(mockUser).getId();
    doReturn(name).when(mockUser).getName();
    doCallRealMethod().when(mockUser).compareTo(isA(User.class));

    return mockUser;
  }

  @Test
  @SuppressWarnings("all")
  public void userCompareToIsCorrect() {

    User<Integer> janeDoe = mockUser(3, "janeDoe");
    User<Integer> jonDoe = mockUser(2, "jonDoe");
    User<Integer> pieDoe = mockUser(1, "pieDoe");

    assertThat(janeDoe.compareTo(jonDoe)).isLessThan(0);
    assertThat(jonDoe.compareTo(jonDoe)).isZero();
    assertThat(pieDoe.compareTo(jonDoe)).isGreaterThan(0);
  }
}
