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
package org.cp.elements.beans.model;

import static org.assertj.core.api.Assertions.assertThat;

import org.cp.elements.security.model.User;
import org.junit.jupiter.api.Test;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.ToString;

/**
 * Integration Tests for {@link BeanAdapter}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.beans.model.BeanAdapter
 * @since 1.0.0
 */
public class BeanAdapterIntegrationTests {

  @Test
  public void getUserNameIsCorrect() {

    User<Integer> jonDoe = TestUser.as("jonDoe");

    BeanAdapter jonDoeBean = BeanAdapter.from(jonDoe);

    assertThat(jonDoeBean).isNotNull();
    assertThat(jonDoeBean.getTarget()).isSameAs(jonDoe);
    assertThat(jonDoe.getName()).isEqualTo("jonDoe");
    assertThat(jonDoeBean.<String>getPropertyValue("name")).isEqualTo(jonDoe.getName());
  }

  @Test
  public void setUseIdIsCorrect() {

    User<Integer> janeDoe = TestUser.as("janeDoe");

    BeanAdapter janeDoeBean = BeanAdapter.from(janeDoe);

    assertThat(janeDoeBean).isNotNull();
    assertThat(janeDoeBean.getTarget()).isSameAs(janeDoe);
    assertThat(janeDoe.getId()).isNull();

    janeDoeBean.setPropertyValue("id", 2);

    assertThat(janeDoe.getId()).isEqualTo(2);
  }

  @Getter
  @ToString(of = "name")
  @EqualsAndHashCode(of = "name")
  @RequiredArgsConstructor(staticName = "as")
  private static final class TestUser implements User<Integer> {

    @Setter
    private Integer id;

    @lombok.NonNull
    private final String name;

  }
}
