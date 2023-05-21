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

import java.util.ArrayList;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import org.cp.elements.beans.model.support.ArrayProperty;
import org.cp.elements.beans.model.support.ListProperty;
import org.cp.elements.beans.model.support.MapProperty;
import org.cp.elements.beans.model.support.SortedSetProperty;
import org.cp.elements.security.model.User;

import lombok.Getter;
import lombok.Setter;

/**
 * Integration Tests for {@link Properties}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.beans.model.Properties
 * @since 1.0.0
 */
public class PropertiesIntegrationTests {

  private static BeanAdapter bean;

  @BeforeAll
  public static void setup() {
    bean = BeanAdapter.from(new TypeWithProperties());
  }

  @Test
  public void constructArrayProperty() {

    Property array = bean.getModel().getProperty("array");

    assertThat(array).isNotNull();
    assertThat(array.getName()).isEqualTo("array");
    assertThat(array.getType()).isEqualTo(User[].class);
    assertThat(array).isInstanceOf(ArrayProperty.class);
  }

  @Test
  public void constructListProperty () {

    Property list = bean.getModel().getProperty("list");

    assertThat(list).isNotNull();
    assertThat(list.getName()).isEqualTo("list");
    assertThat(list.getType()).isEqualTo(ArrayList.class);
    assertThat(list).isInstanceOf(ListProperty.class);
  }

  @Test
  public void constructMapProperty() {

    Property map = bean.getModel().getProperty("map");

    assertThat(map).isNotNull();
    assertThat(map.getName()).isEqualTo("map");
    assertThat(map.getType()).isEqualTo(ConcurrentMap.class);
    assertThat(map).isInstanceOf(MapProperty.class);
  }

  @Test
  public void constructSetProperty() {

    Property set = bean.getModel().getProperty("set");

    assertThat(set).isNotNull();
    assertThat(set.getName()).isEqualTo("set");
    assertThat(set.getType()).isEqualTo(SortedSet.class);
    assertThat(set).isInstanceOf(SortedSetProperty.class);
  }

  @Test
  public void constructStandardProperty() {

    Property user = bean.getModel().getProperty("user");

    assertThat(user).isNotNull();
    assertThat(user.getName()).isEqualTo("user");
    assertThat(user.getType()).isEqualTo(User.class);
    assertThat(user.getClass()).isEqualTo(Property.class);
  }

  static class TypeWithProperties {

    @Getter @Setter
    private User<?>[] array;

    @Getter @Setter
    private ArrayList<User<?>> list;

    @Getter
    private final ConcurrentMap<?, User<?>> map = new ConcurrentHashMap<>();

    @Getter
    private final SortedSet<User<Integer>> set = new TreeSet<>();

    @Getter @Setter
    private User<?> user;

  }
}
