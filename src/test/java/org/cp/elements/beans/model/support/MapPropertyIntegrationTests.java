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
package org.cp.elements.beans.model.support;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Map;

import org.cp.elements.beans.model.BeanUtils;
import org.cp.elements.beans.model.Property;
import org.cp.elements.util.MapBuilder;
import org.junit.jupiter.api.Test;

import lombok.Getter;

/**
 * Integration Tests for {@link MapProperty}.
 *
 * @author John Blum
 * @see java.beans.PropertyDescriptor
 * @see java.util.Map
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.beans.model.support.MapProperty
 * @since 1.0.0
 */
public class MapPropertyIntegrationTests {

  @Test
  public void getValueAtIndex() {

    Property property = BeanUtils.getProperty(new TypeWithMapProperty(), "map");

    assertThat(property).isInstanceOf(MapProperty.class);
    assertThat(property.getName()).isEqualTo("map");
    assertThat(Map.class).isAssignableFrom(property.getType());

    MapProperty mapProperty = (MapProperty) property;

    assertThat(mapProperty.getValue(1)).isEqualTo("ONE");
    assertThat(mapProperty.getValue("two")).isEqualTo("TWO");
    assertThat(mapProperty.getValue(3)).isNull();
  }

  @Test
  public void setValueAtIndex() {

    TypeWithMapProperty target = new TypeWithMapProperty();

    MapProperty property = BeanUtils.getProperty(target, "map");

    assertThat(property).isNotNull();
    assertThat(property.getName()).isEqualTo("map");
    assertThat(Map.class).isAssignableFrom(property.getType());
    assertThat(target.getMap().get("two")).isEqualTo("TWO");
    assertThat(property.setValue("two", 2)).isEqualTo("TWO");
    assertThat(target.getMap().get("two")).isEqualTo(2);
    assertThat(property.setValue("two", 4)).isEqualTo(2);
    assertThat(target.getMap().get("two")).isEqualTo(4);
  }

  @Test
  public void setValueAtNonExistingIndexPutsNewEntry() {

    TypeWithMapProperty target = new TypeWithMapProperty();

    MapProperty property = BeanUtils.getProperty(target, "map");

    assertThat(target.getMap()).doesNotContainKey("three");
    assertThat(property.setValue("three", 3)).isNull();
    assertThat(target.getMap().get("three")).isEqualTo(3);
  }

  @Getter
  static class TypeWithMapProperty {

    private final Map<Object, Object> map = MapBuilder.newHashMap()
      .put(1, "ONE")
      .put("two", "TWO")
      .build();
  }
}
