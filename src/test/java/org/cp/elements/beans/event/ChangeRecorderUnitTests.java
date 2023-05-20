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
package org.cp.elements.beans.event;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;

import java.beans.PropertyChangeEvent;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.annotation.NotNull;

/**
 * Unit Tests for {@link ChangeRecorder}.
 *
 * @author John J. Blum
 * @see java.beans.PropertyChangeEvent
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.beans.event.ChangeRecorder
 * @since 1.0.0
 */
public class ChangeRecorderUnitTests {

  private static final Object SOURCE = new Object();

  private @NotNull PropertyChangeEvent newPropertyChangeEvent(String propertyName, Object oldValue, Object newValue) {
    return new PropertyChangeEvent(SOURCE, propertyName, oldValue, newValue);
  }

  @Test
  public void propertyChangeIsRecordedCorrectly() {

    ChangeRecorder changeRecorder = new ChangeRecorder();

    assertThat(changeRecorder).isNotNull();
    assertThat(changeRecorder.isModified()).isFalse();

    changeRecorder.propertyChange(newPropertyChangeEvent("myProperty", null, "test"));

    assertThat(changeRecorder.isModified()).isTrue();
    assertThat(changeRecorder.isModified("myProperty")).isTrue();
    assertThat(changeRecorder.isModified("yourProperty")).isFalse();

    changeRecorder.propertyChange(newPropertyChangeEvent("myProperty", "test", "testing"));

    assertThat(changeRecorder.isModified()).isTrue();
    assertThat(changeRecorder.isModified("myProperty")).isTrue();
    assertThat(changeRecorder.isModified("yourProperty")).isFalse();

    changeRecorder.propertyChange(newPropertyChangeEvent("myProperty", "testing", "tested"));

    assertThat(changeRecorder.isModified()).isTrue();
    assertThat(changeRecorder.isModified("myProperty")).isTrue();
    assertThat(changeRecorder.isModified("yourProperty")).isFalse();

    changeRecorder.propertyChange(newPropertyChangeEvent("myProperty", "tested", "testing"));

    assertThat(changeRecorder.isModified()).isTrue();
    assertThat(changeRecorder.isModified("myProperty")).isTrue();
    assertThat(changeRecorder.isModified("yourProperty")).isFalse();

    changeRecorder.propertyChange(newPropertyChangeEvent("myProperty", "tested", null));

    assertThat(changeRecorder.isModified()).isFalse();
    assertThat(changeRecorder.isModified("myProperty")).isFalse();
    assertThat(changeRecorder.isModified("yourProperty")).isFalse();

    changeRecorder.propertyChange(newPropertyChangeEvent("yourProperty", null, null));

    assertThat(changeRecorder.isModified()).isFalse();
    assertThat(changeRecorder.isModified("myProperty")).isFalse();
    assertThat(changeRecorder.isModified("yourProperty")).isFalse();
  }

  @Test
  public void iterateBeanProperties()  {

    ChangeRecorder changeRecorder = new ChangeRecorder();

    assertThat(changeRecorder).isNotNull();
    assertThat(changeRecorder.isModified()).isFalse();

    changeRecorder.propertyChange(newPropertyChangeEvent("propertyOne", null, "test"));
    changeRecorder.propertyChange(newPropertyChangeEvent("propertyTwo", null, null));
    changeRecorder.propertyChange(newPropertyChangeEvent("propertyThree", "null", null));
    changeRecorder.propertyChange(newPropertyChangeEvent("propertyFour", "null", "null"));
    changeRecorder.propertyChange(newPropertyChangeEvent("propertyFive", "null", "nil"));

    assertThat(changeRecorder.isModified()).isTrue();
    assertThat(changeRecorder.isModified("propertyOne")).isTrue();
    assertThat(changeRecorder.isModified("propertyTwo")).isFalse();
    assertThat(changeRecorder.isModified("propertyThree")).isTrue();
    assertThat(changeRecorder.isModified("propertyFour")).isFalse();
    assertThat(changeRecorder.isModified("propertyFive")).isTrue();

    Set<String> actualProperties = new HashSet<>(3);

    for (String property : changeRecorder) {
      actualProperties.add(property);
    }

    assertThat(actualProperties).hasSize(3);
    assertThat(actualProperties).containsExactlyInAnyOrder("propertyOne", "propertyThree", "propertyFive");
    assertThat(actualProperties).doesNotContain("propertyTwo", "propertyFour");
  }

  @Test
  public void iteratorIsUnmodifiable() {

    ChangeRecorder changeRecorder = new ChangeRecorder();

    assertThat(changeRecorder).isNotNull();
    assertThat(changeRecorder.isModified()).isFalse();

    changeRecorder.propertyChange(newPropertyChangeEvent("propertyOne", null, "test"));
    changeRecorder.propertyChange(newPropertyChangeEvent("propertyTwo", null, null));

    assertThat(changeRecorder.isModified()).isTrue();
    assertThat(changeRecorder.isModified("propertyOne")).isTrue();
    assertThat(changeRecorder.isModified("propertyTwo")).isFalse();

    Iterator<String> it = changeRecorder.iterator();

    assertThat(it).isNotNull();
    assertThat(it.hasNext()).isTrue();

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(it::remove)
      .withNoCause();

    assertThat(it.next()).isEqualTo("propertyOne");
    assertThat(it.hasNext()).isFalse();
  }

  @Test
  public void clearIsSuccessful() {

    ChangeRecorder changeRecorder = new ChangeRecorder();

    assertThat(changeRecorder).isNotNull();
    assertThat(changeRecorder.isModified()).isFalse();

    changeRecorder.propertyChange(newPropertyChangeEvent("testProperty", 1, 2));

    assertThat(changeRecorder.isModified()).isTrue();
    assertThat(changeRecorder.clear()).isTrue();
    assertThat(changeRecorder.isModified()).isFalse();
  }
}
