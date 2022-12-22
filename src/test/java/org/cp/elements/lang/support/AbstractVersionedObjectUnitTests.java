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
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.assertj.core.api.Assertions.assertThatIllegalStateException;

import org.junit.Test;

/**
 * Unit Tests for {@link AbstractVersionedObject}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.cp.elements.lang.support.AbstractVersionedObject
 * @since 1.0.0
 */
public class AbstractVersionedObjectUnitTests {

  @Test
  public void setAndGetVersion() {

    AbstractVersionedObject<TestVersionedObject, Integer> versionedObject = new TestVersionedObject();

    versionedObject.setVersion(1);

    assertThat(versionedObject.getVersion()).isOne();
  }

  @Test
  public void getVersionWhenUnset() {

    AbstractVersionedObject<TestVersionedObject, Integer> versionedObject = new TestVersionedObject();

    assertThatIllegalStateException()
      .isThrownBy(versionedObject::getVersion)
      .withMessage("Version [null] was not initialized")
      .withNoCause();
  }

  @Test
  public void serVersionToNull() {

    AbstractVersionedObject<TestVersionedObject, Integer> versionedObject = new TestVersionedObject();

    assertThatIllegalArgumentException()
      .isThrownBy(() -> versionedObject.setVersion(null))
      .withMessage("Version is required")
      .withNoCause();
  }

  @Test
  public void atVersionReturnsVersionedObject() {

    AbstractVersionedObject<TestVersionedObject, Integer> versionedObject = new TestVersionedObject();

    assertThat(versionedObject.atVersion(1)).isSameAs(versionedObject);
    assertThat(versionedObject.getVersion()).isOne();
  }

  @Test
  public void getCurrentVersionIsEqualToPreviousVersion() {

    AbstractVersionedObject<TestVersionedObject, Integer> versionedObject = new TestVersionedObject();

    assertThat(versionedObject.atVersion(1)).isSameAs(versionedObject);
    assertThat(versionedObject.getVersion()).isOne();
    assertThat(versionedObject.getCurrentVersion()).isNull();
    assertThat(versionedObject.atVersion(2)).isSameAs(versionedObject);
    assertThat(versionedObject.getCurrentVersion()).isOne();
    assertThat(versionedObject.atVersion(3)).isSameAs(versionedObject);
    assertThat(versionedObject.getCurrentVersion()).isEqualTo(2);
  }

  static final class TestVersionedObject extends AbstractVersionedObject<TestVersionedObject, Integer> { }

}
