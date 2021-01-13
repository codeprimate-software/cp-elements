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

import java.time.LocalDateTime;
import java.time.Month;

import org.cp.elements.lang.Constants;
import org.junit.Before;
import org.junit.Test;

/**
 * Unit tests for {@link AuditableSupport}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.cp.elements.lang.support.AuditableSupport
 * @since 1.0.0
 */
public class AuditableSupportTests {

  private AuditableSupport<String, String, Long> auditableSupport;

  @Before
  @SuppressWarnings("unchecked")
  public void setup() {
    this.auditableSupport = new TestAuditableSupport();
  }

  @Test
  public void setAndGetCreatedBy() {

    this.auditableSupport.setCreatedBy("testUser");

    assertThat(this.auditableSupport.getCreatedBy()).isEqualTo("testUser");
  }

  @Test(expected = IllegalArgumentException.class)
  public void setCreatedByToNullThrowsIllegalArgumentException() {

    try {
      this.auditableSupport.setCreatedBy("testUser");
      this.auditableSupport.setCreatedBy(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Created by is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      assertThat(this.auditableSupport.getCreatedBy()).isEqualTo("testUser");
    }
  }

  @Test
  public void setAndGetCreatedOn() {

    LocalDateTime now = LocalDateTime.now();

    this.auditableSupport.setCreatedOn(now);

    assertThat(this.auditableSupport.getCreatedOn()).isEqualTo(now);
  }

  @Test(expected = IllegalArgumentException.class)
  public void setCreatedOnToNullThrowsIllegalArgumentException() {

    LocalDateTime createdOn = LocalDateTime.of(2018, Month.FEBRUARY, 15, 22, 30);

    try {
      this.auditableSupport.setCreatedOn(createdOn);
      this.auditableSupport.setCreatedOn(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Created on is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      assertThat(this.auditableSupport.getCreatedOn()).isEqualTo(createdOn);
    }
  }

  @Test
  public void setAndGetCreatedWith() {

    this.auditableSupport.setCreatedWith("testProcess");

    assertThat(this.auditableSupport.getCreatedWith()).isEqualTo("testProcess");
  }

  @Test(expected = IllegalArgumentException.class)
  public void setCreatedWithToNullThrowsIllegalArgumentException() {

    try {
      this.auditableSupport.setCreatedWith("testProcess");
      this.auditableSupport.setCreatedWith(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Created with is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      assertThat(this.auditableSupport.getCreatedWith()).isEqualTo("testProcess");
    }
  }

  @Test
  public void setAndGetModifiedBy() {

    this.auditableSupport.setModifiedBy("testUser");

    assertThat(this.auditableSupport.getModifiedBy()).isEqualTo("testUser");
    assertThat(this.auditableSupport.getLastModifiedBy()).isEqualTo("testUser");
  }

  @Test
  public void setModifiedByTwiceReturnsFirstUserForLastModifiedBy() {

    this.auditableSupport.setModifiedBy("userOne");

    assertThat(this.auditableSupport.getModifiedBy()).isEqualTo("userOne");
    assertThat(this.auditableSupport.getLastModifiedBy()).isEqualTo("userOne");

    this.auditableSupport.setModifiedBy("userTwo");

    assertThat(this.auditableSupport.getModifiedBy()).isEqualTo("userTwo");
    assertThat(this.auditableSupport.getLastModifiedBy()).isEqualTo("userOne");
  }

  @Test(expected = IllegalArgumentException.class)
  public void setModifiedByToNullThrowsIllegalArgumentException() {

    try {
      this.auditableSupport.setModifiedBy("testUser");
      this.auditableSupport.setModifiedBy(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Modified by is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      assertThat(this.auditableSupport.getModifiedBy()).isEqualTo("testUser");
      assertThat(this.auditableSupport.getLastModifiedBy()).isEqualTo("testUser");
    }
  }

  @Test
  public void getModifiedByReturnsCreatedBy() {

    this.auditableSupport.setCreatedBy("testUser");

    assertThat(this.auditableSupport.getModifiedBy()).isEqualTo("testUser");
    assertThat(this.auditableSupport.getLastModifiedBy()).isNull();
  }

  @Test
  public void getModifiedByReturnsModifiedBy() {

    this.auditableSupport.setCreatedBy("userOne");
    this.auditableSupport.setModifiedBy("userTwo");

    assertThat(this.auditableSupport.getModifiedBy()).isEqualTo("userTwo");
    assertThat(this.auditableSupport.getLastModifiedBy()).isEqualTo("userTwo");
  }

  @Test
  public void setAndGetModifiedOn() {

    LocalDateTime now = LocalDateTime.now();

    this.auditableSupport.setModifiedOn(now);

    assertThat(this.auditableSupport.getModifiedOn()).isEqualTo(now);
    assertThat(this.auditableSupport.getLastModifiedOn()).isEqualTo(now);
  }

  @Test
  public void setModifiedOnTwiceReturnsFirstDateTimeForLastModifiedOn() {

    LocalDateTime modifiedOnOne = LocalDateTime.of(2017, Month.FEBRUARY, 15, 22, 50);

    this.auditableSupport.setModifiedOn(modifiedOnOne);

    assertThat(this.auditableSupport.getModifiedOn()).isEqualTo(modifiedOnOne);
    assertThat(this.auditableSupport.getLastModifiedOn()).isEqualTo(modifiedOnOne);

    LocalDateTime modifiedOnTwo = LocalDateTime.of(2018, Month.FEBRUARY, 15, 22, 50);

    this.auditableSupport.setModifiedOn(modifiedOnTwo);

    assertThat(this.auditableSupport.getModifiedOn()).isEqualTo(modifiedOnTwo);
    assertThat(this.auditableSupport.getLastModifiedOn()).isEqualTo(modifiedOnOne);
  }

  @Test(expected = IllegalArgumentException.class)
  public void setModifiedOnToNullThrowsIllegalArgumentException() {

    LocalDateTime now = LocalDateTime.now();

    try {
      this.auditableSupport.setModifiedOn(now);
      this.auditableSupport.setModifiedOn(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Modified on is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      assertThat(this.auditableSupport.getModifiedOn()).isEqualTo(now);
      assertThat(this.auditableSupport.getLastModifiedOn()).isEqualTo(now);
    }
  }

  @Test
  public void getModifiedOnReturnsCreatedOn() {

    LocalDateTime createdOn = LocalDateTime.of(2018, Month.FEBRUARY, 15, 22, 45);

    this.auditableSupport.setCreatedOn(createdOn);

    assertThat(this.auditableSupport.getModifiedOn()).isEqualTo(createdOn);
    assertThat(this.auditableSupport.getLastModifiedOn()).isNull();
  }

  @Test
  public void getModifiedOnReturnsModifiedOn() {

    LocalDateTime createdOn = LocalDateTime.of(2017, Month.FEBRUARY, 15, 22, 45);
    LocalDateTime modifiedOn = LocalDateTime.of(2018, Month.FEBRUARY, 15, 22, 45);

    this.auditableSupport.setCreatedOn(createdOn);
    this.auditableSupport.setModifiedOn(modifiedOn);

    assertThat(this.auditableSupport.getModifiedOn()).isEqualTo(modifiedOn);
    assertThat(this.auditableSupport.getLastModifiedOn()).isEqualTo(modifiedOn);
  }

  @Test
  public void setAndGetModifiedWith() {

    this.auditableSupport.setModifiedWith("testProcess");

    assertThat(this.auditableSupport.getModifiedWith()).isEqualTo("testProcess");
    assertThat(this.auditableSupport.getLastModifiedWith()).isEqualTo("testProcess");
  }

  @Test
  public void setModifiedWithTwiceReturnsFirstProcessForLastModifiedWith() {

    this.auditableSupport.setModifiedWith("processOne");

    assertThat(this.auditableSupport.getModifiedWith()).isEqualTo("processOne");
    assertThat(this.auditableSupport.getLastModifiedWith()).isEqualTo("processOne");

    this.auditableSupport.setModifiedWith("processTwo");

    assertThat(this.auditableSupport.getModifiedWith()).isEqualTo("processTwo");
    assertThat(this.auditableSupport.getLastModifiedWith()).isEqualTo("processOne");
  }

  @Test(expected = IllegalArgumentException.class)
  public void setModifiedWithToNullThrowsIllegalArgumentException() {

    try {
      this.auditableSupport.setModifiedWith("testProcess");
      this.auditableSupport.setModifiedWith(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Modified with is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      assertThat(this.auditableSupport.getModifiedWith()).isEqualTo("testProcess");
      assertThat(this.auditableSupport.getLastModifiedWith()).isEqualTo("testProcess");
    }
  }

  @Test
  public void getModifiedWithReturnsCreatedWith() {

    this.auditableSupport.setCreatedWith("testProcess");

    assertThat(this.auditableSupport.getModifiedWith()).isEqualTo("testProcess");
    assertThat(this.auditableSupport.getLastModifiedWith()).isNull();
  }

  @Test
  public void getModifiedWithReturnsModifiedWith() {

    this.auditableSupport.setCreatedWith("processOne");
    this.auditableSupport.setModifiedWith("processTwo");

    assertThat(this.auditableSupport.getModifiedWith()).isEqualTo("processTwo");
    assertThat(this.auditableSupport.getLastModifiedWith()).isEqualTo("processTwo");
  }

  @Test
  public void getUsernameReturnsValue() {
    assertThat(this.auditableSupport.getUsername()).isNotEmpty();
  }

  static final class TestAuditableSupport extends AuditableSupport<String, String, Long> {

    @Override
    public boolean isModified() {
      throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
    }

    @Override
    public boolean isModified(String propertyName) {
      throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
    }
  }
}
