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

import java.time.Instant;

import org.cp.elements.lang.Auditable;
import org.cp.elements.lang.Constants;

/**
 * The {@link AbstractAuditable} class is abstract base class provided to conveniently implement
 * the {@link Auditable} interface.
 *
 * @author John J. Blum
 * @param <USER> {@link Class type} used to track the user.
 * @param <PROCESS> {@link Class type} used to track the process.
 * @see java.time.Instant
 * @see org.cp.elements.lang.Auditable
 * @see org.cp.elements.lang.support.AbstractIdentifiable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractAuditable<USER, PROCESS, ID extends Comparable<ID>> extends AbstractIdentifiable<ID>
    implements Auditable<USER, PROCESS, ID> {

  /**
   * @inheritDoc
   */
  @Override
  public USER getCreatedBy() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * @inheritDoc
   */
  @Override
  public void setCreatedBy(USER user) {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * @inheritDoc
   */
  @Override
  public Instant getCreatedOn() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * @inheritDoc
   */
  @Override
  public void setCreatedOn(Instant createdOn) {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * @inheritDoc
   */
  @Override
  public PROCESS getCreatedWith() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * @inheritDoc
   */
  @Override
  public void setCreatedWith(PROCESS process) {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * @inheritDoc
   */
  @Override
  public USER getLastModifiedBy() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * @inheritDoc
   */
  @Override
  public Instant getLastModifiedOn() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * @inheritDoc
   */
  @Override
  public PROCESS getLastModifiedWith() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * @inheritDoc
   */
  @Override
  public boolean isModified() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * @inheritDoc
   */
  @Override
  public boolean isModified(String propertyName) {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * @inheritDoc
   */
  @Override
  public USER getModifiedBy() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * @inheritDoc
   */
  @Override
  public void setModifiedBy(USER user) {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * @inheritDoc
   */
  @Override
  public Instant getModifiedOn() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * @inheritDoc
   */
  @Override
  public void setModifiedOn(Instant modifiedOn) {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * @inheritDoc
   */
  @Override
  public PROCESS getModifiedWith() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * @inheritDoc
   */
  @Override
  public void setModifiedWith(PROCESS process) {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }
}
