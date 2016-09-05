/*
 * Copyright 2016 Author or Authors.
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

package org.cp.elements.io;

import java.io.File;
import java.io.FileFilter;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Calendar;
import java.util.Date;

import org.cp.elements.lang.Filter;
import org.cp.elements.lang.NullSafe;
import org.cp.elements.lang.RelationalOperator;

/**
 * The FileLastModifiedFilter class is a {@link FileFilter} and {@link Filter} of {@link File}s implementation
 * filtering {@link File}s based on their last modified timestamp (date/time).
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.cp.elements.lang.Filter
 * @see org.cp.elements.lang.RelationalOperator
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class FileLastModifiedFilter implements FileFilter, Filter<File> {

  /**
   * Creates an instance of {@link FileLastModifiedFilter} initialized with the provided {@link RelationalOperator},
   * which is in the evaluation of the {@link File}s last modified timestamp when {@link #accept(File)} is called.
   *
   * @param operator {@link RelationalOperator} used to evaluate the {@link File}.
   * @return an instance of {@link FileLastModifiedFilter} initialized with the given {@link RelationalOperator}
   * used when evaluating the {@link File}.
   * @see org.cp.elements.lang.RelationalOperator
   * @see FileLastModifiedFilter
   */
  protected static FileLastModifiedFilter create(RelationalOperator<Long> operator) {
    return new FileLastModifiedFilter() {
      @Override @NullSafe
      public boolean accept(File file) {
        return (file != null && operator.evaluate(file.lastModified()));
      }
    };
  }

  /**
   * Determines whether the given {@link File} is accepted by this {@link FileFilter}.  The {@link File} is accepted
   * if the {@link File}'s lastModified property satisfies the expected date/time criteria of this {@link FileFilter}.
   *
   * @param file {@link File} to evaluate and filter.
   * @return a boolean value indicating whether the {@link File}'s lastModified property satisfies the criteria
   * of this {@link FileFilter}.
   * @see java.io.FileFilter#accept(java.io.File)
   * @see java.io.File
   */
  public abstract boolean accept(File file);

  /**
   * Factory method to create an instance of the {@link FileLastModifiedFilter} that evaluates and filters {@link File}s
   * based on whether they were last modified after the given timestamp.
   *
   * @param lastModified timestamp in milliseconds used to filter {@link File}'s that were last modified
   * after this date/time.
   * @return an instance of the {@link FileLastModifiedFilter}.
   * @see org.cp.elements.lang.RelationalOperator#greaterThan(Comparable)
   * @see #create(org.cp.elements.lang.RelationalOperator)
   */
  public static FileLastModifiedFilter after(long lastModified) {
    return create(RelationalOperator.greaterThan(lastModified));
  }

  /**
   * Factory method to create an instance of the {@link FileLastModifiedFilter} that evaluates and filters {@link File}s
   * based on whether they were last modified after the given date/time.
   *
   * @param dateTime {@link Calendar} used to filter {@link File}'s that were last modified after this date/time.
   * @return an instance of the {@link FileLastModifiedFilter}.
   * @see java.util.Calendar#getTimeInMillis()
   * @see #after(long)
   */
  public static FileLastModifiedFilter after(Calendar dateTime) {
    return after(dateTime.getTimeInMillis());
  }

  /**
   * Factory method to create an instance of the {@link FileLastModifiedFilter} that evaluates and filters {@link File}s
   * based on whether they were last modified after the given date/time.
   *
   * @param dateTime {@link Date} used to filter {@link File}'s that were last modified after this date/time.
   * @return an instance of the {@link FileLastModifiedFilter}.
   * @see java.util.Date#getTime()
   * @see #after(long)
   */
  public static FileLastModifiedFilter after(Date dateTime) {
    return after(dateTime.getTime());
  }

  /**
   * Factory method to create an instance of the {@link FileLastModifiedFilter} that evaluates and filters {@link File}s
   * based on whether they were last modified after the given date/time.
   *
   * @param dateTime {@link LocalDateTime} used to filter {@link File}'s that were last modified after this date/time.
   * @return an instance of the {@link FileLastModifiedFilter}.
   * @see java.time.LocalDateTime
   * @see #after(long)
   */
  public static FileLastModifiedFilter after(LocalDateTime dateTime) {
    return after(dateTime.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli());
  }

  /**
   * Factory method to create an instance of the {@link FileLastModifiedFilter} that evaluates and filters {@link File}s
   * based on whether they were last modified before the given timestamp.
   *
   * @param lastModified timestamp in milliseconds used to filter {@link File}'s that were last modified
   * before this date/time.
   * @return an instance of the {@link FileLastModifiedFilter}.
   * @see org.cp.elements.lang.RelationalOperator#lessThan(Comparable)
   * @see #create(org.cp.elements.lang.RelationalOperator)
   */
  public static FileLastModifiedFilter before(long lastModified) {
    return create(RelationalOperator.lessThan(lastModified));
  }

  /**
   * Factory method to create an instance of the {@link FileLastModifiedFilter} that evaluates and filters {@link File}s
   * based on whether they were last modified before the given date/time.
   *
   * @param dateTime {@link Calendar} used to filter {@link File}'s that were last modified before this date/time.
   * @return an instance of the {@link FileLastModifiedFilter}.
   * @see java.util.Calendar#getTimeInMillis()
   * @see #before(long)
   */
  public static FileLastModifiedFilter before(Calendar dateTime) {
    return before(dateTime.getTimeInMillis());
  }

  /**
   * Factory method to create an instance of the {@link FileLastModifiedFilter} that evaluates and filters {@link File}s
   * based on whether they were last modified before the given date/time.
   *
   * @param dateTime {@link Date} used to filter {@link File}'s that were last modified before this date/time.
   * @return an instance of the {@link FileLastModifiedFilter}.
   * @see java.util.Date#getTime()
   * @see #before(long)
   */
  public static FileLastModifiedFilter before(Date dateTime) {
    return before(dateTime.getTime());
  }

  /**
   * Factory method to create an instance of the {@link FileLastModifiedFilter} that evaluates and filters {@link File}s
   * based on whether they were last modified before the given date/time.
   *
   * @param dateTime {@link LocalDateTime} used to filter {@link File}'s that were last modified before this date/time.
   * @return an instance of the {@link FileLastModifiedFilter}.
   * @see java.time.LocalDateTime
   * @see #before(long)
   */
  public static FileLastModifiedFilter before(LocalDateTime dateTime) {
    return before(dateTime.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli());
  }

  /**
   * Factory method to create an instance of the {@link FileLastModifiedFilter} that evaluates and filters {@link File}s
   * based on whether they were last modified between a given span of time.
   *
   * @param lastModifiedBegin timestamp in milliseconds used to filter {@link File}'s that were last modified
   * on or after this date/time.
   * @param lastModifiedEnd timestamp in milliseconds used to filter {@link File}'s that were last modified
   * on or before this date/time.
   * @return an instance of the {@link FileLastModifiedFilter}.
   * @see org.cp.elements.lang.RelationalOperator#greaterThanEqualToAndLessThanEqualTo(Comparable, Comparable)
   * @see #create(org.cp.elements.lang.RelationalOperator)
   */
  public static FileLastModifiedFilter during(long lastModifiedBegin, long lastModifiedEnd) {
    return create(RelationalOperator.greaterThanEqualToAndLessThanEqualTo(lastModifiedBegin, lastModifiedEnd));
  }

  /**
   * Factory method to create an instance of the {@link FileLastModifiedFilter} that evaluates and filters {@link File}s
   * based on whether they were last modified between a given span of time.
   *
   * @param dateTimeBegin {@link Calendar} used to filter {@link File}'s that were last modified
   * on or after this date/time.
   * @param dateTimeEnd {@link Calendar} used to filter {@link File}'s that were last modified
   * on or before this date/time.
   * @return an instance of the {@link FileLastModifiedFilter}.
   * @see java.util.Calendar#getTimeInMillis()
   * @see #during(long, long)
   */
  public static FileLastModifiedFilter during(Calendar dateTimeBegin, Calendar dateTimeEnd) {
    return during(dateTimeBegin.getTimeInMillis(), dateTimeEnd.getTimeInMillis());
  }

  /**
   * Factory method to create an instance of the {@link FileLastModifiedFilter} that evaluates and filters {@link File}s
   * based on whether they were last modified between a given span of time.
   *
   * @param dateTimeBegin {@link Date} used to filter {@link File}'s that were last modified on or after this date/time.
   * @param dateTimeEnd {@link Date} used to filter {@link File}'s that were last modified on or before this date/time.
   * @return an instance of the {@link FileLastModifiedFilter}.
   * @see java.util.Date#getTime()
   * @see #during(long, long)
   */
  public static FileLastModifiedFilter during(Date dateTimeBegin, Date dateTimeEnd) {
    return during(dateTimeBegin.getTime(), dateTimeEnd.getTime());
  }

  /**
   * Factory method to create an instance of the {@link FileLastModifiedFilter} that evaluates and filters {@link File}s
   * based on whether they were last modified between a given span of time.
   *
   * @param dateTimeBegin {@link LocalDateTime} used to filter {@link File}'s that were last modified
   * on or after this date/time.
   * @param dateTimeEnd {@link LocalDateTime} used to filter {@link File}'s that were last modified
   * on or before this date/time.
   * @return an instance of the {@link FileLastModifiedFilter}.
   * @see java.time.LocalDateTime
   * @see #during(long, long)
   */
  public static FileLastModifiedFilter during(LocalDateTime dateTimeBegin, LocalDateTime dateTimeEnd) {
    return during(dateTimeBegin.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli(),
      dateTimeEnd.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli());
  }

  /**
   * Factory method to create an instance of the {@link FileLastModifiedFilter} that evaluates and filters {@link File}s
   * based on whether they were last modified on a given date/time.
   *
   * @param lastModified timestamp in milliseconds used to filter {@link File}'s that were last modified
   * on this date/time.
   * @return an instance of the {@link FileLastModifiedFilter}.
   * @see org.cp.elements.lang.RelationalOperator#equalTo(Comparable)
   * @see #create(org.cp.elements.lang.RelationalOperator)
   */
  public static FileLastModifiedFilter on(long lastModified) {
    return create(RelationalOperator.equalTo(lastModified));
  }

  /**
   * Factory method to create an instance of the {@link FileLastModifiedFilter} that evaluates and filters {@link File}s
   * based on whether they were last modified on a given date/time.
   *
   * @param dateTime {@link Calendar} used to filter {@link File}'s that were last modified on this date/time.
   * @return an instance of the {@link FileLastModifiedFilter}.
   * @see java.util.Calendar#getTimeInMillis()
   * @see #on(long)
   */
  public static FileLastModifiedFilter on(Calendar dateTime) {
    return on(dateTime.getTimeInMillis());
  }

  /**
   * Factory method to create an instance of the {@link FileLastModifiedFilter} that evaluates and filters {@link File}s
   * based on whether they were last modified on a given date/time.
   *
   * @param dateTime {@link Date} used to filter {@link File}'s that were last modified on this date/time.
   * @return an instance of the {@link FileLastModifiedFilter}.
   * @see java.util.Date#getTime()
   * @see #on(long)
   */
  public static FileLastModifiedFilter on(Date dateTime) {
    return on(dateTime.getTime());
  }

  /**
   * Factory method to create an instance of the {@link FileLastModifiedFilter} that evaluates and filters {@link File}s
   * based on whether they were last modified on a given date/time.
   *
   * @param dateTime {@link LocalDateTime} used to filter {@link File}'s that were last modified on this date/time.
   * @return an instance of the {@link FileLastModifiedFilter}.
   * @see java.time.LocalDateTime
   * @see #on(long)
   */
  public static FileLastModifiedFilter on(LocalDateTime dateTime) {
    return on(dateTime.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli());
  }

  /**
   * Factory method to create an instance of the {@link FileLastModifiedFilter} that evaluates and filters {@link File}s
   * based on whether they were last modified before or after a given span of time.
   *
   * @param onBefore timestamp in milliseconds used to filter {@link File}'s that were last modified
   * on or before this date/time.
   * @param onAfter timestamp in milliseconds used to filter {@link File}'s that were last modified
   * on or after this date/time.
   * @return an instance of the {@link FileLastModifiedFilter}.
   * @see org.cp.elements.lang.RelationalOperator#lessThanEqualToOrGreaterThanEqualTo(Comparable, Comparable)
   * @see #create(org.cp.elements.lang.RelationalOperator)
   */
  public static FileLastModifiedFilter outside(long onBefore, long onAfter) {
    return create(RelationalOperator.lessThanEqualToOrGreaterThanEqualTo(onBefore, onAfter));
  }

  /**
   * Factory method to create an instance of the {@link FileLastModifiedFilter} that evaluates and filters {@link File}s
   * based on whether they were last modified before or after a given span of time.
   *
   * @param onBefore {@link Calendar} used to filter {@link File}'s that were last modified on or before this date/time.
   * @param onAfter {@link Calendar} used to filter {@link File}'s that were last modified on or after this date/time.
   * @return an instance of the {@link FileLastModifiedFilter}.
   * @see java.util.Calendar#getTimeInMillis()
   * @see #outside(long, long)
   */
  public static FileLastModifiedFilter outside(Calendar onBefore, Calendar onAfter) {
    return outside(onBefore.getTimeInMillis(), onAfter.getTimeInMillis());
  }

  /**
   * Factory method to create an instance of the {@link FileLastModifiedFilter} that evaluates and filters {@link File}s
   * based on whether they were last modified before or after a given span of time.
   *
   * @param onBefore {@link Date} used to filter {@link File}'s that were last modified on or before this date/time.
   * @param onAfter {@link Date} used to filter {@link File}'s that were last modified on or after this date/time.
   * @return an instance of the {@link FileLastModifiedFilter}.
   * @see java.util.Date#getTime()
   * @see #outside(long, long)
   */
  public static FileLastModifiedFilter outside(Date onBefore, Date onAfter) {
    return outside(onBefore.getTime(), onAfter.getTime());
  }

  /**
   * Factory method to create an instance of the {@link FileLastModifiedFilter} that evaluates and filters {@link File}s
   * based on whether they were last modified before or after a given span of time.
   *
   * @param onBefore {@link LocalDateTime} used to filter {@link File}'s that were last modified
   * on or before this date/time.
   * @param onAfter {@link LocalDateTime} used to filter {@link File}'s that were last modified
   * on or after this date/time.
   * @return an instance of the {@link FileLastModifiedFilter}.
   * @see java.time.LocalDateTime
   * @see #outside(long, long)
   */
  public static FileLastModifiedFilter outside(LocalDateTime onBefore, LocalDateTime onAfter) {
    return outside(onBefore.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli(),
      onAfter.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli());
  }
}
