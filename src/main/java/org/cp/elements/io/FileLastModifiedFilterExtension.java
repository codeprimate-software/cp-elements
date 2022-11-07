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
package org.cp.elements.io;

import java.io.File;
import java.time.LocalDate;
import java.time.LocalTime;

import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.RelationalOperator;
import org.cp.elements.lang.annotation.NotNull;

/**
 * Extension of {@link FileLastModifiedFilter} using {@literal java.time.LocalTime} types to filter {@link File Files}
 * by {@link File#lastModified() last modified timestamp}.
 *
 * In this case, the factory methods in this class are only interested in filtering {@link File Files}
 * by the {@link LocalTime time of day}, ignoring the {@link LocalDate date}. Additionally, consideration is only
 * taken for {@literal local} {@link File Files} are considered; remote {@link File Files} are not taken
 * into consideration when filtering.
 *
 * @author John Blum
 * @see java.io.File
 * @see java.time.LocalTime
 * @see org.cp.elements.io.FileLastModifiedFilter
 * @since 1.0.0
 */
public abstract class FileLastModifiedFilterExtension extends FileLastModifiedFilter {

  /**
   * Factory method used to construct a new instance of {@link FileLastModifiedFilterExtension} used to
   * filter {@link File Files} with a {@link File#lastModified() last modified timestamp} occurring after
   * the given, required {@link LocalTime time}.
   *
   * @param lastModified {@link LocalTime} used to filter {@link File Files}
   * with a {@link File#lastModified() last modified timestamp} occurring after this time; must not be {@literal null}.
   * @return a new {@link FileLastModifiedFilter}.
   * @throws IllegalArgumentException if the {@link LocalTime} is {@literal null}.
   * @see java.io.File#lastModified()
   * @see java.time.LocalTime
   */
  public static @NotNull FileLastModifiedFilter afterTime(@NotNull LocalTime lastModified) {
    return create(RelationalOperator.greaterThan(ObjectUtils.requireObject(lastModified, "Time is required")),
      FileUtils::toLastModifiedTime);
  }

  /**
   * Factory method used to construct a new instance of {@link FileLastModifiedFilterExtension} used to
   * filter {@link File Files} with a {@link File#lastModified() last modified timestamp} occurring before
   * the given, required {@link LocalTime time}.
   *
   * @param lastModified {@link LocalTime} used to filter {@link File Files}
   * with a {@link File#lastModified() last modified timestamp} occurring before this time; must not be {@literal null}.
   * @return a new {@link FileLastModifiedFilter}.
   * @throws IllegalArgumentException if the {@link LocalTime} is {@literal null}.
   * @see java.io.File#lastModified()
   * @see java.time.LocalTime
   */
  public static @NotNull FileLastModifiedFilter beforeTime(@NotNull LocalTime lastModified) {
    return create(RelationalOperator.lessThan(ObjectUtils.requireObject(lastModified, "Time is required")),
      FileUtils::toLastModifiedTime);
  }

  /**
   * Factory method used to construct a new instance of {@link FileLastModifiedFilterExtension} used to
   * filter {@link File Files} with a {@link File#lastModified() last modified timestamp} occurring during
   * (between) the given, required {@link LocalTime times}.
   *
   * @param lastModifiedOnAfter {@link LocalTime} used to filter {@link File Files}
   * with a {@link File#lastModified() last modified timestamp} occurring on or after this time;
   * must not be {@literal null}.
   * @param lastModifiedOnBefore {@link LocalTime} used to filter {@link File Files}
   * with a {@link File#lastModified() last modified timestamp} occurring on or before this time;
   * must not be {@literal null}.
   * @return a new {@link FileLastModifiedFilter}.
   * @throws IllegalArgumentException if either {@link LocalTime} is {@literal null}.
   * @see java.io.File#lastModified()
   * @see java.time.LocalTime
   */
  public static @NotNull FileLastModifiedFilter duringTime(
      @NotNull LocalTime lastModifiedOnAfter, @NotNull LocalTime lastModifiedOnBefore) {

    return create(RelationalOperator.greaterThanEqualToAndLessThanEqualTo(
        ObjectUtils.requireObject(lastModifiedOnAfter, "Time on or after is required"),
        ObjectUtils.requireObject(lastModifiedOnBefore, "Time on or before is required")),
      FileUtils::toLastModifiedTime);
  }

  /**
   * Factory method used to construct a new instance of {@link FileLastModifiedFilterExtension} used to
   * filter {@link File Files} with a {@link File#lastModified() last modified timestamp} occurring on
   * the given, required {@link LocalTime time}.
   *
   * @param lastModified {@link LocalTime} used to filter {@link File Files}
   * with a {@link File#lastModified() last modified timestamp} occurring on this time; must not be {@literal null}.
   * @return a new {@link FileLastModifiedFilter}.
   * @throws IllegalArgumentException if the {@link LocalTime} is {@literal null}.
   * @see java.io.File#lastModified()
   * @see java.time.LocalTime
   */
  public static @NotNull FileLastModifiedFilter onTime(@NotNull LocalTime lastModified) {
    return create(RelationalOperator.equalTo(ObjectUtils.requireObject(lastModified, "Time is required")),
      FileUtils::toLastModifiedTime);
  }

  /**
   * Factory method used to construct a new instance of {@link FileLastModifiedFilterExtension} used to
   * filter {@link File Files} with a {@link File#lastModified() last modified timestamp} occurring outside
   * of the given, required {@link LocalTime times}.
   *
   * @param lastModifiedBefore {@link LocalTime} used to filter {@link File Files}
   * with a {@link File#lastModified() last modified timestamp} occurring before this time;
   * must not be {@literal null}.
   * @param lastModifiedAfter {@link LocalTime} used to filter {@link File Files}
   * with a {@link File#lastModified() last modified timestamp} occurring after this time;
   * must not be {@literal null}.
   * @return a new {@link FileLastModifiedFilter}.
   * @throws IllegalArgumentException if either {@link LocalTime} is {@literal null}.
   * @see java.io.File#lastModified()
   * @see java.time.LocalTime
   */
  public static @NotNull FileLastModifiedFilter outsideTime(
      @NotNull LocalTime lastModifiedBefore, @NotNull LocalTime lastModifiedAfter) {

    return create(RelationalOperator.lessThanOrGreaterThan(
        ObjectUtils.requireObject(lastModifiedBefore, "Time before is required"),
        ObjectUtils.requireObject(lastModifiedAfter, "Time after is required")),
      FileUtils::toLastModifiedTime);
  }
}
