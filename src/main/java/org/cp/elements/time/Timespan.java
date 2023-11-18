/*
 * Copyright 2023-Present Author or Authors.
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
package org.cp.elements.time;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.Month;
import java.time.Year;
import java.time.YearMonth;
import java.time.format.DateTimeFormatter;
import java.util.Optional;
import java.util.function.Predicate;
import java.util.function.Supplier;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.Renderable;
import org.cp.elements.lang.annotation.Dsl;
import org.cp.elements.lang.annotation.FluentApi;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Abstract Data Type (ADT) modeling a span of time.
 *
 * @author John Blum
 * @see java.lang.Comparable
 * @see java.time.LocalDate
 * @see java.time.LocalDateTime
 * @see java.time.LocalTime
 * @see java.time.YearMonth
 * @see java.time.Month
 * @see java.time.Year
 * @see org.cp.elements.lang.Builder
 * @see org.cp.elements.lang.Renderable
 * @see org.cp.elements.lang.annotation.Dsl
 * @see org.cp.elements.lang.annotation.FluentApi
 * @since 2.0.0
 */
@FluentApi
@SuppressWarnings("unused")
public class Timespan implements Comparable<Timespan>, Renderable {

	protected static final String BEGINNING_OF_TIME = "Beginning of Time";
	protected static final String DATE_PATTERN = "yyyy-MM-dd HH:mm:ss:S";
	protected static final String DATE_TIME_PATTERN = DATE_PATTERN.concat(" HH:mm:ss:S");
	protected static final String END_OF_TIME = "End of Time";
	protected static final String TIMESPAN_TO_STRING = "%s-%s";

	protected static final Predicate<LocalDateTime> TRUE_PREDICATE = dateTime -> true;

	protected static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern(DATE_PATTERN);
	protected static final DateTimeFormatter DATE_TIME_FORMATTER = DateTimeFormatter.ofPattern(DATE_TIME_PATTERN);

	/**
	 * Factory method used to construct a new {@link Timespan} beginning in the given, required {@link Year}.
	 * <p>
	 * The beginning of the {@link Timespan} is set to {@literal <Year>-January-1:00:00:00.0}.
	 *
	 * @param year {@link Year} marking the beginning of the {@link Timespan}; must not be {@literal null}.
	 * @return a new {@link Timespan} beginning in the given {@link Year}.
	 * @throws IllegalArgumentException if the given {@link Year} is {@literal null}.
	 * @see #beginning(YearMonth)
	 * @see java.time.Year
	 */
	public static @NotNull Timespan beginning(@NotNull Year year) {
		Assert.notNull(year, "Beginning year is required");
		return beginning(year.atMonth(Month.JANUARY));
	}

	/**
	 * Factory method used to construct a new {@link Timespan} beginning in the given, required {@link YearMonth}.
	 * <p>
	 * The beginning of the {@link Timespan} is set to {@literal <Year>-<Month>-1:00:00:00.0}.
	 *
	 * @param yearMonth {@link YearMonth} marking the beginning of the {@link Timespan}; mus not be {@literal null}.
	 * @return a new {@link Timespan} beginning in the given {@link YearMonth}.
	 * @throws IllegalArgumentException if the given {@link YearMonth} are {@literal null}.
	 * @see #beginning(LocalDate)
	 * @see java.time.YearMonth
	 */
	public static @NotNull Timespan beginning(@NotNull YearMonth yearMonth) {
		Assert.notNull(yearMonth, "Beginning year and month are required");
		return beginning(yearMonth.atDay(1));
	}

	/**
	 * Factory method used to construct a new {@link Timespan} beginning on the given, required {@link LocalDate},
	 * with no timestamp.
	 *
	 * @param date {@link LocalDate} marking the beginning of the {@link Timespan}; must not be {@literal null}.
	 * @return a new {@link Timespan} beginning on the given {@link LocalDate}.
	 * @throws IllegalArgumentException if the given {@link LocalDate} is {@literal null}.
	 * @see #beginning(LocalDateTime)
	 * @see java.time.LocalDate
	 */
	public static @NotNull Timespan beginning(@NotNull LocalDate date) {
		Assert.notNull(date, "Beginning date is required");
		return beginning(date.atStartOfDay());
	}

	/**
	 * Factory method used to construct a new {@link Timespan} beginning on the given,
	 * required {@link LocalDateTime date and time}.
	 *
	 * @param dateTime {@link LocalDateTime} marking the beginning of the {@link Timespan}; must not be {@literal null}.
	 * @return a new {@link Timespan} beginning on the given {@link LocalDateTime}.
	 * @throws IllegalArgumentException if the given {@link LocalDateTime} is {@literal null}.
	 * @see java.time.LocalDateTime
	 */
	public static @NotNull Timespan beginning(@NotNull LocalDateTime dateTime) {
		Assert.notNull(dateTime, "Beginning date and time is required");
		return new Timespan(dateTime, null);
	}

	/**
	 * Factory method used to construct a new {@link Timespan} beginning at the given, required {@link LocalTime}
	 * on the current day.
	 * <p>
	 * The beginning of the {@link Timespan} is set to
	 * {@literal <Current Year>-<Current Month>-<Current Day of Month>-<Hour>-<Minute>-<Second>}.
	 *
	 * @param time {@link LocalTime} marking the beginning of the {@link Timespan}; must not be {@literal null}.
	 * @return a new {@link Timespan} beginning at the given {@link LocalTime}.
	 * @throws IllegalArgumentException if the given {@link LocalTime} is {@literal null}.
	 * @see #beginning(LocalDateTime)
	 * @see java.time.LocalTime
	 */
	public static @NotNull Timespan beginning(@NotNull LocalTime time) {
		Assert.notNull(time, "Start time is required");
		return beginning(time.atDate(LocalDate.now()));
	}

	/**
	 * Factory method used to construct a new {@link Timespan} between the given {@link LocalDateTime begin}
	 * and {@link LocalDateTime end} date/times.
	 *
	 * @param begin {@link LocalDateTime} declaring the {@literal beginning}, or start of the {@link Timespan};
	 * must not be {@literal null}.
	 * @param end {@link LocalDateTime} declaring the {@literal ending}, or start of the {@link Timespan};
	 * must not be {@literal null}.
	 * @return a new {@link Timespan} between the given {@link LocalDateTime begin}
	 * and {@link LocalDateTime end} date/times.
	 * @throws IllegalArgumentException if the {@link LocalDateTime begin} is on or after the {@link LocalDateTime end}.
	 * @see java.time.LocalDateTime
	 */
	@SuppressWarnings("all")
	public static @NotNull Timespan between(@NotNull LocalDateTime begin, @NotNull LocalDateTime end) {

		Assert.notNull(begin, "Begin date/time is required");
		Assert.notNull(end, "End date/time is required");

		Supplier<Boolean> timePredicate = () -> begin == null || end == null || begin.isBefore(end);

		Assert.isTrue(timePredicate.get(), "Beginning [%s] must occur before the Ending [%s]",
			begin.format(DATE_TIME_FORMATTER), end.format(DATE_TIME_FORMATTER));

		return new Timespan(begin, end);
	}

	/**
	 * Factory method used to construct a new {@link Timespan} ending in the given, required {@link Year}.
	 * <p>
	 * The ending of the {@link Timespan} is set to {@literal <Year>-December-31:00:00:00.0}.
	 *
	 * @param year {@link Year} marking the end of the {@link Timespan}; must not be {@literal null}.
	 * @return a new {@link Timespan} ending in the given {@link Year}.
	 * @throws IllegalArgumentException if the given {@link Year} is {@literal null}.
	 * @see #ending(YearMonth)
	 * @see java.time.Year
	 */
	public static @NotNull Timespan ending(@NotNull Year year) {
		Assert.notNull(year, "End year is required");
		return ending(year.atMonth(Month.DECEMBER));
	}

	/**
	 * Factory method used to construct a new {@link Timespan} ending in the given, required {@link YearMonth}.
	 * <p>
	 * The ending of the {@link Timespan} is set to {@literal <Year>-<Month>-<last-day-of-given-month>:00:00:00.0}.
	 *
	 * @param yearMonth {@link YearMonth} marking the end of the {@link Timespan}; must not be {@literal null}.
	 * @return a new {@link Timespan} ending in the given {@link YearMonth}.
	 * @throws IllegalArgumentException if the given {@link YearMonth} is {@literal null}.
	 * @see java.time.YearMonth
	 * @see #ending(LocalDate)
	 */
	public static @NotNull Timespan ending(@NotNull YearMonth yearMonth) {
		Assert.notNull(yearMonth, "End year and month are required");
		return ending(yearMonth.atEndOfMonth());
	}

	/**
	 * Factory method used to construct a new {@link Timespan} ending on the given, required {@link LocalDate},
	 * with no timestamp.
	 *
	 * @param date {@link LocalDate} marking the end of the {@link Timespan}; must not be {@literal null}.
	 * @return a new {@link Timespan} ending on the given {@link LocalDate}.
	 * @throws IllegalArgumentException if the given {@link LocalDate} is {@literal null}.
	 * @see #ending(LocalDateTime)
	 * @see java.time.LocalDate
	 */
	public static @NotNull Timespan ending(@NotNull LocalDate date) {
		Assert.notNull(date, "End date is required");
		return ending(date.atTime(LocalTime.MAX));
	}

	/**
	 * Factory method used to construct a new {@link Timespan} ending on the given,
	 * required {@link LocalDateTime date and time}.
	 *
	 * @param dateTime {@link LocalDateTime} marking the end of the {@link Timespan}; must not be {@literal null}.
	 * @return a new {@link Timespan} ending on the given {@link LocalDateTime}.
	 * @throws IllegalArgumentException if the given {@link LocalDateTime} is {@literal null}.
	 * @see java.time.LocalDateTime
	 */
	public static @NotNull Timespan ending(@NotNull LocalDateTime dateTime) {
		Assert.notNull(dateTime, "End date and time are required");
		return new Timespan(null, dateTime);
	}

	/**
	 * Factory method used to construct a new {@link Timespan} ending at the given, required {@link LocalTime}
	 * of the current day.
	 * <p>
	 * The end of the {@link Timespan} is set to
	 * {@literal <Current Year>-<Current Month>-<Current Day of Month>-<Hour>-<Minute>-<Second>}.
	 *
	 * @param time {@link LocalTime} marking the end of the {@link Timespan}; must not be {@literal null}.
	 * @return a new {@link Timespan} ending at the given {@link LocalTime}.
	 * @throws IllegalArgumentException if the given {@link LocalTime} is {@literal null}.
	 * @see #ending(LocalDateTime)
	 * @see java.time.LocalTime
	 */
	public static @NotNull Timespan ending(@NotNull LocalTime time) {
		Assert.notNull(time, "End time is required");
		return ending(time.atDate(LocalDate.now()));
	}

	/**
	 * Factory method used to construct a new {@link Timespan} beginning the given {@link Year} and ending
	 * in a subsequent {@link Year}.
	 *
	 * @param beginning {@link Year} marking the beginning of the {@link Timespan}.
	 * @return a new {@link Timespan} with a {@link Year} timeframe.
	 * @throws IllegalArgumentException if the beginning or ending {@link Year} are {@literal null}
	 * or the {@link Year begin year} is after the {@link Year end year}.
	 * @see #from(YearMonth)
	 * @see java.time.Year
	 * @see WithTo
	 */
	@Dsl
	public static @NotNull WithTo from(@Nullable Year beginning) {
		return from(beginning != null ? beginning.atMonth(Month.JANUARY) : null);
	}

	/**
	 * Factory method used to construct a new {@link Timespan} beginning the given {@link YearMonth}
	 * and ending in a subsequent {@link YearMonth}.
	 *
	 * @param beginning {@link YearMonth} marking the beginning of the {@link Timespan}.
	 * @return a new {@link Timespan} with a {@link YearMonth} timeframe.
	 * @throws IllegalArgumentException if the beginning or ending {@link YearMonth} are {@literal null}
	 * or the {@link YearMonth begin year and month} are after the {@link YearMonth end year and month}.
	 * @see java.time.YearMonth
	 * @see #from(LocalDate)
	 * @see WithTo
	 */
	@Dsl
	public static @NotNull WithTo from(@Nullable YearMonth beginning) {
		return from(beginning != null ? beginning.atDay(1) : null);
	}

	/**
	 * Factory method used to construct a new {@link Timespan} beginning from the given {@link LocalDate}
	 * and ending on a subsequent {@link LocalDate}.
	 *
	 * @param beginning {@link LocalDate} marking the beginning of the {@link Timespan}; must not be {@literal null}.
	 * @return a new {@link Timespan} with a specific {@link LocalDate} timeframe.
	 * @throws IllegalArgumentException if the beginning or ending {@link LocalDate} are {@literal null}
	 * or the {@link LocalDate begin date} is after the {@link LocalDate end date}.
	 * @see #from(LocalDateTime)
	 * @see java.time.LocalDate
	 * @see WithTo
	 */
	@Dsl
	public static @NotNull WithTo from(@Nullable LocalDate beginning) {
		return from(beginning != null ? beginning.atStartOfDay() : null);
	}

	/**
	 * Factory method used to construct a new {@link Timespan} beginning from the given {@link LocalDateTime}
	 * and ending on a subsequent {@link LocalDateTime}.
	 *
	 * @param beginning {@link LocalDateTime} marking the beginning of the {@link Timespan}; must not be {@literal null}.
	 * @return a new {@link Timespan} with a specific {@link LocalDateTime} timeframe.
	 * @throws IllegalArgumentException if the beginning or ending {@link LocalDateTime} are {@literal null}
	 * or the {@link LocalDateTime begin date and time} is after the {@link LocalDateTime end date and time}.
	 * @see java.time.LocalDateTime
	 * @see WithTo
	 */
	@Dsl
	public static @NotNull WithTo from(@Nullable LocalDateTime beginning) {
		return new WithTo(beginning);
	}

	/**
	 * Factory method used to construct a new {@link Timespan} beginning at the given {@link LocalTime}
	 * and ending at a subsequent {@link LocalTime}.
	 *
	 * @param beginning {@link LocalTime} marking the beginning of the {@link Timespan}; must not be {@literal null}.
	 * @return a new {@link Timespan} with a specific {@link LocalTime} timeframe.
	 * @throws IllegalArgumentException if the beginning or ending {@link LocalTime} are {@literal null}
	 * or the {@link LocalTime begin time} is after the {@link LocalTime end time}.
	 * @see java.time.LocalDateTime
	 * @see WithTo
	 */
	@Dsl
	public static @NotNull WithTo from(@Nullable LocalTime beginning) {
		return from(beginning != null ? beginning.atDate(LocalDate.now()) : null);
	}

	/**
	 * Factory method used to construct a new {@link Timespan} that starts {@link LocalDateTime#now() now}
	 * and extends until the end of time.
	 *
	 * @return a new {@link Timespan} that starts {@link LocalDateTime#now() now} and extends until the end of time.
	 */
	public static @NotNull Timespan fromNow() {
		return new Timespan(LocalDateTime.now(), null);
	}

	/**
	 * Factory method used to construct a new {@link Timespan} that is {@literal infinite} across time, extending
	 * into the infinite past and into the infinite future.
	 *
	 * @return a new {@link Timespan} that is {@literal infinite} across time, extending into the infinite past
	 * and into the infinite future.
	 */
	public static @NotNull Timespan infinite() {
		return new Timespan(null, null);
	}

	/**
	 * Factory method used to construct a new {@link Timespan} that ends {@link LocalDateTime#now() now}
	 * and extends into the infinite past.
	 *
	 * @return a new {@link Timespan} that ends {@link LocalDateTime#now() now} and extends into the infinite past.
	 */
	public static @NotNull Timespan untilNow() {
		return new Timespan(null, LocalDateTime.now());
	}

	@Nullable
	private final LocalDateTime begin;

	@Nullable
	private final LocalDateTime end;

	/**
	 * Constructs a new {@link Timespan} between the given {@link LocalDateTime begin} and {@link LocalDateTime end}
	 * date/times.
	 *
	 * @param begin {@link LocalDateTime} declaring the {@literal beginning}, or start of the {@link Timespan};
	 * must not be {@literal null}.
	 * @param end {@link LocalDateTime} declaring the {@literal ending}, or start of the {@link Timespan};
	 * must not be {@literal null}.
	 * @throws IllegalArgumentException if the {@link LocalDateTime begin} is on or after the {@link LocalDateTime end}.
	 * @see java.time.LocalDateTime
	 */
	@SuppressWarnings("all")
	private Timespan(@Nullable LocalDateTime begin, @Nullable LocalDateTime end) {
		this.begin = begin;
		this.end = end;
	}

	/**
	 * Gets the {@link LocalDateTime beginning} of this {@link Timespan}.
	 *
	 * @return the {@link LocalDateTime beginning} of this {@link Timespan}; returns {@literal null}
	 * if the beginning of this {@link Timespan} is open-ended.
	 * @see #getOptionalBegin()
	 * @see #getEnd()
	 */
	public @Nullable LocalDateTime getBegin() {
		return this.begin;
	}

	/**
	 * Gets the {@link LocalDateTime end} of this {@link Timespan}.
	 *
	 * @return the {@link LocalDateTime end} of this {@link Timespan}; returns {@literal null}
	 * if the end of this {@link Timespan} is open-ended.
	 * @see #getOptionalEnd()
	 * @see #getBegin()
	 */
	public @Nullable LocalDateTime getEnd() {
		return this.end;
	}

	/**
	 * Gets an {@link Optional} {@link LocalDateTime beginning} to this {@link Timespan}.
	 *
	 * @return an {@link Optional} {@link LocalDateTime beginning} to this {@link Timespan}.
	 * @see java.util.Optional
	 * @see #getBegin()
	 */
	public Optional<LocalDateTime> getOptionalBegin() {
		return Optional.ofNullable(getBegin());
	}

	/**
	 * Gets an {@link Optional} {@link LocalDateTime ending} to this {@link Timespan}.
	 *
	 * @return an {@link Optional} {@link LocalDateTime ending} to this {@link Timespan}.
	 * @see java.util.Optional
	 * @see #getEnd()
	 */
	public Optional<LocalDateTime> getOptionalEnd() {
		return Optional.ofNullable(getEnd());
	}

	/**
	 * Determine whether the given {@link Year} is after this {@link Timespan}.
	 *
	 * @param year {@link Year} to compare with the {@link #getEnd() end} of this {@link Timespan}.
	 * @return a boolean value indicating whether the given {@link Year} is after this {@link Timespan}.
	 * @see #isAfter(YearMonth)
	 * @see java.time.Year
	 */
	public boolean isAfter(@NotNull Year year) {
		return year != null && isAfter(year.atMonth(Month.JANUARY));
	}

	/**
	 * Determine whether the given {@link YearMonth} is after this {@link Timespan}.
	 *
	 * @param yearMonth {@link YearMonth} to compare with the {@link #getEnd() end} of this {@link Timespan}.
	 * @return a boolean value indicating whether the given {@link YearMonth} is after this {@link Timespan}.
	 * @see #isAfter(LocalDate)
	 * @see java.time.YearMonth
	 */
	public boolean isAfter(@NotNull YearMonth yearMonth) {
		return yearMonth != null && isAfter(yearMonth.atDay(1));
	}

	/**
	 * Determine whether the given {@link LocalDate} is after this {@link Timespan}.
	 *
	 * @param date {@link LocalDate} to compare with the {@link #getEnd() end} of this {@link Timespan}.
	 * @return a boolean value indicating whether the given {@link LocalDate} is after this {@link Timespan}.
	 * @see #isAfter(LocalDateTime)
	 * @see java.time.LocalDate
	 */
	public boolean isAfter(@NotNull LocalDate date) {
		return date != null && isAfter(date.atStartOfDay());
	}

	/**
	 * Determine whether the given {@link LocalDateTime} is after this {@link Timespan}.
	 *
	 * @param dateTime {@link LocalDate} to compare with the {@link #getEnd() end} of this {@link Timespan}.
	 * @return a boolean value indicating whether the given {@link LocalDateTime} is after this {@link Timespan}.
	 * @see java.time.LocalDateTime
	 * @see #getOptionalEnd()
	 */
	public boolean isAfter(@NotNull LocalDateTime dateTime) {
		return dateTime != null && getOptionalEnd().map(dateTime::isAfter).orElse(false);
	}

	/**
	 * Determine whether the given {@link LocalTime} is after this {@link Timespan}.
	 *
	 * @param time {@link LocalTime} to compare with the {@link #getEnd() end} of this {@link Timespan}.
	 * @return a boolean value indicating whether the given {@link LocalTime} is after this {@link Timespan}.
	 * @see #isAfter(LocalDateTime)
	 * @see java.time.LocalTime
	 */
	public boolean isAfter(@NotNull LocalTime time) {
		return time != null && isAfter(time.atDate(LocalDate.now()));
	}

	// Determine whether the given year is before this Timespan
	public boolean isBefore(@NotNull Year year) {
		return year != null && isBefore(year.atMonth(Month.DECEMBER));
	}

	// Determine whether the given year/month is before this Timespan
	public boolean isBefore(@NotNull YearMonth yearMonth) {
		return yearMonth != null && isBefore(yearMonth.atEndOfMonth());
	}

	// Determine whether the given date occurs is this Timespan
	public boolean isBefore(@NotNull LocalDate date) {
		return date != null && isBefore(date.atTime(LocalTime.MAX));
	}

	// Determine whether the given date/time is before this Timespan
	public boolean isBefore(@NotNull LocalDateTime dateTime) {
		return dateTime != null && getOptionalBegin().map(dateTime::isBefore).orElse(false);
	}

	// Determine whether the given time occurs is this Timespan
	public boolean isBefore(@NotNull LocalTime time) {
		return time != null && isBefore(time.atDate(LocalDate.now()));
	}

	// Determine whether the entire year is during this Timespan
	public boolean isDuring(@NotNull Year year) {
		return year != null && isDuring(year.atMonth(Month.JANUARY)) && isDuring(year.atMonth(Month.DECEMBER));
	}

	// Determine whether the entire month of the year is during this Timespan
	public boolean isDuring(@NotNull YearMonth yearMonth) {
		return yearMonth != null && isDuring(yearMonth.atDay(1)) && isDuring(yearMonth.atEndOfMonth());
	}

	// Determine whether the entire day is during this Timespan
	public boolean isDuring(@NotNull LocalDate date) {
		return date != null && isDuring(date.atStartOfDay()) && isDuring(date.atTime(LocalTime.MAX));
	}

	// Determine whether the given date/time (instant in time) is during this Timespan
	public boolean isDuring(@NotNull LocalDateTime dateTime) {

		return dateTime != null
			&& getOptionalBegin().map(begin -> !dateTime.isBefore(begin)).orElse(true)
			&& getOptionalEnd().map(end -> !dateTime.isAfter(end)).orElse(true);
	}

	// Determine whether the given time occurs is this Timespan
	public boolean isDuring(@NotNull LocalTime time) {
		return time != null && isDuring(time.atDate(LocalDate.now()));
	}

	public boolean hasBeginning() {
		return getOptionalBegin().isPresent();
	}

	public boolean hasEnding() {
		return getOptionalEnd().isPresent();
	}

	public boolean hasNoBeginning() {
		return getOptionalBegin().isEmpty();
	}

	public boolean hasNoEnding() {
		return getOptionalEnd().isEmpty();
	}

	public boolean isAllEternity() {
		return hasNoBeginning() && hasNoEnding();
	}

	public boolean isFinite() {
		return hasBeginning() && hasEnding();
	}

	public boolean isInfinite() {
		return hasNoBeginning() || hasNoEnding();
	}

	public boolean isOpenEnded() {
		return hasNoBeginning() || hasNoEnding();
	}

	@SuppressWarnings("all")
	public boolean isContained(@NotNull Timespan timespan) {

		boolean containedBeginning = this.hasNoBeginning() || isDuring(timespan.getBegin());
		boolean containedEnding = this.hasNoEnding() || isDuring(timespan.getEnd());
		boolean allTrue = !timespan.isAllEternity() && containedBeginning && containedEnding;

		return timespan != null && (this.isAllEternity() || allTrue);
	}

	public boolean isContainedBy(@NotNull Timespan timespan) {
		return timespan != null && timespan.isContained(this);
	}

	public boolean isOverlapping(@NotNull Timespan timespan) {

		boolean anyTrue = this.isAllEternity() || timespan.isAllEternity()
			|| (timespan.hasNoBeginning() && isDuring(timespan.getEnd()))
			|| (timespan.hasNoEnding() && isDuring(timespan.getBegin()));

		return timespan != null && anyTrue;
	}

	public boolean isNotOverlapping(@NotNull Timespan timespan) {
		return !isOverlapping(timespan);
	}

	@Override
	@SuppressWarnings("all")
	public int compareTo(@NotNull Timespan that) {

		return this.getBegin() == null ? -1
			: that.getBegin() == null ? 1
			: this.getBegin().isBefore(that.getBegin()) ? -1
			: that.getEnd() == null ? -1
			: this.getEnd() == null ? 1
			: this.getEnd().isBefore(that.getEnd()) ? -1
			: 0;
	}

	@Override
	public String toString() {

		String beginningString = getOptionalBegin()
			.map(begin -> begin.format(DATE_TIME_FORMATTER))
			.orElse(BEGINNING_OF_TIME);

		String endingString = getOptionalEnd()
			.map(end -> end.format(DATE_TIME_FORMATTER))
			.orElse(END_OF_TIME);

		return TIMESPAN_TO_STRING.formatted(beginningString, endingString);
	}

	/**
	 * Builder with a {@link FluentApi} used to construct a new {@link Timespan} within a given {@literal timeframe}.
	 */
	@FluentApi
	public static class WithTo {

		private final LocalDateTime beginning;

		protected WithTo(@NotNull LocalDateTime beginning) {
			this.beginning = ObjectUtils.requireObject(beginning, "Begin date and time are required");
		}

		protected @NotNull LocalDateTime getBeginning() {
			return this.beginning;
		}

		@Dsl
		public @NotNull Builder to(@Nullable Year ending) {
			return to(ending != null ? ending.atMonth(Month.DECEMBER) : null);
		}

		@Dsl
		public @NotNull Builder to(@Nullable YearMonth ending) {
			return to(ending != null ? ending.atEndOfMonth() : null);
		}

		@Dsl
		public @NotNull Builder to(@Nullable LocalDate ending) {
			return to(ending != null ? ending.atTime(LocalTime.MAX) : null);
		}

		@Dsl
		public @NotNull Builder to(@Nullable LocalDateTime ending) {
			return new Builder(getBeginning(), ending);
		}

		@Dsl
		public @NotNull Builder to(@Nullable LocalTime ending) {
			return to(ending != null ? ending.atDate(LocalDate.now()) : null);
		}
	}

	/**
	 * Elements {@link Builder} used to build a new {@link Timespan} within a given {@literal timeframe}.
	 *
	 * @see org.cp.elements.lang.Builder
	 */
	public static class Builder implements org.cp.elements.lang.Builder<Timespan> {

		private final LocalDateTime beginning;
		private final LocalDateTime ending;

		protected Builder(@NotNull LocalDateTime beginning, @NotNull LocalDateTime ending) {
			this.beginning = ObjectUtils.requireObject(beginning, "Begin date and time are required");
			this.ending = ObjectUtils.requireObject(ending, "End date and time are required");
		}

		protected @NotNull LocalDateTime getBeginning() {
			return this.beginning;
		}

		protected @NotNull LocalDateTime getEnding() {
			return this.ending;
		}

		@Override
		public Timespan build() {

			Assert.isFalse(getBeginning().isAfter(getEnding()),
				() -> "Beginning [%s] of Timespan must not be after the ending [%s]"
					.formatted(getBeginning().format(DATE_TIME_FORMATTER), getEnding()).formatted(DATE_TIME_FORMATTER));

			return new Timespan(getBeginning(), getEnding());
		}
	}
}
