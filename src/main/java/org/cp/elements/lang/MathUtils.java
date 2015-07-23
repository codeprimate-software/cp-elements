/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * 
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * 
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * 
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * 
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.lang;

import java.math.BigInteger;

/**
 * The MathUtils class is a utility class encapsulating common mathematical operations and calculations.
 * 
 * @author John J. Blum
 * @see java.lang.Math
 * @link http://www.math.com
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class MathUtils {

  public static final BigInteger NEGATIVE_ONE = BigInteger.ONE.negate();
  public static final BigInteger TWO = BigInteger.valueOf(2);

  protected static final String NUMBER_LESS_THAN_ZERO_ERROR_MESSAGE = "value (%1$d) must be greater than equal to 0";

  /**
   * Calculates the area of a circle.
   *
   * @param radius a double value indicating the radius of the circle.
   * @return the area of a circle given the radius.
   * @see #ellipseArea(double, double)
   */
  public static double circleArea(final double radius) {
    return ellipseArea(radius, radius);
  }

  /**
   * Calculates the circumference of a circle.
   *
   * @param radius a double value indicating the radius of the circle.
   * @return the circumference of a circle given the radius.
   * @see java.lang.Math#PI
   */
  public static double circleCircumference(final double radius) {
    return (2.0d * Math.PI * radius);
  }

  /**
   * Calculates the diameter of a circle.
   *
   * @param radius a double value indicating the radius of the circle.
   * @return the diameter of a circle given the radius.
   */
  public static double circleDiameter(final double radius) {
    return (2.0d * radius);
  }

  /**
   * Calculates the volume of a cone.
   *
   * @param radius a double value indicating the radius of the cone's base.
   * @param height a double value indicating the height of the cone.
   * @return the volume of a cone given the radius and height.
   * @see java.lang.Math#PI
   * @see java.lang.Math#pow(double, double)
   */
  public static double coneVolume(final double radius, final double height) {
    return ((Math.PI * Math.pow(radius, 2) * height) / 3.0d);
  }

  /**
   * Calculates the surface area of a cube.
   *
   * @param side a double value indicating the side length of the cube.
   * @return the surface area of a cube given a side.
   * @see #squareArea(double)
   */
  public static double cubeSurfaceArea(final double side) {
    return (6.0d * squareArea(side));
  }

  /**
   * Calculates the volume of a cube.
   *
   * @param side a double value indicating the length of the cube's side.
   * @return the volume of a cube given the length of a side.
   * @see java.lang.Math#pow(double, double)
   */
  public static double cubeVolume(final double side) {
    return Math.pow(side, 3);
  }

  /**
   * Calculates the surface area of a cylinder.
   *
   * @param radius a double value indicating the radius of the cylinder.
   * @param height a double value indicating the height of the cylinder.
   * @return the surface area of a cylinder given the radius and height.
   * @see java.lang.Math#PI
   * @see java.lang.Math#pow(double, double)
   */
  public static double cylinderSurfaceArea(final double radius, final double height) {
    return ((2.0d * Math.PI * Math.pow(radius, 2)) + (2.0d * Math.PI * radius * height));
  }

  /**
   * Calculates the volume of a cylinder.
   *
   * @param radius a double value indicating the radius of the cylinder's end(s).
   * @param height a double value indicating the height of the cylinder.
   * @return the volume of a cylinder given the radius and height.
   * @see java.lang.Math#PI
   * @see java.lang.Math#pow(double, double)
   */
  public static double cylinderVolume(final double radius, final double height) {
    return (Math.PI * Math.pow(radius, 2) * height);
  }

  /**
   * Calculates the area of an ellipse.
   *
   * @param radiusLength a double value indicating the radius length of the ellipse (x axis).
   * @param radiusHeight a double value indicating the radius height of the ellipse (y axis).
   * @return the area of an ellipse given the radius length and height.
   * @see java.lang.Math#PI
   */
  public static double ellipseArea(final double radiusLength, final double radiusHeight) {
    return (Math.PI * radiusLength * radiusHeight);
  }

  /**
   * Calculates the volume of an ellipsoid.
   *
   * @param radiusLength a double value indicating the radius length of the ellipsoid (x axis).
   * @param radiusHeight a double value indicating the radius height of the ellipsoid (y axis).
   * @param radiusWidth a double value indicating the radius width of the ellipsoid (z axis).
   * @return the volume of the ellipsoid given the radius length, height and width.
   * @see java.lang.Math#PI
   */
  public static double ellipsoidVolume(final double radiusLength, final double radiusHeight, final double radiusWidth) {
    return ((4.0d * Math.PI * radiusLength * radiusHeight * radiusWidth) / 3.0d);
  }

  /**
   * Calculates the area of a equilateral triangle.
   *
   * @param side a double value indicating the length of the equilateral triangle's side.
   * @return the area of a equilateral triangle given the length of a side.
   * @see java.lang.Math#pow(double, double)
   * @see java.lang.Math#sqrt(double)
   */
  public static double equilateralTriangleArea(final double side) {
    return ((Math.sqrt(3.0d) / 4.0d) * Math.pow(side, 2));
  }

  /**
   * Calculates the factorial of the given number using an iterative algorithm and BigInteger value type
   * to avoid a StackOverflowException and numeric overflow, respectively.
   *
   * @param value an Integer value used to compute the factorial.
   * @return the factorial of the given number.
   * @throws java.lang.IllegalArgumentException if the number value is null or less than 0.
   * @see java.math.BigInteger
   */
  public static BigInteger factorial(BigInteger value) {
    Assert.notNull(value, "value must not be null");
    Assert.isTrue(value.compareTo(BigInteger.ZERO) >= 0, String.format(NUMBER_LESS_THAN_ZERO_ERROR_MESSAGE, value));

    if (value.compareTo(TWO) <= 0) {
      return (value.equals(TWO) ? TWO : BigInteger.ONE);
    }

    BigInteger result = value;

    for (value = result.add(NEGATIVE_ONE) ; value.compareTo(BigInteger.ONE) > 0; value = value.add(NEGATIVE_ONE)) {
      result = result.multiply(value);
    }

    return result;
  }

  /**
   * Gets the Fibonacci number at position n in the Fibonacci Sequence.
   *
   * @param n an integer value indicating the nth number in the Fibonacci Sequence.
   * @return the nth number in the Fibonacci Sequence.
   * @throws IllegalArgumentException if the position (n) is less than 1.
   * @see #fibonacciSequence(int)
   */
  public static int fibonacciNumber(final int n) {
    return fibonacciSequence(n)[n - 1];
  }

  /**
   * Calculates the Fibonacci Sequence to the nth position.
   *
   * @param n an integer value indicating the position of the nth element in the Fibonacci Sequence.
   * @return an integer array containing n elements of the Fibonacci Sequence.
   * @throws IllegalArgumentException if the position (n) is less than 1.
   */
  public static int[] fibonacciSequence(final int n) {
    Assert.argument(n > 0, "The number of elements from the Fibonacci Sequence to calculate must be greater than equal to 0!");

    int[] fibonacciNumbers = new int[n];

    for (int position = 0; position < n; position++) {
      if (position == 0) {
        fibonacciNumbers[position] = 0;
      }
      else if (position < 2) {
        fibonacciNumbers[position] = 1;
      }
      else {
        fibonacciNumbers[position] = (fibonacciNumbers[position - 1] + fibonacciNumbers[position - 2]);
      }
    }

    return fibonacciNumbers;
  }

  /**
   * Calculates the volume of an irregular prism.
   *
   * @param base a double value indicating the prism's base (width).
   * @param height a double value indicating the prism's height.
   * @return the volume of an irregular prism given the base and height.
   */
  public static double irregularPrismVolume(final double base, final double height) {
    return (base * height);
  }

  /**
   * Determines the maximum numerical value in an array of values.
   *
   * @param values an array of numerical values from which to determine the maximum value.
   * @return the maximum numerical value in the array of numerical values.  Returns Double.NaN
   * if the array of values is null.
   * @see java.lang.Double#NaN
   * @see java.lang.Math#max(double, double)
   */
  @NullSafe
  public static double max(final double... values) {
    double maxValue = Double.NaN;

    if (values != null) {
      for (double value : values) {
        maxValue = (Double.isNaN(maxValue) ? value : Math.max(maxValue, value));
      }
    }

    return maxValue;
  }

  /**
   * Determines the minimum numerical value in an array of values.
   *
   * @param values an array of numerical values from which to determine the minimum value.
   * @return the minimum numerical value in the array of numerical values.  Returns Double.NaN
   * if the array of values is null.
   * @see java.lang.Double#NaN
   * @see java.lang.Math#min(double, double)
   */
  @NullSafe
  public static double min(final double... values) {
    double minValue = Double.NaN;

    if (values != null) {
      for (double value : values) {
        minValue = (Double.isNaN(minValue) ? value : Math.min(minValue, value));
      }
    }

    return minValue;
  }

  /**
   * Multiplies the array of numbers.
   *
   * @param numbers the array of integer values to multiply.
   * @return the array of integer numbers multiplied together.
   */
  @NullSafe
  public static int multiply(final int... numbers) {
    int result = 0;

    if (numbers != null) {
      result = (numbers.length > 0 ? 1 : 0);
      for (int number : numbers) {
        result *= number;
      }
    }

    return result;
  }

  /**
   * Calculates the area of a parallelogram.
   *
   * @param base a double value indicating the base length of the parallelogram.
   * @param height a double value indicating the height of the parallelogram.
   * @return the area of a parallelogram given the base length and height.
   * @see #rectangleArea(double, double)
   */
  public static double parallelogramArea(final double base, final double height) {
    return rectangleArea(base, height);
  }

  /**
   * Calculates the volume of a pyramid.
   *
   * @param base a double value indicating the base length of the pyramid.
   * @param height a double value indicating the height of the pyramid.
   * @return the volume of a pyramid given the base length and height.
   */
  public static double pyramidVolume(final double base, final double height) {
    return ((base * height) / 3.0d);
  }

  /**
   * Calculates the Pythagorean Theorem (c2 = a2 + b2).
   *
   * @param a double value operand.
   * @param b double value operand.
   * @return the value for c using the Pythagorean Theorem
   * @see java.lang.Math#pow(double, double)
   * @see java.lang.Math#sqrt(double)
   */
  public static double pythagoreanTheorem(final double a, final double b) {
    return Math.sqrt(Math.pow(a, 2) + Math.pow(b, 2));
  }

  /**
   * Calculates the area of a rectangle.
   *
   * @param length a double value indicating the length of the rectangle.
   * @param height a double value indicating the height of the rectangle.
   * @return the area of a rectangle given the length and height.
   */
  public static double rectangleArea(final double length, final double height) {
    return (length * height);
  }

  /**
   * Calculates the surface area of a rectangular prism;
   *
   * @param length a double value indicating the length of the rectangular prism (x axis).
   * @param height a double value indicating the height of the rectangular prism (y axis).
   * @param width a double value indicating the width of the rectangular prism (z axis).
   * @return the surface area of a rectangular prism given the length, height and width of the sides.
   */
  public static double rectangularPrismSurfaceArea(final double length, final double height, final double width) {
    return ((2 * length * height) + (2 * length * width) + (2 * height * width));
  }

  /**
   * Calculates the volume of a rectangular prism.
   *
   * @param length a double value indicating the length of the rectangular prism (x axis).
   * @param height a double value indicating the height of the rectangular prism (y axis).
   * @param width a double value indicating the width of the rectangular prism (z axis).
   * @return the volume of a rectangular prism given the length, height and width.
   */
  public static double rectangularPrismVolume(final double length, final double height, final double width) {
    return (length * height * width);
  }

  /**
   * Rounds the specified double value to the nearest tenth.
   *
   * @param value the double value to round to the nearest tenth.
   * @return the double value rounded to the nearest tenth.
   * @see java.lang.Math#round(double)
   */
  public static double roundToNearestTenth(double value) {
    value *= 10.0d;
    value = Math.round(value);
    value /= 10.0d;
    return value;
  }

  /**
   * Calculates the surface area of a sphere.
   *
   * @param radius a double value indicating the radius of the sphere.
   * @return the surface area of a sphere given a radius
   * @see java.lang.Math#PI
   * @see java.lang.Math#pow(double, double)
   */
  public static double sphereSurfaceArea(final double radius) {
    return (4.0d * Math.PI * Math.pow(radius, 2));
  }

  /**
   * Calculates the volume of a sphere.
   *
   * @param radius a double value indicating the radius of the sphere.
   * @return the volume of a sphere given the radius.
   * @see java.lang.Math#PI
   * @see java.lang.Math#pow(double, double)
   */
  public static double sphereVolume(final double radius) {
    return ((4.0d * Math.PI * Math.pow(radius, 3)) / 3.0d);
  }

  /**
   * Calculates the area of a square.
   *
   * @param side a double value indicating the length of the square's side.
   * @return the area of a square given the length of a side.
   * @see #rectangleArea(double, double)
   */
  public static double squareArea(final double side) {
    return rectangleArea(side, side);
  }

  /**
   * Calculates the sum of all integer values in the array.
   *
   * @param numbers an array of integer values to sum up.
   * @return the sum of all integer values in the array.
   */
  @NullSafe
  public static int sum(final int... numbers) {
    int sum = 0;

    if (numbers != null) {
      for (int number : numbers) {
        sum += number;
      }
    }

    return sum;
  }

  /**
   * Calculates the area of a trapezoid.
   *
   * @param height a double value indicating the height of the trapezoid.
   * @param baseOne a double value indicating the length of one of the trapezoid's bases.
   * @param baseTwo a double value indicating the length of the other trapezoid's bases.
   * @return the area of a trapezoid given the height and length of the 2 bases.
   */
  public static double trapezoidArea(final double height, final double baseOne, final double baseTwo) {
    return ((height / 2.0d) * (baseOne + baseTwo));
  }

  /**
   * Calculates the area of a triangle.
   *
   * @param base a double value indicating the length of the triangle's base.
   * @param height a double value indicating the height of one of the triangle's sides.
   * @return the area of a triangle given the base length and height.
   */
  public static double triangleArea(final double base, final double height) {
    return ((base * height) / 2.0d);
  }

}
