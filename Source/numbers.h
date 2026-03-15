#ifndef NUMBERS_H
#define NUMBERS_H

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>
#include <stdbool.h>

#include "WolframLibrary.h"
#include "WolframNumericArrayLibrary.h"

#define IBM_FLOAT_SIZE 4
#define EXPONENT_BIAS 64
#define SIGN_MASK 0x80
#define EXPONENT_MASK 0x7F
#define POW256_3 16777216.0
#define POW10_M75 1e-75
#define POW10_75 1e75

/**
 * Converts array of double precision numbers to IBM 32-bit float format
 * Implements IBM Hexadecimal Floating Point (HFP) specification:
 * - 1 bit sign, 7 bits exponent (base 16, bias 64), 24 bits mantissa
 * - Normalized mantissa in range [1/16, 1)
 * - Formula: (-1)^sign × mantissa × 16^(exponent - 64)
 * @param numbers - input array of double values
 * @param bytes - output byte array (must be size count * 4)
 * @param count - number of elements to convert
 */
void ibm_32_double_to_byte_array(double *numbers, uint8_t *bytes, mint count);

/**
 * Converts IBM 32-bit float format to double precision numbers
 * Implements reverse conversion from IBM HFP to IEEE double
 * @param inputBytes - input byte array in IBM float format
 * @param outputNumbers - output array of double values (must be size count)
 * @param count - number of elements to convert
 */
void ibm_32_byte_array_to_double(uint8_t *bytes, double *numbers, mint count);


/**
 * Exported Wolfram Library function to convert double array to IBM 32-bit float
 * Expects: {numbersList, numbersLength}
 * Returns: Byte array of IBM float values
 */
DLLEXPORT int ibm32RealToByteArray(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

/**
 * Exported Wolfram Library function to convert IBM 32-bit float to double array
 * Expects: {bytesArray, bytesLength}
 * Returns: Double array of converted values
 */
DLLEXPORT int ibm32ByteArrayToReal(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res);

#endif