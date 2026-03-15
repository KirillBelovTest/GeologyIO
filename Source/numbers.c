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

static void to_ibm_32_float(double *inputNumbers, uint8_t *outputBytes, mint count);
static void from_ibm_32_float(uint8_t *inputBytes, double *outputNumbers, mint count);

/**
 * Converts array of double precision numbers to IBM 32-bit float format
 * Implements IBM Hexadecimal Floating Point (HFP) specification:
 * - 1 bit sign, 7 bits exponent (base 16, bias 64), 24 bits mantissa
 * - Normalized mantissa in range [1/16, 1)
 * - Formula: (-1)^sign × mantissa × 16^(exponent - 64)
 * @param inputNumbers - input array of double values
 * @param outputBytes - output byte array (must be size count * 4)
 * @param count - number of elements to convert
 */
static void to_ibm_32_float(double *inputNumbers, uint8_t *outputBytes, mint count) {
    const double log16 = log(16.0);

    for (mint i = 0; i < count; ++i) {
        double number = inputNumbers[i];
        size_t byteIndex = i * IBM_FLOAT_SIZE;
        double abs_number = fabs(number);

        if (abs_number == 0.0 || abs_number < POW10_M75 || abs_number > POW10_75) {
            memset(&outputBytes[byteIndex], 0, IBM_FLOAT_SIZE);
            continue;
        }

        int exponent = (int)floor(log(abs_number) / log16);
        double mantissa = abs_number / pow(16.0, exponent);

        if (mantissa >= 1.0) {
            mantissa /= 16.0;
            exponent++;
        }

        uint8_t sign_bit = (number < 0) ? SIGN_MASK : 0;
        uint8_t firstbyte = (exponent + EXPONENT_BIAS) | sign_bit;

        uint32_t fraction = (uint32_t)(mantissa * POW256_3 + 0.5);

        outputBytes[byteIndex] = firstbyte;
        outputBytes[byteIndex + 1] = (fraction >> 16) & 0xFF;
        outputBytes[byteIndex + 2] = (fraction >> 8) & 0xFF;
        outputBytes[byteIndex + 3] = fraction & 0xFF;
    }
}

/**
 * Converts IBM 32-bit float format to double precision numbers
 * Implements reverse conversion from IBM HFP to IEEE double
 * @param inputBytes - input byte array in IBM float format
 * @param outputNumbers - output array of double values (must be size count)
 * @param count - number of elements to convert
 */
static void from_ibm_32_float(uint8_t *inputBytes, double *outputNumbers, mint count) {
    for (mint i = 0; i < count; ++i) {
        size_t byteIndex = i * IBM_FLOAT_SIZE;
        uint8_t b1 = inputBytes[byteIndex];
        uint8_t b2 = inputBytes[byteIndex + 1];
        uint8_t b3 = inputBytes[byteIndex + 2];
        uint8_t b4 = inputBytes[byteIndex + 3];

        if (b1 == 0 && b2 == 0 && b3 == 0 && b4 == 0) {
            outputNumbers[i] = 0.0;
            continue;
        }

        double sign = (b1 & SIGN_MASK) ? -1.0 : 1.0;
        int exponent = (b1 & EXPONENT_MASK) - EXPONENT_BIAS;
        double fraction = (b2 * 65536.0 + b3 * 256.0 + b4) / POW256_3;

        outputNumbers[i] = sign * fraction * pow(16.0, exponent);
    }
}

DLLEXPORT mint WolframLibrary_getVersion() {
    return WolframLibraryVersion;
}

DLLEXPORT int WolframLibrary_initialize(WolframLibraryData libData) {
    return LIBRARY_NO_ERROR;
}

DLLEXPORT void WolframLibrary_uninitialize(WolframLibraryData libData) {
    (void)libData;
}

/**
 * Exported Wolfram Library function to convert double array to IBM 32-bit float
 * Expects: {numbersArray, numbersLength}
 * Returns: Byte array of IBM float values
 */
DLLEXPORT int toIBMFloat32(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    if (Argc != 2) {
        return LIBRARY_FUNCTION_ERROR;
    }

    MNumericArray numbersArray = MArgument_getMNumericArray(Args[0]);
    if (numbersArray == NULL) {
        return LIBRARY_TYPE_ERROR;
    }

    const mint numbersLen = MArgument_getInteger(Args[1]);
    if (numbersLen <= 0) {
        return LIBRARY_DIMENSION_ERROR;
    }

    double *numbers = (double *)libData->numericarrayLibraryFunctions->MNumericArray_getData(numbersArray);
    if (numbers == NULL) {
        return LIBRARY_FUNCTION_ERROR;
    }

    MNumericArray bytesArray;
    const mint bytesLen = numbersLen * IBM_FLOAT_SIZE;

    int err = libData->numericarrayLibraryFunctions->MNumericArray_new(
        MNumericArray_Type_UBit8, 1, &bytesLen, &bytesArray
    );
    if (err != LIBRARY_NO_ERROR) {
        return err;
    }

    uint8_t *bytes = (uint8_t*)libData->numericarrayLibraryFunctions->MNumericArray_getData(bytesArray);
    if (bytes == NULL) {
        libData->numericarrayLibraryFunctions->MNumericArray_free(bytesArray);
        return LIBRARY_FUNCTION_ERROR;
    }

    to_ibm_32_float(numbers, bytes, numbersLen);

    MArgument_setMNumericArray(Res, bytesArray);
    return LIBRARY_NO_ERROR;
}

/**
 * Exported Wolfram Library function to convert IBM 32-bit float to double array
 * Expects: {bytesArray, bytesLength}
 * Returns: Double array of converted values
 */
DLLEXPORT int fromIBMFloat32(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    if (Argc != 2) {
        return LIBRARY_FUNCTION_ERROR;
    }

    MNumericArray bytesArray = MArgument_getMNumericArray(Args[0]);
    if (bytesArray == NULL) {
        return LIBRARY_TYPE_ERROR;
    }

    const mint bytesLen = MArgument_getInteger(Args[1]);
    if (bytesLen <= 0 || bytesLen % IBM_FLOAT_SIZE != 0) {
        return LIBRARY_DIMENSION_ERROR;
    }

    uint8_t *bytes = (uint8_t *)libData->numericarrayLibraryFunctions->MNumericArray_getData(bytesArray);
    if (bytes == NULL) {
        return LIBRARY_FUNCTION_ERROR;
    }

    MTensor numbersTensor;
    const mint numbersLen = bytesLen / IBM_FLOAT_SIZE;

    int err = libData->MTensor_new(MType_Real, 1, &numbersLen, &numbersTensor);
    if (err != LIBRARY_NO_ERROR) {
        return err;
    }

    double *numbers = (double*)libData->MTensor_getRealData(numbersTensor);
    if (numbers == NULL) {
        libData->MTensor_free(numbersTensor);
        return LIBRARY_FUNCTION_ERROR;
    }

    from_ibm_32_float(bytes, numbers, numbersLen);

    MArgument_setMTensor(Res, numbersTensor);
    return LIBRARY_NO_ERROR;
}