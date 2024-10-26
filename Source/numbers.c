#include <stdio.h>
#include <stdlib.h>

#include <stdint.h>
#include <math.h>

#include "WolframLibrary.h"
#include "WolframNumericArrayLibrary.h"

DLLEXPORT mint WolframLibrary_getVersion() {
    return WolframLibraryVersion;
}

DLLEXPORT int WolframLibrary_initialize(WolframLibraryData libData) {
    return LIBRARY_NO_ERROR;
}

DLLEXPORT void WolframLibrary_uninitialize(WolframLibraryData libData) {
    return;
}

DLLEXPORT int toIBMFloat32(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    //input args
    MNumericArray numbersArray = MArgument_getMNumericArray(Args[0]);
    const mint numbersLen = MArgument_getInteger(Args[1]);
    double *numbers = (double *)libData->numericarrayLibraryFunctions->MNumericArray_getData(numbersArray);

    //result lst of bytes
    MNumericArray bytesArray;
    const mint bytesLen = numbersLen * 4;
    libData->numericarrayLibraryFunctions->MNumericArray_new(MNumericArray_Type_UBit8, 1, &bytesLen, &bytesArray);
    uint8_t *bytes = (uint8_t*)libData->numericarrayLibraryFunctions->MNumericArray_getData(bytesArray);
    
    //consts
    const double log16 = log(16.0);
    const double pow256_3 = pow(256.0, 3.0);
    const double pow10_m75 = pow(10.0, -75.0); 
    const double pow10_75 = pow(10.0, 75.0); 

    for (size_t i = 0; i < numbersLen; ++i) {
        double number = numbers[i];
        size_t byteIndex = i * 4;
        double abs_number = fabs(number);

        //check that number in valid range
        if (abs_number < pow10_m75 || abs_number > pow10_75) {
            bytes[byteIndex] = 0; 
            bytes[byteIndex + 1] = 0; 
            bytes[byteIndex + 2] = 0; 
            bytes[byteIndex + 3] = 0; 
            continue;
        }; 

        int8_t rsign = number < 0 ? 1 : 0;

        int8_t e = (int8_t)ceil(log(abs_number) / log16);
        int8_t firstbyte = e + 64 + rsign * 128;

        long fract = (long)ceil(pow256_3 * abs_number / pow(16.0, e));

        bytes[byteIndex] = firstbyte;
        bytes[byteIndex + 1] = (fract >> 16) & 0xFF;
        bytes[byteIndex + 2] = (fract >> 8) & 0xFF;
        bytes[byteIndex + 3] = fract & 0xFF;
    }
    
    //result
    MArgument_setMNumericArray(Res, bytesArray);
    return LIBRARY_NO_ERROR;
}

DLLEXPORT int fromIBMFloat32(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
    //input args
    MNumericArray bytesArray = MArgument_getMNumericArray(Args[0]);
    const mint bytesLen = MArgument_getInteger(Args[1]);
    uint8_t *bytes = (uint8_t *)libData->numericarrayLibraryFunctions->MNumericArray_getData(bytesArray);

    //result - numbers list
    MTensor numbersTensor;
    const mint numbersLen = bytesLen / 4;
    libData->MTensor_new(MType_Real, 1, &numbersLen, &numbersTensor);
    double *numbers = (double*)libData->MTensor_getRealData(numbersTensor);

    //constants
    double pow_256_3 = pow(256.0, 3.0); 
    double pow_256_2 = pow(256.0, 2.0); 
    
    //temp vars
    double sign;
    double expp; 
    double fract; 

    uint8_t b1; 

    for (size_t i = 0; i < bytesLen; i = i + 4) {
        b1 = bytes[i]; 
        
        //sign of the number
        sign = bytes[i] < 128 ? 1.0 : -1.0;

        //16th exp
        expp = pow(16.0, (double)((b1 - 64) & 127)); 

        fract = (
            (double)bytes[i+1] * pow_256_2 + 
            (double)bytes[i+2] * 256.0 + 
            (double)bytes[i+3]
        ) / pow_256_3; 

        if (fract == 0.0) fract = 1.0; 
        if (b1 == 0 && bytes[i+1] == 0 && bytes[i+2] == 0 && bytes[i+3] == 0) expp = 0.0; 

        numbers[i/4] = sign * expp * fract; 
    }
    
    //result
    MArgument_setMTensor(Res, numbersTensor);
    return LIBRARY_NO_ERROR;
}
