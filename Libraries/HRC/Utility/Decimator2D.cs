﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;

namespace HRC.Utility
{
    public static class Decimator2D
    {
        public static float[,] Decimate(float[,] source, int outputWidth, int outputHeight)
        {
            if (outputWidth > source.GetLength(0) || outputHeight > source.GetLength(1)) throw new DataMisalignedException("Cannot decimate to a larger area.");
            var result = new float[outputWidth, outputHeight];
            var sourceWidth = source.GetLength(0);
            var sourceHeight = source.GetLength(1);
            var widthStep = (double)sourceWidth / outputWidth;
            var heightStep = (double)sourceHeight / outputHeight;

            for (var widthIndex = 0; widthIndex < outputWidth; widthIndex++)
            {
                var sourceWidthIndex = (int)((widthIndex*widthStep)+(widthStep/2));
                for (var heightIndex = 0; heightIndex < outputHeight; heightIndex++)
                {
                    var sourceHeightIndex = (int)((heightIndex*heightStep)+(heightStep/2));
                    result[widthIndex, heightIndex] = source[sourceWidthIndex, sourceHeightIndex];
                }
            }

            return result;
        }
    }
}
