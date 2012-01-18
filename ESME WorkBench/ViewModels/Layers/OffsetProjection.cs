using System;
using ThinkGeo.MapSuite.Core;

namespace ESMEWorkBench.ViewModels.Layers
{
    //This class, inheriting from Projection, applies the very simple projection of offsetting all the points
    //360 degrees to the left.

    class OffsetProjection: Projection, IDisposable
    {
        protected override Vertex[] ConvertToExternalProjectionCore(double[] x, double[] y)
        {

            var vertices = new Vertex[x.Length];

            for (var i = 0; i < vertices.Length; i++)
            {
                vertices[i] = new Vertex(x[i] - 360, y[i]);
            }
            return vertices;
        }

        protected override Vertex[] ConvertToInternalProjectionCore(double[] x, double[] y)
        {
            var vertices = new Vertex[x.Length];

            for (var i = 0; i < vertices.Length; i++)
            {
                vertices[i] = new Vertex(x[i] + 360, y[i]);
            }
            return vertices;
        }

        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        private void Dispose(bool disposing)
        {
            Close();
        }

    }
}
