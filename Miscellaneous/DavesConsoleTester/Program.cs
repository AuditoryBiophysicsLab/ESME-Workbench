using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using ESME.Environment;
using ShoNS.Hosting;

namespace DavesConsoleTester
{
    class Program
    {
        static void Main(string[] args)
        {
            // create the Embedded Sho
            Console.Write("Starting Embedded Sho...");
            // the path argument below should point to what "SHODIR" evaluates
            // to in Sho
            var es = new EmbeddedSho(@"c:\Program Files (x86)\Sho 2.0 for .NET 4");
            Console.Write("done.\n");
            // es.CacheShoOutput will store the output from Sho so you can show
            // it in a form, etc;
            // if you don't call this the default is to print the output to
            //the console.
            es.CacheShoOutput();
            // do some Sho stuff
            es.ExecutePython("a = rand(6,10)");
            es.ExecutePython("foo = a[0,0]");
            // print information from the class
            //es.ExecutePython("print tc.info");
            // print cached output from Sho
            Console.WriteLine("{0}", es.GetOutputText());
            es.ExecutePython("plot(a[0, :], a[1, :])");
            es.ExecutePython("plot(a[2, :], a[3, :])");
            es.ExecutePython("plot(a[4, :], a[5, :])");
            // get the result in res so we can bring it back to C#
            //es.ExecutePython("res = tc.info");
            es.ExecutePython("for x in range(10): print x");
            // print cached output text. This can be called as often as
            //you wish; the text is flushed each time.
            Console.WriteLine("{0}", es.GetOutputText());
            // get back and print Sho values
            // note that GetPythonVariable will return an Object, which we
            //can cast to a known type
            Console.WriteLine("foo: {0}", es.GetPythonVariable("foo"));
            // alternatively, we can use GetPythonVariableAs<type>, which
            //will cast it to the type in one call.
            //Console.WriteLine("res: {0}",
            //es.GetPythonVariableAs<String>("res"));
        }
    }
}
