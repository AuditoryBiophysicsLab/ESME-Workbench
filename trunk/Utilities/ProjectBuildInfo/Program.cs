using System;
using System.CodeDom;
using System.CodeDom.Compiler;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using Microsoft.CSharp;

namespace ProjectBuildInfo
{
    internal class Program
    {
        static int Main(string[] args)
        {
            string namespaceName = null;
            string className = null;
            string outputFilename = null;
            string svnVersionDirectory = null;
            string svnVersionString = null;
            for (var i = 0; i < args.Length; i++)
            {
                switch (args[i])
                {
                    case "-namespace":
                        namespaceName = args[++i];
                        break;
                    case "-class":
                        className = args[++i];
                        break;
                    case "-output":
                        outputFilename = args[++i];
                        break;
                    case "-svnversion":
                        svnVersionDirectory = args[++i].Trim();
                        if (svnVersionDirectory.EndsWith("\\")) svnVersionDirectory = svnVersionDirectory.Remove(svnVersionDirectory.Length - 1, 1);
                        break;
                    default:
                        Usage();
                        return -1;
                }
            }

            if (svnVersionDirectory != null)
            {
                var svnProcess = new Process
                                 {
                                     StartInfo = new ProcessStartInfo(@"C:\Program Files\SlikSvn\bin\svnversion.exe", "\"" + svnVersionDirectory + "\"")
                                                 {
                                                     CreateNoWindow = true,
                                                     UseShellExecute = false,
                                                     RedirectStandardInput = false,
                                                     RedirectStandardOutput = true,
                                                     RedirectStandardError = true,
                                                 },
                                 };
                svnProcess.Start();
                svnVersionString = svnProcess.StandardOutput.ReadToEnd().Trim();
                svnProcess.WaitForExit();
            }
            //CSharpCodeExample();
            //var ccu = GenerateCSharpCode();
            //GenerateCode(ccu, "CSHARP");
            try
            {
                GenerateCode(namespaceName, className, svnVersionString, outputFilename);
            }
            catch (Exception e)
            {
                Console.WriteLine(e.Message);
                Usage();
                return -1;
            }
            return 0;
        }

        static void Usage()
        {
            Console.WriteLine("Usage: {0} -namespace <namespaceName> -class <className> [-svnversion <projectRootDirectory>] -output <outputFilename>", Path.GetFileNameWithoutExtension(Assembly.GetExecutingAssembly().Location));
            Console.WriteLine("Where: <namespaceName> is the desired namespace for the generated code");
            Console.WriteLine("       <className> is the desired static class name for the generated code");
            Console.WriteLine("       <projectRootDirectory> (optional) is the root directory of the project");
            Console.WriteLine("                              (usually $(ProjectDir) from inside Visual Studio)");
            Console.WriteLine("                              If this is specified, the read-only property SVNVersion will be generated");
            Console.WriteLine("       <outputFilename> is the filename that will contain the generated code");
        }

        static void GenerateCode(string namespaceName, string className, string svnVersionString, string outputFilename)
        {
            if (string.IsNullOrEmpty(namespaceName) || string.IsNullOrEmpty(className) || string.IsNullOrEmpty(outputFilename)) throw new ApplicationException("Namespace and output filename must be provided");

            var compileUnit = new CodeCompileUnit();

            // Create a NameSpace - namespace <namespaceName>
            //
            var codedomsamplenamespace = new CodeNamespace(namespaceName);

            // Create using statement - "using System;"
            //
            // var firstimport = new CodeNamespaceImport("System");

            // Add the using statement to the namespace -
            // namespace CodeDomSampleNS {
            //      using System;
            //
            codedomsamplenamespace.Imports.Add(new CodeNamespaceImport("System"));

            // Create a type inside the namespace - public class <className>
            //
            var newType = new CodeTypeDeclaration(className)
                          {
                              Attributes = MemberAttributes.Public | MemberAttributes.Static,
                          };

            var buildDateTimeMember = new CodeMemberProperty
                                      {
                                          Name = "BuildDateTime",
                                          Type = new CodeTypeReference(typeof (DateTime)),
                                          Attributes = MemberAttributes.Public | MemberAttributes.Static,
                                      };
            buildDateTimeMember.GetStatements.Add(new CodeMethodReturnStatement
                                                  {
                                                      Expression = new CodeObjectCreateExpression(typeof (DateTime), new CodeExpression[]
                                                                                                                     {
                                                                                                                         new CodePrimitiveExpression(DateTime.Now.Year), new CodePrimitiveExpression(DateTime.Now.Month), new CodePrimitiveExpression(DateTime.Now.Day), new CodePrimitiveExpression(DateTime.Now.Hour), new CodePrimitiveExpression(DateTime.Now.Minute), new CodePrimitiveExpression(DateTime.Now.Second), new CodeSnippetExpression("DateTimeKind.Local")
                                                                                                                     })
                                                  });
            newType.Members.Add(buildDateTimeMember);

            var buildEngineer = new CodeMemberProperty
                                {
                                    Name = "BuildEngineer",
                                    Type = new CodeTypeReference(typeof (string)),
                                    Attributes = MemberAttributes.Public | MemberAttributes.Static,
                                };

            buildEngineer.GetStatements.Add(new CodeMethodReturnStatement
                                            {
                                                Expression = new CodePrimitiveExpression(Environment.UserName)
                                            });
            newType.Members.Add(buildEngineer);

            if (!string.IsNullOrEmpty(svnVersionString))
            {
                var svnVersion = new CodeMemberProperty
                                 {
                                     Name = "SVNVersion",
                                     Type = new CodeTypeReference(typeof (string)),
                                     Attributes = MemberAttributes.Public | MemberAttributes.Static,
                                 };

                svnVersion.GetStatements.Add(new CodeMethodReturnStatement
                                             {
                                                 Expression = new CodePrimitiveExpression(svnVersionString)
                                             });
                newType.Members.Add(svnVersion);
            }

            // Add the type to the namespace
            //
            codedomsamplenamespace.Types.Add(newType);

            // Add the NameSpace to the CodeCompileUnit
            //
            compileUnit.Namespaces.Add(codedomsamplenamespace);

            var csharpcodeprovider = new CSharpCodeProvider();

            var tw1 = new IndentedTextWriter(new StreamWriter(outputFilename, false), "    ");
            csharpcodeprovider.GenerateCodeFromCompileUnit(compileUnit, tw1, new CodeGeneratorOptions
                                                                             {
                                                                                 BracingStyle = "C",
                                                                             });
            tw1.Close();
        }
#if false
        static void GenerateCode(CodeCompileUnit ccu, String codeprovider)
        {
            var cp = new CompilerParameters();
            String sourceFile;

            switch (codeprovider)
            {
                case "CSHARP":
                    // Generate Code from Compile Unit using CSharp code provider
                    //
                    var csharpcodeprovider = new CSharpCodeProvider();

                    if (csharpcodeprovider.FileExtension[0] == '.')
                    {
                        sourceFile = "CSharpSample" + csharpcodeprovider.FileExtension;
                    }
                    else
                    {
                        sourceFile = "CSharpSample." + csharpcodeprovider.FileExtension;
                    }
                    var tw1 = new IndentedTextWriter(new StreamWriter(sourceFile, false), "    ");
                    csharpcodeprovider.GenerateCodeFromCompileUnit(ccu, tw1, new CodeGeneratorOptions());
                    tw1.Close();
                    cp.GenerateExecutable = true;
                    cp.OutputAssembly = "CSharpSample.exe";
                    cp.GenerateInMemory = false;
                    csharpcodeprovider.CompileAssemblyFromDom(cp, ccu);
                    break;
                case "VBASIC":
                    // Generate Code from Compile Unit using VB code provider
                    //
                    var vbcodeprovider = new VBCodeProvider();
                    if (vbcodeprovider.FileExtension[0] == '.')
                    {
                        sourceFile = "VBSample" + vbcodeprovider.FileExtension;
                    }
                    else
                    {
                        sourceFile = "VBSample." + vbcodeprovider.FileExtension;
                    }
                    var tw2 = new IndentedTextWriter(new StreamWriter(sourceFile, false), "    ");
                    vbcodeprovider.GenerateCodeFromCompileUnit(ccu, tw2, new CodeGeneratorOptions());
                    tw2.Close();
                    cp.GenerateExecutable = true;
                    cp.OutputAssembly = "VBSample.exe";
                    cp.GenerateInMemory = false;
                    vbcodeprovider.CompileAssemblyFromDom(cp, ccu);
                    break;
            }
            return;
        }

        public static void CSharpCodeExample()
        {
            const string sourcecode = "\nusing System;\npublic class Sample \n{\n    static void Main()\n    {\n        Console.WriteLine(\"This is a test\");\n    }\n}";
            var provider = new CSharpCodeProvider();
            var cp = new CompilerParameters
                     {
                         GenerateExecutable = true,
                         OutputAssembly = "Result.exe",
                         GenerateInMemory = false
                     };
            var cr = provider.CompileAssemblyFromSource(cp, sourcecode);
            if (cr.Errors.Count > 0)
            {
                Console.WriteLine("Errors building {0} into {1}", sourcecode, cr.PathToAssembly);
                foreach (CompilerError ce in cr.Errors)
                {
                    Console.WriteLine("  {0}", ce);
                    Console.WriteLine();
                }
            }
            else
            {
                Console.WriteLine("Source \n \n {0} \n \n \n built into {1} successfully.", sourcecode, cr.PathToAssembly);
            }
            return;
        }

        static CodeCompileUnit GenerateCSharpCode()
        {
            var compileUnit = new CodeCompileUnit();

            // Create a NameSpace - "namespace CodeDomSampleNS"
            //
            var codedomsamplenamespace = new CodeNamespace("CodeDomSampleNS");

            // Create using statement - "using System;"
            //
            var firstimport = new CodeNamespaceImport("System");

            // Add the using statement to the namespace -
            // namespace CodeDomSampleNS {
            //      using System;
            //
            codedomsamplenamespace.Imports.Add(firstimport);

            // Create a type inside the namespace - public class CodeDomSample
            //
            var newType = new CodeTypeDeclaration("CodeDomSample")
                          {
                              Attributes = MemberAttributes.Public
                          };

            // Create a Main method which will be entry point for the class
            // public static void Main
            //
            var mainmethod = new CodeEntryPointMethod();

            // Add an expression inside Main -
            //  Console.WriteLine("Inside Main ...");
            var mainexp1 = new CodeMethodInvokeExpression(new CodeTypeReferenceExpression("System.Console"), "WriteLine", new CodePrimitiveExpression("Inside Main ..."));
            mainmethod.Statements.Add(mainexp1);

            // Add another expression inside Main
            //  CodeDomSample cs = new CodeDomSample()
            //
            CodeStatement cs = new CodeVariableDeclarationStatement("CodeDomSample", "cs", new CodeObjectCreateExpression(new CodeTypeReference("CodeDomSample")));
            mainmethod.Statements.Add(cs);

            // At the end of the CodeStatements we should have constructed the following
            // public static void Main() {
            //      Console.WriteLine("Inside Main ...");
            //      CodeDomSample cs = new CodeDomSample();
            // }

            // Create a constructor for the CodeDomSample class
            // public CodeDomSample() { }
            //
            var constructor = new CodeConstructor
                              {
                                  Attributes = MemberAttributes.Public
                              };

            // Add an expression to the constructor
            // public CodeDomSample() { Comsole.WriteLine("Inside CodeDomSample Constructor ...");
            //
            var constructorexp = new CodeMethodInvokeExpression(new CodeTypeReferenceExpression("System.Console"), "WriteLine", new CodePrimitiveExpression("Inside CodeDomSample Constructor ..."));
            constructor.Statements.Add(constructorexp);

            // Add constructor and mainmethod to type
            //
            newType.Members.Add(constructor);
            newType.Members.Add(mainmethod);

            // Add the type to the namespace
            //
            codedomsamplenamespace.Types.Add(newType);

            // Add the NameSpace to the CodeCompileUnit
            //
            compileUnit.Namespaces.Add(codedomsamplenamespace);

            // Return the CompileUnit
            //
            return compileUnit;
        }
#endif
    }
}