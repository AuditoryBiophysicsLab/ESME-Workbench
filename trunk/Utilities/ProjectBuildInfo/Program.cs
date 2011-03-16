﻿using System;
using System.CodeDom;
using System.CodeDom.Compiler;
using System.IO;
using System.Reflection;
using System.Security.Principal;
using Microsoft.CSharp;
using Microsoft.VisualBasic;

namespace ProjectBuildInfo
{
    internal class Program
    {
        static int Main(string[] args)
        {
            string namespaceName = null;
            string className = null;
            string outputFilename = null;
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
                    case "-filename":
                        outputFilename = args[++i];
                        break;
                    default:
                        Usage();
                        return -1;
                }
            }

            //CSharpCodeExample();
            //var ccu = GenerateCSharpCode();
            //GenerateCode(ccu, "CSHARP");
            try
            {
                GenerateCode(namespaceName, className, outputFilename);
            }
            catch (Exception e)
            {
                Console.WriteLine(e.Message);
                Usage();
                return -1;
            }
            return 0;
        }

        static void Usage() { Console.WriteLine("Usage: {0} -namespace <namespaceName> -filename <outputFilename>", Path.GetFileNameWithoutExtension(Assembly.GetExecutingAssembly().Location)); }

        static void GenerateCode(string namespaceName, string className, string outputFilename)
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
            // codedomsamplenamespace.Imports.Add(new CodeNamespaceImport("System"));

            // Create a type inside the namespace - public class <className>
            //
            var newType = new CodeTypeDeclaration(className)
                          {
                              Attributes = MemberAttributes.Public | MemberAttributes.Static,
                          };

            var buildDateTimeMember = new CodeMemberProperty
                                      {
                                          Name = "BuildDateTime",
                                          Attributes = MemberAttributes.Public | MemberAttributes.Static,
                                      };

            buildDateTimeMember.GetStatements.Add(new CodeMethodReturnStatement
                                                  {
                                                      Expression = new CodePrimitiveExpression(DateTime.Now.ToString())
                                                  });
            newType.Members.Add(buildDateTimeMember);

            var buildEngineer = new CodeMemberProperty
                                {
                                    Name = "BuildEngineer",
                                    Attributes = MemberAttributes.Public | MemberAttributes.Static,
                                };

            buildEngineer.GetStatements.Add(new CodeMethodReturnStatement
                                                  {
                                                      Expression = new CodePrimitiveExpression(Environment.UserName)
                                                  });
            newType.Members.Add(buildEngineer);

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
    }
}