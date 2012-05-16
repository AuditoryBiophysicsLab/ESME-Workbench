using System;
using System.CodeDom;
using System.CodeDom.Compiler;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Reflection;
using Microsoft.CSharp;
using SharpSvn;

namespace ProjectBuildInfo
{
    // ReSharper disable BitwiseOperatorOnEnumWihtoutFlags
    internal class Program
    {
        static int Main(string[] args)
        {
            string namespaceName = null;
            string className = null;
            string outputFilename = null;
            string assemblyVersionFile = null;
            string wixVersionFile = null;
            string assemblyVersionString = null;
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
                    case "-assemblyversion":
                        assemblyVersionFile = args[++i].Trim();
                        break;
                    case "-wixversion":
                        wixVersionFile = args[++i].Trim();
                        break;
                    default:
                        Usage();
                        return -1;
                }
            }

            try
            {
                string svnVersionString;
                using (var client = new SvnClient())
                {
                    SvnInfoEventArgs svnInfo;
                    client.GetInfo(new SvnUriTarget(client.GetUriFromWorkingCopy(Path.GetDirectoryName(outputFilename))), out svnInfo);
                    svnVersionString = svnInfo.Revision.ToString(CultureInfo.InvariantCulture);
                }

                if (assemblyVersionFile != null)
                {
                    var inputLines = File.ReadAllLines(assemblyVersionFile);
                    var outputLines = new List<string>();
                    foreach (var inputLine in inputLines)
                    {
                        if (inputLine.Contains("AssemblyVersion") || inputLine.Contains("AssemblyFileVersion"))
                        {
                            var lineFields = inputLine.Split('"');
                            if (lineFields.Length != 3) throw new FormatException(string.Format("Assembly version file not in expected format. lineFields.Length should be 3, but was {0}", lineFields.Length));
                            var versionFields = lineFields[1].Split('.');
                            if (versionFields.Length != 4) throw new FormatException(string.Format("Assembly version file not in expected format. versionFields.Length should be 4, but was {0}", versionFields.Length));
                            assemblyVersionString = string.Format("{0}.{1}.{2}.{3}", versionFields[0], versionFields[1], versionFields[2], svnVersionString);
                            if (versionFields[1] == assemblyVersionString)
                            {
                                outputLines.Clear();
                                outputLines = null;
                                break;
                            }
                            if (inputLine.Contains("AssemblyVersion")) outputLines.Add(string.Format("[assembly: AssemblyVersion(\"{0}\")]", assemblyVersionString));
                            if (inputLine.Contains("AssemblyFileVersion")) outputLines.Add(string.Format("[assembly: AssemblyFileVersion(\"{0}\")]", assemblyVersionString));
                        }
                        else outputLines.Add(inputLine);
                    }
                    if (outputLines != null)
                    {
                        if (outputLines.Count != inputLines.Length) throw new ApplicationException("InputLines and OutputLines have different counts for assembly version file");
                        File.WriteAllLines(assemblyVersionFile, outputLines.ToArray());
                    }
                }

                if (wixVersionFile != null)
                {
                    var inputLines = File.ReadAllLines(wixVersionFile);
                    var outputLines = new List<string>();
                    foreach (var inputLine in inputLines)
                    {
                        if (inputLine.Contains("ProductFullVersion"))
                        {
                            
                            var lineFields = inputLine.Split('"');
                            if (lineFields.Length != 3) throw new FormatException(string.Format("WiX version file not in expected format. lineFields.Length should be 3, but was {0}", lineFields.Length));
                            var versionFields = lineFields[1].Split('.');
                            if (versionFields.Length != 4) throw new FormatException(string.Format("WiX version file not in expected format. versionFields.Length should be 4, but was {0}", versionFields.Length));
                            var wixVersionString = string.Format("{0}.{1}.{2}.{3}", versionFields[0], versionFields[1], versionFields[2], svnVersionString);
                            if (assemblyVersionString != null) wixVersionString = assemblyVersionString;
                            if (versionFields[1] == wixVersionString)
                            {
                                outputLines.Clear();
                                outputLines = null;
                                break;
                            }
                            outputLines.Add(string.Format("  <?define ProductFullVersion = \"{0}\" ?>", wixVersionString));
                        }
                        else outputLines.Add(inputLine);
                    }
                    if (outputLines != null)
                    {
                        if (outputLines.Count != inputLines.Length) throw new ApplicationException("InputLines and OutputLines have different counts for WiX version file");
                        File.WriteAllLines(wixVersionFile, outputLines.ToArray());
                    }
                }
                if (namespaceName != null && className != null) GenerateCode(namespaceName, className, svnVersionString, outputFilename);
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
            Console.WriteLine("Usage: {0} -namespace [<namespaceName>] [-class <className>] [-assemblyversion <assemblyVersionFile>] [-wixversion <wixVersionFile>] -output <outputFilename>", Path.GetFileNameWithoutExtension(Assembly.GetExecutingAssembly().Location));
            Console.WriteLine("Where: <namespaceName> is the desired namespace for the generated code");
            Console.WriteLine("       <className> is the desired static class name for the generated code");
            Console.WriteLine("       <assemblyVersionFile> (optional) is the path to a file usually called AssemblyVersionInfo.cs");
            Console.WriteLine("                             This file will be updated (if it exists) with the subversion version number");
            Console.WriteLine("                            in the least significant field");
            Console.WriteLine("       <wixVersionFile> (optional) is the path to a file usually called version.wxi");
            Console.WriteLine("                        This file will be updated (if it exists) with the subversion version number");
            Console.WriteLine("                        in the least significant field.  If <assemblyVersionFile> is also specified");
            Console.WriteLine("                        the wix version number will be replaced by the assembly version number");
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
            codedomsamplenamespace.Imports.Add(new CodeNamespaceImport("System"));

            // Create a type inside the namespace - public class <className>
            //
            var newType = new CodeTypeDeclaration(className)
                          {
                              TypeAttributes = TypeAttributes.Public,
                              Attributes = MemberAttributes.Static,
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
    }
    // ReSharper restore BitwiseOperatorOnEnumWihtoutFlags
}