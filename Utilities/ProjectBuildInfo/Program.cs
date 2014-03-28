using System;
using System.CodeDom;
using System.CodeDom.Compiler;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Security.Cryptography;
using GitSharp;
using Microsoft.CSharp;

namespace ProjectBuildInfo
{
    // ReSharper disable BitwiseOperatorOnEnumWithoutFlags
    internal class Program
    {
        static int Main(string[] args)
        {
#if DEBUG
            Console.Write("ProjectBuildInfo command line: ");
            foreach (var arg in args) Console.Write(arg + " ");
#endif
            Console.WriteLine();
            string namespaceName = null;
            string className = null;
            string outputFilename = null;
            string versionFile = null;
            string versionNumber = null;
            string assemblyVersionFile = null;
            string wixVersionFile = null;
            string webVersionFile = null;
            string installer32 = null;
            string installer64 = null;
            string productName = null;
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
                    case "-version":
                        versionFile = args[++i].Trim();
                        break;
                    case "-assemblyversion":
                        assemblyVersionFile = args[++i].Trim();
                        break;
                    case "-wixversion":
                        wixVersionFile = args[++i].Trim();
                        break;
                    case "-webversion":
                        webVersionFile = args[++i].Trim();
                        break;
                    case "-installer32bit":
                        installer32 = args[++i].Trim();
                        break;
                    case "-installer64bit":
                        installer64 = args[++i].Trim();
                        break;
                    case "-productName":
                        productName = args[++i].Trim();
                        break;
                    default:
                        Usage();
                        return -1;
                }
            }
            try
            {
                var gitUrl = Repository.FindRepository(outputFilename);
                if (gitUrl == null || !Repository.IsValid(gitUrl))
                    throw new ApplicationException("Given path doesn't seem to refer to a git repository: " + outputFilename);
                var repo = new Repository(gitUrl);
                var target = repo.Head.Target;

                if (versionFile != null)
                {
                    foreach (var versionFields in from curLine in File.ReadAllLines(versionFile)
                                                  select curLine.Trim()
                                                  into trimmedLine
                                                  where !string.IsNullOrEmpty(trimmedLine) && !trimmedLine.StartsWith("//")
                                                  select trimmedLine.Split('.'))
                    {
                        switch (versionFields.Length)
                        {
                            case 4:
                            case 3:
                                int major, minor, build;
                                if (!int.TryParse(versionFields[0], out major) || !int.TryParse(versionFields[1], out minor) || !int.TryParse(versionFields[2], out build) || major < 0 || minor < 0 ||
                                    build < 0)
                                    throw new FormatException(
                                        string.Format(
                                            "Version file not in expected format. There should be only one line that does not begin with a comment mark ('//') and that line should contain a version number template in the form 1.2.3 where 1 is the Major version number of this application, 2 is the Minor version number and 3 is the Build number.  A fourth field, taken from the Subversion revision number of the output directory, will be appended to this and used for assembly and installer version numbers later in the build process."));
                                versionNumber = string.Format("{0}.{1}.{2}", versionFields[0], versionFields[1], versionFields[2]);
                                break;
                            default:
                                throw new FormatException(
                                    string.Format(
                                        "Version file not in expected format. There should be only one line that does not begin with a comment mark ('//') and that line should contain a version number template in the form 1.2.3 where 1 is the Major version number of this application, 2 is the Minor version number and 3 is the Build number.  A fourth field, taken from the git hash of the output directory, will be appended to this and used for assembly and installer version numbers later in the build process."));
                        }
                    }
                }
                if (assemblyVersionFile != null)
                {
                    if (versionNumber == null) throw new ApplicationException("if -assemblyversion is specified, -version must also be specified");
                    using (var writer = new StreamWriter(assemblyVersionFile))
                    {
                        writer.WriteLine("using System.Reflection;");
                        writer.WriteLine();
                        writer.WriteLine("[assembly: AssemblyVersion(\"{0}\")]", versionNumber);
                        writer.WriteLine("[assembly: AssemblyFileVersion(\"{0}\")]", versionNumber);
                    }
                }

                if (wixVersionFile != null)
                {
                    if (versionNumber == null) throw new ApplicationException("if -wixversion is specified, -version must also be specified");
                    using (var writer = new StreamWriter(wixVersionFile))
                    {
                        writer.WriteLine("<?xml version=\"1.0\" encoding=\"utf-8\"?>");
                        writer.WriteLine("<Include>");
                        writer.WriteLine("  <?define ProductFullVersion = \"{0}\" ?>", versionNumber);
                        writer.WriteLine("</Include>");
                    }
                }
                if (namespaceName != null && className != null) GenerateCode(namespaceName, className, target.Hash, outputFilename);
                if (webVersionFile != null)
                {
                    using (var writer = new StreamWriter(webVersionFile))
                    {
                        writer.WriteLine("<?xml version=\"1.0\" encoding=\"utf-8\"?>");
                        writer.WriteLine("<product>");
                        if (productName != null)
                        {
                            writer.WriteLine("  <name>{0}</name>", productName);
                        }
                        if (installer32 != null)
                        {
                            writer.WriteLine("  <x86>");
                            writer.WriteLine(GenerateWebVersionXml(installer32));
                            writer.WriteLine("  </x86>");
                        }
                        if (installer64 != null)
                        {
                            writer.WriteLine("  <x64>");
                            writer.WriteLine(GenerateWebVersionXml(installer64));
                            writer.WriteLine("  </x64>");
                        }
                        writer.WriteLine("</product>");
                    }
                }
            }
            catch (Exception e)
            {
                Console.WriteLine(e.Message);
                Usage();
                return -1;
            }
#if false
            try
            {
                string svnVersionString;
                using (var client = new SvnClient())
                {
                    if (string.IsNullOrEmpty(outputFilename)) outputFilename = assemblyVersionFile;
                    SvnInfoEventArgs svnInfo;
                    client.GetInfo(new SvnUriTarget(client.GetUriFromWorkingCopy(Path.GetDirectoryName(outputFilename))), out svnInfo);
                    svnVersionString = svnInfo.Revision.ToString(CultureInfo.InvariantCulture);
                }
                if (versionFile != null)
                {
                    var inputLines = File.ReadAllLines(versionFile);
                    foreach (var inputLine in inputLines)
                    {
                        var curLine = inputLine.Trim();
                        if (string.IsNullOrEmpty(curLine) || curLine.StartsWith("//")) continue;
                        var versionFields = curLine.Split('.');
                        switch (versionFields.Length)
                        {
                            case 4:
                            case 3:
                                int major, minor, build;
                                if (!int.TryParse(versionFields[0], out major) || !int.TryParse(versionFields[1], out minor) || !int.TryParse(versionFields[2], out build) || major < 0 || minor < 0 || build < 0) throw new FormatException(string.Format("Version file not in expected format. There should be only one line that does not begin with a comment mark ('//') and that line should contain a version number template in the form 1.2.3 where 1 is the Major version number of this application, 2 is the Minor version number and 3 is the Build number.  A fourth field, taken from the Subversion revision number of the output directory, will be appended to this and used for assembly and installer version numbers later in the build process."));
                                versionNumber = string.Format("{0}.{1}.{2}.{3}", versionFields[0], versionFields[1], versionFields[2], svnVersionString);
                                break;
                            default:
                                throw new FormatException(string.Format("Version file not in expected format. There should be only one line that does not begin with a comment mark ('//') and that line should contain a version number template in the form 1.2.3 where 1 is the Major version number of this application, 2 is the Minor version number and 3 is the Build number.  A fourth field, taken from the Subversion revision number of the output directory, will be appended to this and used for assembly and installer version numbers later in the build process."));
                        }
                    }
                }
                if (assemblyVersionFile != null)
                {
                    if (versionNumber == null) throw new ApplicationException("if -assemblyversion is specified, -version must also be specified");
                    using (var writer = new StreamWriter(assemblyVersionFile))
                    {
                        writer.WriteLine("using System.Reflection;");
                        writer.WriteLine();
                        writer.WriteLine("[assembly: AssemblyVersion(\"{0}\")]", versionNumber);
                        writer.WriteLine("[assembly: AssemblyFileVersion(\"{0}\")]", versionNumber);
                    }
                }

                if (wixVersionFile != null)
                {
                    if (versionNumber == null) throw new ApplicationException("if -wixversion is specified, -version must also be specified");
                    using (var writer = new StreamWriter(wixVersionFile))
                    {
                        writer.WriteLine("<?xml version=\"1.0\" encoding=\"utf-8\"?>");
                        writer.WriteLine("<Include>");
                        writer.WriteLine("  <?define ProductFullVersion = \"{0}\" ?>", versionNumber);
                        writer.WriteLine("</Include>");
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
#endif
            return 0;
        }

        static void Usage()
        {
            Console.WriteLine("Usage: {0} -namespace [<namespaceName>] [-class <className>] [-version <versionNumberFile>] [-assemblyversion <assemblyVersionFile>] [-wixversion <wixVersionFile>] -output <outputFilename>", Path.GetFileNameWithoutExtension(Assembly.GetExecutingAssembly().Location));
            Console.WriteLine("Where: <namespaceName> is the desired namespace for the generated code");
            Console.WriteLine("       <className> is the desired static class name for the generated code");
            Console.WriteLine("       <versionNumberFile> (optional) is the path to a text file file that will have the application");
            Console.WriteLine("                           version number read out of it.  This version number, combined with the");
            Console.WriteLine("                           subversion revision number of the output directory, will be used for");
            Console.WriteLine("                           the version numbers written to the <assemblyVersionFile> and <wixVersionFile>");
            Console.WriteLine("       <assemblyVersionFile> (optional) is the path to a file usually called AssemblyVersionInfo.cs");
            Console.WriteLine("                             This file will be created or updated with the version in the versionNumberFile");
            Console.WriteLine("       <wixVersionFile> (optional) is the path to a file usually called version.wxi");
            Console.WriteLine("                        This file will be created or updated with the version in the versionNumberFile");
            Console.WriteLine("       <outputFilename> is the filename that will contain the generated code");
        }

        static string GenerateWebVersionXml(string installerFilename)
        {
            var versionString = FileVersionInfo.GetVersionInfo(installerFilename).FileVersion;
            string md5Sum;
            using (var stream = File.Open(installerFilename, FileMode.Open, FileAccess.Read))
                md5Sum = Convert.ToBase64String(new MD5CryptoServiceProvider().ComputeHash(stream));
            return string.Format("    <installer>{0}</installer>\n    <version>{1}</version>\n    <md5SumBase64>{2}</md5SumBase64>", Path.GetFileName(installerFilename), versionString, md5Sum);
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
                                     Name = "GitHash",
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