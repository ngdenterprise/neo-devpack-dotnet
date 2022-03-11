// Copyright (C) 2015-2021 The Neo Project.
// 
// The Neo.Compiler.CSharp is free software distributed under the MIT 
// software license, see the accompanying file LICENSE in the main directory 
// of the project or http://www.opensource.org/licenses/mit-license.php 
// for more details.
// 
// Redistribution and use in source and binary forms with or without
// modifications are permitted.

using Microsoft.CodeAnalysis;
using Neo.IO;
using System;
using System.CommandLine;
using System.CommandLine.Invocation;
using System.IO;
using System.IO.Compression;
using System.Linq;
using System.Reflection;

namespace Neo.Compiler
{
    class Program
    {
        static int Main(string[] args)
        {
            RootCommand rootCommand = new(Assembly.GetExecutingAssembly().GetCustomAttribute<AssemblyTitleAttribute>()!.Title)
            {
                new Argument<string[]>("paths", "The path of the project file, project directory or source files."),
                new Option<string>(new[] { "-o", "--output" }, "Specifies the output directory."),
                new Option<string>("--contract-name", "Specifies the base name of the output files."),
                new Option<bool>(new[] { "-d", "--debug" }, "Indicates whether to generate debugging information."),
                new Option<bool>("--assembly", "Indicates whether to generate assembly."),
                new Option<bool>("--no-optimize", "Instruct the compiler not to optimize the code."),
                new Option<bool>("--no-inline", "Instruct the compiler not to insert inline code."),
                new Option<byte>("--address-version", () => ProtocolSettings.Default.AddressVersion, "Indicates the address version used by the compiler."),
                new Option<bool>("--no-compress-debug-info", "Indicates whether to compress debug info"),
            };
            rootCommand.Handler = CommandHandler.Create<RootCommand, Options, string[]>(Handle);
            return rootCommand.Invoke(args);
        }

        private static int Handle(RootCommand command, Options options, string[] paths)
        {
            if (paths is null || paths.Length == 0)
            {
                var ret = ProcessDirectory(options, Environment.CurrentDirectory);
                if (ret == 2)
                {
                    // Display help without args
                    command.Invoke("--help");
                }
                return ret;
            }
            paths = paths.Select(p => Path.GetFullPath(p)).ToArray();
            if (paths.Length == 1)
            {
                string path = paths[0];
                if (Directory.Exists(path))
                    return ProcessDirectory(options, path);
                if (File.Exists(path) && Path.GetExtension(path).ToLowerInvariant() == ".csproj")
                    return ProcessCsproj(options, path);
            }
            foreach (string path in paths)
            {
                if (Path.GetExtension(path).ToLowerInvariant() != ".cs")
                {
                    Console.Error.WriteLine("The files must have a .cs extension.");
                    return 1;
                }
                if (!File.Exists(path))
                {
                    Console.Error.WriteLine($"The file \"{path}\" doesn't exist.");
                    return 1;
                }
            }
            return ProcessSources(options, Path.GetDirectoryName(paths[0])!, paths);
        }

        private static int ProcessDirectory(Options options, string path)
        {
            string? csproj = Directory.EnumerateFiles(path, "*.csproj", SearchOption.TopDirectoryOnly).FirstOrDefault();
            if (csproj is null)
            {
                string obj = Path.Combine(path, "obj");
                string[] sourceFiles = Directory.EnumerateFiles(path, "*.cs", SearchOption.AllDirectories).Where(p => !p.StartsWith(obj)).ToArray();
                if (sourceFiles.Length == 0)
                {
                    Console.Error.WriteLine($"No .cs file is found in \"{path}\".");
                    return 2;
                }
                return ProcessSources(options, path, sourceFiles);
            }
            else
            {
                return ProcessCsproj(options, csproj);
            }
        }

        private static int ProcessCsproj(Options options, string path)
        {
            return ProcessOutputs(options, Path.GetDirectoryName(path)!, CompilationContext.CompileProject(path, options));
        }

        private static int ProcessSources(Options options, string folder, string[] sourceFiles)
        {
            return ProcessOutputs(options, folder, CompilationContext.CompileSources(sourceFiles, options));
        }

        private static int ProcessOutputs(Options options, string folder, CompilationContext context)
        {
            foreach (Diagnostic diagnostic in context.Diagnostics)
            {
                if (diagnostic.Severity == DiagnosticSeverity.Error)
                    Console.Error.WriteLine(diagnostic.ToString());
                else
                    Console.WriteLine(diagnostic.ToString());
            }
            if (context.Success)
            {
                folder = options.Output ?? Path.Combine(folder, "bin", "sc");
                var contractName = string.IsNullOrEmpty(options.ContractName)
                    ? context.ContractName : options.ContractName;
                Directory.CreateDirectory(folder);
                string path = Path.Combine(folder, $"{contractName}.nef");
                SmartContract.NefFile nef = context.CreateExecutable();
                File.WriteAllBytes(path, nef.ToArray());
                Console.WriteLine($"Created {path}");
                path = Path.Combine(folder, $"{contractName}.manifest.json");
                File.WriteAllBytes(path, context.CreateManifest().ToByteArray(false));
                Console.WriteLine($"Created {path}");
                if (options.Debug)
                {
                    var writerOptions = new System.Text.Json.JsonWriterOptions
                    {
                        Encoder = System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping,
                        Indented = true
                    };

                    if (options.NoCompressDebugInfo)
                    {
                        path = Path.Combine(folder, $"{contractName}.debug.json");
                        using FileStream fs = new(path, FileMode.Create, FileAccess.Write);
                        using var writer = new System.Text.Json.Utf8JsonWriter(fs, writerOptions);
                        context.WriteDebugInfo(writer, nef);
                    }
                    else
                    {
                        path = Path.Combine(folder, $"{contractName}.nefdbgnfo");
                        using FileStream fs = new(path, FileMode.Create, FileAccess.Write);
                        using ZipArchive archive = new(fs, ZipArchiveMode.Create);
                        using Stream stream = archive.CreateEntry($"{contractName}.debug.json").Open();
                        using var writer = new System.Text.Json.Utf8JsonWriter(stream, writerOptions);
                        context.WriteDebugInfo(writer, nef);
                    }
                    Console.WriteLine($"Created {path}");
                }
                if (options.Assembly)
                {
                    path = Path.Combine(folder, $"{contractName}.asm");
                    File.WriteAllText(path, context.CreateAssembly());
                    Console.WriteLine($"Created {path}");
                }
                Console.WriteLine("Compilation completed successfully.");
                return 0;
            }
            else
            {
                Console.Error.WriteLine("Compilation failed.");
                return 1;
            }
        }
    }

}
