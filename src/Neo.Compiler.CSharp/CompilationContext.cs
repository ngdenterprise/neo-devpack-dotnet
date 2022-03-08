// Copyright (C) 2015-2021 The Neo Project.
// 
// The Neo.Compiler.CSharp is free software distributed under the MIT 
// software license, see the accompanying file LICENSE in the main directory 
// of the project or http://www.opensource.org/licenses/mit-license.php 
// for more details.
// 
// Redistribution and use in source and binary forms with or without
// modifications are permitted.

extern alias scfx;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Neo.Cryptography.ECC;
using Neo.IO.Json;
using Neo.SmartContract;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Numerics;
using System.Reflection;
using System.Text;
using System.Xml.Linq;
using Diagnostic = Microsoft.CodeAnalysis.Diagnostic;
using Utf8JsonWriter = System.Text.Json.Utf8JsonWriter;

namespace Neo.Compiler
{
    public class CompilationContext
    {
        record struct StructDef(INamedTypeSymbol Struct, IReadOnlyList<IFieldSymbol> Fields);
        record struct StorageKeySegment(string Name, PrimitiveType Type);
        record struct StorageGroup(
            string Name, 
            INamedTypeSymbol ValueType, 
            ReadOnlyMemory<byte> Prefix, 
            IReadOnlyList<StorageKeySegment> Segments);

        private static readonly MetadataReference[] commonReferences;
        private static readonly Dictionary<string, MetadataReference> metaReferences = new();
        private readonly Compilation compilation;
        private INamedTypeSymbol? smartContractSymbol;
        private readonly List<Diagnostic> diagnostics = new();
        private readonly HashSet<string> supportedStandards = new();
        private readonly List<AbiMethod> methodsExported = new();
        private readonly List<AbiEvent> eventsExported = new();
        private readonly PermissionBuilder permissions = new();
        private readonly HashSet<string> trusts = new();
        private readonly JObject manifestExtra = new();
        private readonly MethodConvertCollection methodsConverted = new();
        private readonly MethodConvertCollection methodsForward = new();
        private readonly List<MethodToken> methodTokens = new();
        private readonly Dictionary<IFieldSymbol, byte> staticFields = new();
        private readonly Dictionary<ITypeSymbol, byte> vtables = new();
        private byte[]? script;
        private readonly List<StructDef> structDefs = new();
        private readonly List<StorageGroup> storageGroups = new();
        private readonly TypeCache typeCache;

        public bool Success => diagnostics.All(p => p.Severity != DiagnosticSeverity.Error);
        public IReadOnlyList<Diagnostic> Diagnostics => diagnostics;
        public string? ContractName { get; private set; }
        private string? Source { get; set; }
        internal Options Options { get; private set; }
        internal IEnumerable<IFieldSymbol> StaticFieldSymbols => staticFields.OrderBy(p => p.Value).Select(p => p.Key);
        internal IEnumerable<(byte, ITypeSymbol)> VTables => vtables.OrderBy(p => p.Value).Select(p => (p.Value, p.Key));
        internal int StaticFieldCount => staticFields.Count + vtables.Count;
        private byte[] Script => script ??= GetInstructions().Select(p => p.ToArray()).SelectMany(p => p).ToArray();

        static CompilationContext()
        {
            string coreDir = Path.GetDirectoryName(typeof(object).Assembly.Location)!;
            commonReferences = new[]
            {
                MetadataReference.CreateFromFile(Path.Combine(coreDir, "System.Runtime.dll")),
                MetadataReference.CreateFromFile(Path.Combine(coreDir, "System.Runtime.InteropServices.dll")),
                MetadataReference.CreateFromFile(typeof(string).Assembly.Location),
                MetadataReference.CreateFromFile(typeof(DisplayNameAttribute).Assembly.Location),
                MetadataReference.CreateFromFile(typeof(BigInteger).Assembly.Location)
            };
        }

        private CompilationContext(Compilation compilation, Options options)
        {
            this.compilation = compilation;
            this.typeCache = new(this.compilation);
            this.Options = options;
            this.ContractName = options.ContractName;
        }

        private void RemoveEmptyInitialize()
        {
            int index = methodsExported.FindIndex(p => p.Name == "_initialize");
            if (index < 0) return;
            AbiMethod method = methodsExported[index];
            if (methodsConverted[method.Symbol].Instructions.Count <= 1)
            {
                methodsExported.RemoveAt(index);
                methodsConverted.Remove(method.Symbol);
            }
        }

        private IEnumerable<Instruction> GetInstructions()
        {
            return methodsConverted.SelectMany(p => p.Instructions).Concat(methodsForward.SelectMany(p => p.Instructions));
        }

        private int GetAbiOffset(IMethodSymbol method)
        {
            if (!methodsForward.TryGetValue(method, out MethodConvert? convert))
                convert = methodsConverted[method];
            return convert.Instructions[0].Offset;
        }

        private static bool ValidateContractTrust(string value)
        {
            if (value == "*") return true;
            if (UInt160.TryParse(value, out _)) return true;
            if (ECPoint.TryParse(value, ECCurve.Secp256r1, out _)) return true;
            return false;
        }

        private void Compile()
        {
            HashSet<INamedTypeSymbol> processed = new();
            foreach (SyntaxTree tree in compilation.SyntaxTrees)
            {
                SemanticModel model = compilation.GetSemanticModel(tree);
                diagnostics.AddRange(model.GetDiagnostics());
                if (!Success) continue;
                try
                {
                    ProcessCompilationUnit(processed, model, tree.GetCompilationUnitRoot());
                }
                catch (CompilationException ex)
                {
                    diagnostics.Add(ex.Diagnostic);
                }
            }
            if (Success)
            {
                if (smartContractSymbol is null)
                {
                    diagnostics.Add(Diagnostic.Create(DiagnosticId.NoEntryPoint, DiagnosticCategory.Default, "No SmartContract is found in the sources.", DiagnosticSeverity.Error, DiagnosticSeverity.Error, true, 0));
                    return;
                }
                RemoveEmptyInitialize();
                Instruction[] instructions = GetInstructions().ToArray();
                instructions.RebuildOffsets();
                if (!Options.NoOptimize) Optimizer.CompressJumps(instructions);
                instructions.RebuildOperands();
                GenerateDebugInfo();
            }
        }

        void DebugWarning(ISymbol symbol, string id, string message, int warningLevel = 1)
        {
            var locaction = symbol.Locations.IsEmpty ? null : symbol.Locations[0];
            var diag = Diagnostic.Create(id: id, 
                category: DiagnosticCategory.Default, 
                message: message,
                severity: DiagnosticSeverity.Warning,
                defaultSeverity: DiagnosticSeverity.Warning,
                isEnabledByDefault: true, 
                warningLevel: warningLevel, 
                location: locaction);
            diagnostics.Add(diag);
        }

        private void GenerateDebugInfo()
        {
            IEnumerable<INamedTypeSymbol> structs = GetSymbols<ClassDeclarationSyntax>(this.compilation)
                .Concat(GetSymbols<StructDeclarationSyntax>(compilation))
                .OfType<INamedTypeSymbol>();

            foreach (var @struct in structs)
            {
                var structFields = @struct.GetAllMembers()
                    .OfType<IFieldSymbol>()
                    .Where(f => !f.HasConstantValue && !f.IsStatic);

                if (!structFields.Any()) continue;

                structDefs.Add(new StructDef(@struct, structFields.ToArray()));
            }

            Debug.Assert(smartContractSymbol is not null);

            var fields = smartContractSymbol.GetAllMembers().OfType<IFieldSymbol>();
            foreach (var field in fields)
            {
                var storageGroup = field.GetAttributes()
                    .SingleOrDefault(a => a.AttributeClass is not null 
                        && typeCache.Equals(a.AttributeClass, "Neo.SmartContract.Framework.Attributes.StorageGroupAttribute"));
                if (storageGroup is null) continue;

                if (!field.IsConst || field.ConstantValue is null)
                {
                    DebugWarning(field, "NC4001", "Storage Group Fields must be const");
                    continue;
                }

                string name = "";
                INamedTypeSymbol? valueType = null;
                if (storageGroup.ConstructorArguments.Length == 1)
                {
                    name = field.Name.StartsWith("Prefix_", StringComparison.InvariantCultureIgnoreCase)
                        ? field.Name.Substring(7) 
                        : field.Name;
                    valueType = storageGroup.ConstructorArguments[0].Value as INamedTypeSymbol; 
                }
                else if (storageGroup.ConstructorArguments.Length == 2)
                {
                    name = storageGroup.ConstructorArguments[0].Value as string ?? string.Empty; 
                    valueType = storageGroup.ConstructorArguments[1].Value as INamedTypeSymbol; 
                }

                if (string.IsNullOrEmpty(name) || valueType is null)
                {
                    DebugWarning(field, "NC4002", "Invalid StorageGroup attribute values");
                    continue;
                }

                ReadOnlyMemory<byte> prefix;
                if (field.Type.SpecialType == SpecialType.System_Byte)
                {
                    prefix = new [] { (byte)field.ConstantValue };
                }
                else if (field.Type.SpecialType == SpecialType.System_String)
                { 
                    prefix = Neo.Utility.StrictUTF8.GetBytes((string)field.ConstantValue!);
                }
                else
                {
                    DebugWarning(field, "NC4003", "Storage Group Fields must be byte or string");
                    continue;
                }

                var keySegments = field.GetAttributes()
                    .Where(a => a.AttributeClass is not null 
                        && typeCache.Equals(a.AttributeClass, "Neo.SmartContract.Framework.Attributes.StorageKeySegmentAttribute"))
                    .Select(a => new StorageKeySegment((string)a.ConstructorArguments[0].Value!, (PrimitiveType)a.ConstructorArguments[1].Value!))
                    .ToArray();

                var initialSegments = keySegments.Take(keySegments.Length - 1);
                if (initialSegments.Any(s => s.Type switch
                    {
                        PrimitiveType.Address => true,
                        PrimitiveType.Hash160 => true,
                        PrimitiveType.Hash256 => true,
                        _ => false,
                    }))
                {
                    DebugWarning(field, "NC4004", "Only the last key segment can be a variable length primitive type");
                    continue;
                }

                storageGroups.Add(new StorageGroup(name, valueType, prefix, keySegments));
            }
        }

        internal static CompilationContext Compile(IEnumerable<string> sourceFiles, IEnumerable<MetadataReference> references, Options options)
        {
            IEnumerable<SyntaxTree> syntaxTrees = sourceFiles.OrderBy(p => p).Select(p => CSharpSyntaxTree.ParseText(File.ReadAllText(p), path: p));
            CSharpCompilationOptions compilationOptions = new(OutputKind.DynamicallyLinkedLibrary, deterministic: true);
            CSharpCompilation compilation = CSharpCompilation.Create(null, syntaxTrees, references, compilationOptions);
            CompilationContext context = new(compilation, options);
            context.Compile();
            return context;
        }

        public static CompilationContext CompileSources(string[] sourceFiles, Options options)
        {
            List<MetadataReference> references = new(commonReferences);
            references.Add(MetadataReference.CreateFromFile(typeof(scfx.Neo.SmartContract.Framework.SmartContract).Assembly.Location));
            return Compile(sourceFiles, references, options);
        }

        public static Compilation GetCompilation(string csproj, out string assemblyName)
        {
            string folder = Path.GetDirectoryName(csproj)!;
            string obj = Path.Combine(folder, "obj");
            HashSet<string> sourceFiles = Directory.EnumerateFiles(folder, "*.cs", SearchOption.AllDirectories)
                .Where(p => !p.StartsWith(obj))
                .ToHashSet(StringComparer.OrdinalIgnoreCase);
            List<MetadataReference> references = new(commonReferences);
            CSharpCompilationOptions options = new(OutputKind.DynamicallyLinkedLibrary, deterministic: true);
            XDocument xml = XDocument.Load(csproj);
            assemblyName = xml.Root!.Elements("PropertyGroup").Elements("AssemblyName").Select(p => p.Value).SingleOrDefault() ?? Path.GetFileNameWithoutExtension(csproj);
            sourceFiles.UnionWith(xml.Root!.Elements("ItemGroup").Elements("Compile").Attributes("Include").Select(p => Path.GetFullPath(p.Value, folder)));
            Process.Start(new ProcessStartInfo
            {
                FileName = "dotnet",
                Arguments = $"restore \"{csproj}\"",
                WorkingDirectory = folder
            })!.WaitForExit();
            string assetsPath = Path.Combine(folder, "obj", "project.assets.json");
            JObject assets = JObject.Parse(File.ReadAllBytes(assetsPath));
            foreach (var (name, package) in assets["targets"][0].Properties)
            {
                MetadataReference? reference = GetReference(name, package, assets, folder, options);
                if (reference is not null) references.Add(reference);
            }
            IEnumerable<SyntaxTree> syntaxTrees = sourceFiles.OrderBy(p => p).Select(p => CSharpSyntaxTree.ParseText(File.ReadAllText(p), path: p));
            return CSharpCompilation.Create(assets["project"]["restore"]["projectName"].GetString(), syntaxTrees, references, options);
        }

        private static MetadataReference? GetReference(string name, JObject package, JObject assets, string folder, CSharpCompilationOptions options)
        {
            string assemblyName = Path.GetDirectoryName(name)!;
            if (!metaReferences.TryGetValue(assemblyName, out var reference))
            {
                switch (assets["libraries"][name]["type"].GetString())
                {
                    case "package":
                        string packagesPath = assets["project"]["restore"]["packagesPath"].GetString();
                        string namePath = assets["libraries"][name]["path"].GetString();
                        string[] files = assets["libraries"][name]["files"].GetArray()
                            .Select(p => p.GetString())
                            .Where(p => p.StartsWith("src/"))
                            .ToArray();
                        if (files.Length == 0)
                        {
                            JObject dllFiles = package["compile"] ?? package["runtime"];
                            if (dllFiles is null) return null;
                            foreach (var (file, _) in dllFiles.Properties)
                            {
                                if (file.EndsWith("_._")) continue;
                                string path = Path.Combine(packagesPath, namePath, file);
                                if (!File.Exists(path)) continue;
                                reference = MetadataReference.CreateFromFile(path);
                                break;
                            }
                            if (reference is null) return null;
                        }
                        else
                        {
                            IEnumerable<SyntaxTree> st = files.OrderBy(p => p).Select(p => Path.Combine(packagesPath, namePath, p)).Select(p => CSharpSyntaxTree.ParseText(File.ReadAllText(p), path: p));
                            CSharpCompilation cr = CSharpCompilation.Create(assemblyName, st, commonReferences, options);
                            reference = cr.ToMetadataReference();
                        }
                        break;
                    case "project":
                        string msbuildProject = assets["libraries"][name]["msbuildProject"].GetString();
                        msbuildProject = Path.GetFullPath(msbuildProject, folder);
                        reference = GetCompilation(msbuildProject, out _).ToMetadataReference();
                        break;
                    default:
                        throw new NotSupportedException();
                }
                metaReferences.Add(assemblyName, reference);
            }
            return reference;
        }

        public static CompilationContext CompileProject(string csproj, Options options)
        {
            Compilation compilation = GetCompilation(csproj, out string assemblyName);
            CompilationContext context = new(compilation, options);
            context.ContractName ??= assemblyName;
            context.Compile();
            return context;
        }

        public NefFile CreateExecutable()
        {
            Assembly assembly = Assembly.GetExecutingAssembly();
            var titleAttribute = assembly.GetCustomAttribute<AssemblyTitleAttribute>()!;
            var versionAttribute = assembly.GetCustomAttribute<AssemblyInformationalVersionAttribute>()!;
            NefFile nef = new()
            {
                Compiler = $"{titleAttribute.Title} {versionAttribute.InformationalVersion}",
                Source = Source ?? string.Empty,
                Tokens = methodTokens.ToArray(),
                Script = Script
            };
            nef.CheckSum = NefFile.ComputeChecksum(nef);
            return nef;
        }

        public string CreateAssembly()
        {
            static void WriteMethod(StringBuilder builder, MethodConvert method)
            {
                foreach (Instruction i in method.Instructions)
                {
                    builder.Append($"{i.Offset:x8}: ");
                    i.ToString(builder);
                    builder.AppendLine();
                }
                builder.AppendLine();
                builder.AppendLine();
            }
            StringBuilder builder = new();
            foreach (MethodConvert method in methodsConverted)
            {
                builder.Append("// ");
                builder.AppendLine(method.Symbol.ToString());
                builder.AppendLine();
                WriteMethod(builder, method);
            }
            foreach (MethodConvert method in methodsForward)
            {
                builder.Append("// ");
                builder.Append(method.Symbol.ToString());
                builder.AppendLine(" (Forward)");
                builder.AppendLine();
                WriteMethod(builder, method);
            }
            return builder.ToString();
        }

        public JObject CreateManifest()
        {
            return new JObject
            {
                ["name"] = ContractName,
                ["groups"] = new JArray(),
                ["features"] = new JObject(),
                ["supportedstandards"] = supportedStandards.OrderBy(p => p).Select(p => (JString)p).ToArray(),
                ["abi"] = new JObject
                {
                    ["methods"] = methodsExported.Select(p => new JObject
                    {
                        ["name"] = p.Name,
                        ["offset"] = GetAbiOffset(p.Symbol),
                        ["safe"] = p.Safe,
                        ["returntype"] = p.ReturnType,
                        ["parameters"] = p.Parameters.Select(p => p.ToJson()).ToArray()
                    }).ToArray(),
                    ["events"] = eventsExported.Select(p => new JObject
                    {
                        ["name"] = p.Name,
                        ["parameters"] = p.Parameters.Select(p => p.ToJson()).ToArray()
                    }).ToArray()
                },
                ["permissions"] = permissions.ToJson(),
                ["trusts"] = trusts.Contains("*") ? "*" : trusts.OrderBy(p => p.Length).ThenBy(p => p).Select(u => new JString(u)).ToArray(),
                ["extra"] = manifestExtra
            };
        }

        static void WriteArray(Utf8JsonWriter writer, string name, IEnumerable<string> values)
        {
            writer.WritePropertyName(name);
            writer.WriteStartArray();
            foreach (var value in values)
            {
                writer.WriteStringValue(value);
            }
            writer.WriteEndArray();
        }

        static IEnumerable<INamedTypeSymbol?> GetSymbols<T>(Compilation compilation)
            where T : TypeDeclarationSyntax
        {
            return compilation.SyntaxTrees
                .SelectMany(tree => tree.GetRoot()
                    .DescendantNodes()
                    .OfType<T>())
                .Select(c => compilation
                    .GetSemanticModel(c.SyntaxTree)
                    .GetDeclaredSymbol(c));
        }

        public void WriteDebugInfo(Utf8JsonWriter writer, SmartContract.NefFile nef)
        {
            string[] sourceLocations = GetSourceLocations(compilation).Distinct().ToArray();
            ContractTypeVisitor resolver = new(typeCache);

            writer.WriteStartObject();
            writer.WriteNumber("version", 2);
            writer.WriteString("hash", $"{nef.Script.ToScriptHash()}");
            writer.WriteNumber("checksum", nef.CheckSum);
            WriteArray(writer, "documents", sourceLocations);

            var statics = staticFields.OrderBy(kvp => kvp.Value)
                .Select(t => $"{t.Key.Name},{resolver.Resolve(t.Key.Type).AsString()},{t.Value}");
            WriteArray(writer, "static-variables", statics);

            writer.WritePropertyName("methods");
            writer.WriteStartArray();
            foreach (var m in methodsConverted.Where(p => p.SyntaxNode is not null))
            {
                writer.WriteStartObject();

                writer.WriteString("id", m.Symbol.Name);
                writer.WriteString("name", $"{m.Symbol.ContainingType},{m.Symbol.Name}");
                writer.WriteString("range", $"{m.Instructions[0].Offset}-{m.Instructions[^1].Offset}");

                writer.WritePropertyName("params");
                writer.WriteStartArray();
                if (!m.Symbol.IsStatic)
                {
                    writer.WriteStringValue($"#this,{resolver.Resolve(m.Symbol.ContainingSymbol).AsString()}");
                }
                foreach (var param in m.Symbol.Parameters)
                {
                    writer.WriteStringValue($"{param.Name},{resolver.Resolve(param.Type).AsString()}");
                }
                writer.WriteEndArray();

                var @return = resolver.Resolve(m.Symbol.ReturnType) switch
                {
                    VoidContractType => "#Void",
                    ContractType t => t.AsString(),
                };
                writer.WriteString("return", @return);

                var vars = m.Variables
                    .Select(v => $"{v.Symbol.Name},{(resolver.Resolve(v.Symbol.Type).AsString())},{v.SlotIndex}");
                WriteArray(writer, "variables", vars);

                var sequencePoints = m.Instructions
                    .Where(p => p.SourceLocation is not null)
                    .Select(p =>
                    {
                        FileLinePositionSpan span = p.SourceLocation!.GetLineSpan();
                        var index = Array.IndexOf(sourceLocations, p.SourceLocation.SourceTree!.FilePath);
                        return $"{p.Offset}[{index}]{span.StartLinePosition.Line + 1}:{span.StartLinePosition.Character + 1}-{span.EndLinePosition.Line + 1}:{span.EndLinePosition.Character + 1}";
                    });
                WriteArray(writer, "sequence-points", sequencePoints);

                writer.WriteEndObject();
            }
            writer.WriteEndArray();

            writer.WritePropertyName("events");
            writer.WriteStartArray();
            foreach (var e in eventsExported)
            {
                writer.WriteStartObject();

                writer.WriteString("id", e.Symbol.Name);
                writer.WriteString("name", $"{e.Symbol.ContainingType},{e.Symbol.Name}");

                var symbol = (IEventSymbol)e.Symbol;
                var method = ((INamedTypeSymbol)symbol.Type).DelegateInvokeMethod ?? throw new Exception();
                var @params = method.Parameters
                    .Select(p => $"{p.Name},{resolver.Resolve(p.Type).AsString()}");
                WriteArray(writer, "params", @params);

                writer.WriteEndObject();
            }
            writer.WriteEndArray();

            writer.WritePropertyName("structs");
            writer.WriteStartArray();
            foreach (var (@struct, fields) in structDefs)
            {
                writer.WriteStartObject();
                writer.WriteString("id", @struct.Name);
                writer.WriteString("name", $"{@struct.ContainingSymbol}.{@struct.Name}");
                WriteArray(writer, "fields", fields
                    .Select(f => $"{f.Name},{resolver.Resolve(f.Type).AsString()}"));
                writer.WriteEndObject();
            }
            writer.WriteEndArray();

            writer.WritePropertyName("storages");
            writer.WriteStartArray();
            foreach (var storage in storageGroups)
            {
                writer.WriteStartObject();
                writer.WriteString("name", storage.Name);
                writer.WriteString("type", resolver.Resolve(storage.ValueType).AsString());
                writer.WriteString("prefix", Convert.ToHexString(storage.Prefix.Span));
                if (storage.Segments.Any())
                { 
                    WriteArray(writer, "segments", storage.Segments.Select(s => $"{s.Name},{s.Type}"));
                }
                writer.WriteEndObject();
            }
            writer.WriteEndArray();

            writer.WriteEndObject();
        }

        public JObject CreateDebugInformation()
        {
            string[] sourceLocations = GetSourceLocations(compilation).Distinct().ToArray();
            return new JObject
            {
                ["hash"] = Script.ToScriptHash().ToString(),
                ["documents"] = sourceLocations.Select(p => (JString)p).ToArray(),
                ["static-variables"] = staticFields.OrderBy(p => p.Value).Select(p => (JString)$"{p.Key.Name},{p.Key.Type.GetContractParameterType()},{p.Value}").ToArray(),
                ["methods"] = methodsConverted.Where(p => p.SyntaxNode is not null).Select(m => new JObject
                {
                    ["id"] = m.Symbol.ToString(),
                    ["name"] = $"{m.Symbol.ContainingType},{m.Symbol.Name}",
                    ["range"] = $"{m.Instructions[0].Offset}-{m.Instructions[^1].Offset}",
                    ["params"] = (m.Symbol.IsStatic ? Array.Empty<string>() : new string[] { "this,Any" })
                        .Concat(m.Symbol.Parameters.Select(p => $"{p.Name},{p.Type.GetContractParameterType()}"))
                        .Select((p, i) => (JString)$"{p},{i}")
                        .ToArray(),
                    ["return"] = m.Symbol.ReturnType.GetContractParameterType().ToString(),
                    ["variables"] = m.Variables.Select(p => (JString)$"{p.Symbol.Name},{p.Symbol.Type.GetContractParameterType()},{p.SlotIndex}").ToArray(),
                    ["sequence-points"] = m.Instructions.Where(p => p.SourceLocation is not null).Select(p =>
                    {
                        FileLinePositionSpan span = p.SourceLocation!.GetLineSpan();
                        return (JString)$"{p.Offset}[{Array.IndexOf(sourceLocations, p.SourceLocation.SourceTree!.FilePath)}]{span.StartLinePosition.Line + 1}:{span.StartLinePosition.Character + 1}-{span.EndLinePosition.Line + 1}:{span.EndLinePosition.Character + 1}";
                    }).ToArray()
                }).ToArray(),
                ["events"] = eventsExported.Select(e => new JObject
                {
                    ["id"] = e.Name,
                    ["name"] = $"{e.Symbol.ContainingType},{e.Symbol.Name}",
                    ["params"] = e.Parameters.Select((p, i) => (JString)$"{p.Name},{p.Type},{i}").ToArray()
                }).ToArray()
            };
        }

        private static IEnumerable<string> GetSourceLocations(Compilation compilation)
        {
            foreach (SyntaxTree syntaxTree in compilation.SyntaxTrees)
                yield return syntaxTree.FilePath;
            foreach (CompilationReference reference in compilation.References.OfType<CompilationReference>())
                foreach (string path in GetSourceLocations(reference.Compilation))
                    yield return path;
        }

        private void ProcessCompilationUnit(HashSet<INamedTypeSymbol> processed, SemanticModel model, CompilationUnitSyntax syntax)
        {
            foreach (MemberDeclarationSyntax member in syntax.Members)
                ProcessMemberDeclaration(processed, model, member);
        }

        private void ProcessMemberDeclaration(HashSet<INamedTypeSymbol> processed, SemanticModel model, MemberDeclarationSyntax syntax)
        {
            switch (syntax)
            {
                case NamespaceDeclarationSyntax @namespace:
                    foreach (MemberDeclarationSyntax member in @namespace.Members)
                        ProcessMemberDeclaration(processed, model, member);
                    break;
                case ClassDeclarationSyntax @class:
                    INamedTypeSymbol symbol = model.GetDeclaredSymbol(@class)!;
                    if (processed.Add(symbol)) ProcessClass(model, symbol);
                    break;
            }
        }

        private void ProcessClass(SemanticModel model, INamedTypeSymbol symbol)
        {
            if (symbol.IsSubclassOf(nameof(Attribute))) return;
            bool isPublic = symbol.DeclaredAccessibility == Accessibility.Public;
            bool isAbstract = symbol.IsAbstract;
            bool isContractType = symbol.IsSubclassOf(nameof(scfx.Neo.SmartContract.Framework.SmartContract));
            bool isSmartContract = isPublic && !isAbstract && isContractType;
            if (isSmartContract)
            {
                if (smartContractSymbol is not null) throw new CompilationException(DiagnosticId.MultiplyContracts, $"Only one smart contract is allowed.");
                smartContractSymbol = symbol;
                foreach (var attribute in symbol.GetAttributesWithInherited())
                {
                    switch (attribute.AttributeClass!.Name)
                    {
                        case nameof(DisplayNameAttribute):
                            ContractName = (string)attribute.ConstructorArguments[0].Value!;
                            break;
                        case nameof(scfx.Neo.SmartContract.Framework.Attributes.ContractSourceCodeAttribute):
                            Source = (string)attribute.ConstructorArguments[0].Value!;
                            break;
                        case nameof(scfx.Neo.SmartContract.Framework.Attributes.ManifestExtraAttribute):
                            manifestExtra[(string)attribute.ConstructorArguments[0].Value!] = (string)attribute.ConstructorArguments[1].Value!;
                            break;
                        case nameof(scfx.Neo.SmartContract.Framework.Attributes.ContractPermissionAttribute):
                            permissions.Add((string)attribute.ConstructorArguments[0].Value!, attribute.ConstructorArguments[1].Values.Select(p => (string)p.Value!).ToArray());
                            break;
                        case nameof(scfx.Neo.SmartContract.Framework.Attributes.ContractTrustAttribute):
                            string trust = (string)attribute.ConstructorArguments[0].Value!;
                            if (!ValidateContractTrust(trust))
                                throw new ArgumentException($"The value {trust} is not a valid one for ContractTrust");
                            trusts.Add(trust);
                            break;
                        case nameof(scfx.Neo.SmartContract.Framework.Attributes.SupportedStandardsAttribute):
                            supportedStandards.UnionWith(attribute.ConstructorArguments[0].Values.Select(p => (string)p.Value!));
                            break;
                    }
                }
                ContractName ??= symbol.Name;
            }
            foreach (ISymbol member in symbol.GetAllMembers())
            {
                switch (member)
                {
                    case IEventSymbol @event when isSmartContract:
                        ProcessEvent(@event);
                        break;
                    case IMethodSymbol method when method.Name != "_initialize" && method.MethodKind != MethodKind.StaticConstructor:
                        ProcessMethod(model, method, isSmartContract);
                        break;
                }
            }
            if (isSmartContract)
            {
                IMethodSymbol _initialize = symbol.StaticConstructors.Length == 0
                    ? symbol.GetAllMembers().OfType<IMethodSymbol>().First(p => p.Name == "_initialize")
                    : symbol.StaticConstructors[0];
                ProcessMethod(model, _initialize, true);
            }
        }

        private void ProcessEvent(IEventSymbol symbol)
        {
            if (symbol.DeclaredAccessibility != Accessibility.Public) return;
            INamedTypeSymbol type = (INamedTypeSymbol)symbol.Type;
            if (!type.DelegateInvokeMethod!.ReturnsVoid)
                throw new CompilationException(symbol, DiagnosticId.EventReturns, $"Event return value is not supported.");
            AbiEvent ev = new(symbol);
            if (eventsExported.Any(u => u.Name == ev.Name))
                throw new CompilationException(symbol, DiagnosticId.EventNameConflict, $"Duplicate event name: {ev.Name}.");
            eventsExported.Add(ev);
        }

        private void ProcessMethod(SemanticModel model, IMethodSymbol symbol, bool export)
        {
            if (symbol.IsAbstract) return;
            if (symbol.MethodKind != MethodKind.StaticConstructor)
            {
                if (symbol.DeclaredAccessibility != Accessibility.Public)
                    export = false;
                if (symbol.MethodKind != MethodKind.Ordinary && symbol.MethodKind != MethodKind.PropertyGet && symbol.MethodKind != MethodKind.PropertySet)
                    return;
            }
            if (export)
            {
                AbiMethod method = new(symbol);
                if (methodsExported.Any(u => u.Name == method.Name && u.Parameters.Length == method.Parameters.Length))
                    throw new CompilationException(symbol, DiagnosticId.MethodNameConflict, $"Duplicate method key: {method.Name},{method.Parameters.Length}.");
                methodsExported.Add(method);
            }
            MethodConvert convert = ConvertMethod(model, symbol);
            if (export && !symbol.IsStatic)
            {
                MethodConvert forward = new(this, symbol);
                forward.ConvertForward(model, convert);
                methodsForward.Add(forward);
            }
        }

        internal MethodConvert ConvertMethod(SemanticModel model, IMethodSymbol symbol)
        {
            if (!methodsConverted.TryGetValue(symbol, out MethodConvert? method))
            {
                method = new MethodConvert(this, symbol);
                methodsConverted.Add(method);
                if (!symbol.DeclaringSyntaxReferences.IsEmpty)
                {
                    ISourceAssemblySymbol assembly = (ISourceAssemblySymbol)symbol.ContainingAssembly;
                    model = assembly.Compilation.GetSemanticModel(symbol.DeclaringSyntaxReferences[0].SyntaxTree);
                }
                method.Convert(model);
            }
            return method;
        }

        internal ushort AddMethodToken(UInt160 hash, string method, ushort parametersCount, bool hasReturnValue, CallFlags callFlags)
        {
            int index = methodTokens.FindIndex(p => p.Hash == hash && p.Method == method && p.ParametersCount == parametersCount && p.HasReturnValue == hasReturnValue && p.CallFlags == callFlags);
            if (index >= 0) return (ushort)index;
            methodTokens.Add(new MethodToken
            {
                Hash = hash,
                Method = method,
                ParametersCount = parametersCount,
                HasReturnValue = hasReturnValue,
                CallFlags = callFlags
            });
            permissions.Add(hash.ToString(), method);
            return (ushort)(methodTokens.Count - 1);
        }

        internal byte AddStaticField(IFieldSymbol symbol)
        {
            if (!staticFields.TryGetValue(symbol, out byte index))
            {
                index = (byte)StaticFieldCount;
                staticFields.Add(symbol, index);
            }
            return index;
        }

        internal byte AddVTable(ITypeSymbol type)
        {
            if (!vtables.TryGetValue(type, out byte index))
            {
                index = (byte)StaticFieldCount;
                vtables.Add(type, index);
            }
            return index;
        }
    }
}
